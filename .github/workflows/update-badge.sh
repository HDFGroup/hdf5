#!/bin/bash
#
# Badge Generation and Gist Update Script
# Generates badges for critical and high priority issue progress and updates GitHub Gist
#
# Environment Variables Required:
#   GITHUB_TOKEN - GitHub token for Gist API access
#   GIST_ID - ID of the Gist to update
#   GITHUB_REPOSITORY - Full repository name (owner/repo)
#   GITHUB_REPOSITORY_OWNER - Repository owner
#   PERCENTAGE - Overall completion percentage
#   DONE - Number of completed items
#   TOTAL - Total number of items
#   BLOCKER_DONE - Number of completed critical priority items
#   BLOCKER_TOTAL - Total number of critical priority items
#   MUSTDO_DONE - Number of completed high priority items
#   MUSTDO_TOTAL - Total number of high priority items
#   VERSION - Version string (e.g., "2.1") - optional
#

set -euo pipefail

# Validate required environment variables
required_vars=(
  "GITHUB_TOKEN"
  "GIST_ID"
  "GITHUB_REPOSITORY"
  "GITHUB_REPOSITORY_OWNER"
  "PERCENTAGE"
  "DONE"
  "TOTAL"
  "BLOCKER_DONE"
  "BLOCKER_TOTAL"
  "MUSTDO_DONE"
  "MUSTDO_TOTAL"
)

for var in "${required_vars[@]}"; do
  if [ -z "${!var:-}" ]; then
    echo "::error::Required environment variable $var is not set"
    exit 1
  fi
done

echo "::notice::Updating badge with: ${PERCENTAGE}% (${DONE}/${TOTAL})"

# Determine badge color and status based on percentage
PERCENTAGE_INT="${PERCENTAGE%.*}"
if [ "$PERCENTAGE_INT" -ge 90 ]; then
  COLOR="brightgreen"
  STATUS="🟢 Readying for Deployment"
elif [ "$PERCENTAGE_INT" -ge 60 ]; then
  COLOR="yellow"
  STATUS="🟡 Nearing Completion"
elif [ "$PERCENTAGE_INT" -ge 40 ]; then
  COLOR="orange"
  STATUS="🟠 In Development"
else
  COLOR="red"
  STATUS="🔴 Initial Phase"
fi

echo "::notice title=Release Progress::${PERCENTAGE}% Complete (${DONE}/${TOTAL}) - ${STATUS}"

# Function to determine badge color based on percentage
get_badge_color() {
  local percentage_int="${1%.*}"
  if [ "$percentage_int" -ge 90 ]; then
    echo "brightgreen"
  elif [ "$percentage_int" -ge 60 ]; then
    echo "yellow"
  elif [ "$percentage_int" -ge 40 ]; then
    echo "orange"
  else
    echo "red"
  fi
}

# Function to create badge JSON
create_badge_json() {
  local label="$1"
  local done="$2"
  local total="$3"
  local percentage="$4"
  local color="$5"

  # Handle 0/0 case (when percentage is -1 or total is 0)
  local message
  if [ "$total" -eq 0 ] || [ "$percentage" = "-1.0" ]; then
    message="0/0"
  else
    message="$done/$total ($percentage%)"
  fi

  jq -n \
    --arg label "$label" \
    --arg message "$message" \
    --arg color "$color" \
    '{
      "schemaVersion": 1,
      "label": $label,
      "message": $message,
      "color": $color,
      "style": "flat-square"
    }'
}

# Calculate percentages for each category
if [ "$BLOCKER_TOTAL" -eq 0 ]; then
  BLOCKER_PERCENTAGE="-1.0"
else
  BLOCKER_PERCENTAGE=$(awk "BEGIN {printf \"%.1f\", ($BLOCKER_DONE / $BLOCKER_TOTAL * 100)}")
fi

if [ "$MUSTDO_TOTAL" -eq 0 ]; then
  MUSTDO_PERCENTAGE="-1.0"
else
  MUSTDO_PERCENTAGE=$(awk "BEGIN {printf \"%.1f\", ($MUSTDO_DONE / $MUSTDO_TOTAL * 100)}")
fi

# Determine colors using the shared function (use lightgrey for 0/0)
if [ "$BLOCKER_PERCENTAGE" = "-1.0" ]; then
  BLOCKER_COLOR="lightgrey"
else
  BLOCKER_COLOR=$(get_badge_color "$BLOCKER_PERCENTAGE")
fi

if [ "$MUSTDO_PERCENTAGE" = "-1.0" ]; then
  MUSTDO_COLOR="lightgrey"
else
  MUSTDO_COLOR=$(get_badge_color "$MUSTDO_PERCENTAGE")
fi

# Determine badge labels - include version if available
if [ -n "${VERSION:-}" ] && [ "$VERSION" != "all" ]; then
  BLOCKER_LABEL="${VERSION} Critical Priority"
  MUSTDO_LABEL="${VERSION} High Priority"
else
  BLOCKER_LABEL="Critical Priority"
  MUSTDO_LABEL="High Priority"
fi

# Create badge JSONs using the shared function
BLOCKER_BADGE_JSON=$(create_badge_json "$BLOCKER_LABEL" "$BLOCKER_DONE" "$BLOCKER_TOTAL" "$BLOCKER_PERCENTAGE" "$BLOCKER_COLOR")
MUSTDO_BADGE_JSON=$(create_badge_json "$MUSTDO_LABEL" "$MUSTDO_DONE" "$MUSTDO_TOTAL" "$MUSTDO_PERCENTAGE" "$MUSTDO_COLOR")

# Validate JSONs were created successfully
if [ -z "$BLOCKER_BADGE_JSON" ] || ! echo "$BLOCKER_BADGE_JSON" | jq empty 2>/dev/null; then
  echo "::error::Failed to generate valid critical priority badge JSON"
  exit 1
fi
if [ -z "$MUSTDO_BADGE_JSON" ] || ! echo "$MUSTDO_BADGE_JSON" | jq empty 2>/dev/null; then
  echo "::error::Failed to generate valid high priority badge JSON"
  exit 1
fi

# The filenames in the Gist
BLOCKER_GIST_NAME="release-blocker-${GITHUB_REPOSITORY##*/}.json"
MUSTDO_GIST_NAME="release-mustdo-${GITHUB_REPOSITORY##*/}.json"
echo "::notice::Updating Gist files: $BLOCKER_GIST_NAME, $MUSTDO_GIST_NAME"

# Create the request payload with both files
REQUEST_PAYLOAD=$(jq -n \
  --arg blocker_filename "$BLOCKER_GIST_NAME" \
  --arg mustdo_filename "$MUSTDO_GIST_NAME" \
  --argjson blocker_content "$BLOCKER_BADGE_JSON" \
  --argjson mustdo_content "$MUSTDO_BADGE_JSON" \
  '{
    "files": {
      ($blocker_filename): {
        "content": ($blocker_content | tostring)
      },
      ($mustdo_filename): {
        "content": ($mustdo_content | tostring)
      }
    }
  }')

# Update the existing Gist with response validation
echo "Updating Gist: ${GIST_ID}"
RESPONSE=$(curl -s -w "\n%{http_code}" -L -X PATCH \
  -H "Authorization: token ${GITHUB_TOKEN}" \
  -H "Accept: application/vnd.github.v3+json" \
  "https://api.github.com/gists/${GIST_ID}" \
  -d "$REQUEST_PAYLOAD")

# Extract HTTP status code (last line) and response body
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
RESPONSE_BODY=$(echo "$RESPONSE" | head -n -1)

# Validate API response
if [ "$HTTP_CODE" != "200" ]; then
  echo "::error::Gist update failed with HTTP status $HTTP_CODE"
  echo "Response body: $RESPONSE_BODY"
  exit 1
fi

echo "::notice::Gist updated successfully"

# Generate badge URLs for use in README
BLOCKER_BADGE_URL="https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/${GITHUB_REPOSITORY_OWNER}-Bot/${GIST_ID}/raw/${BLOCKER_GIST_NAME}"
MUSTDO_BADGE_URL="https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/${GITHUB_REPOSITORY_OWNER}-Bot/${GIST_ID}/raw/${MUSTDO_GIST_NAME}"
PROJECT_URL="https://github.com/${GITHUB_REPOSITORY}/projects/39"

echo "::notice::Critical Priority Badge URL: $BLOCKER_BADGE_URL"
echo "::notice::High Priority Badge URL: $MUSTDO_BADGE_URL"

# Output to GitHub Actions if GITHUB_OUTPUT is set
if [ -n "${GITHUB_OUTPUT:-}" ]; then
  echo "blocker_badge_url=$BLOCKER_BADGE_URL" >> "$GITHUB_OUTPUT"
  echo "mustdo_badge_url=$MUSTDO_BADGE_URL" >> "$GITHUB_OUTPUT"
  echo "project_url=$PROJECT_URL" >> "$GITHUB_OUTPUT"
fi

# Create enhanced step summary if GITHUB_STEP_SUMMARY is set
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  cat >> "$GITHUB_STEP_SUMMARY" << EOF
## 📊 Release Progress: ${PERCENTAGE}%

**Status:** ${STATUS}
**Overall Progress:** ${DONE} of ${TOTAL} items completed

### Critical Priority
**Progress:** ${BLOCKER_DONE} of ${BLOCKER_TOTAL} completed (${BLOCKER_PERCENTAGE}%)
**Badge Color:** ${BLOCKER_COLOR}

### High Priority
**Progress:** ${MUSTDO_DONE} of ${MUSTDO_TOTAL} completed (${MUSTDO_PERCENTAGE}%)
**Badge Color:** ${MUSTDO_COLOR}

**Gist ID:** ${GIST_ID}

### Badge URLs
**Critical Priority Markdown:** \`[![Critical Priority Progress](${BLOCKER_BADGE_URL})](${PROJECT_URL})\`
**High Priority Markdown:** \`[![High Priority Progress](${MUSTDO_BADGE_URL})](${PROJECT_URL})\`

### Badge JSON Preview
\`\`\`json
// Critical Priority Badge
${BLOCKER_BADGE_JSON}

// High Priority Badge
${MUSTDO_BADGE_JSON}
\`\`\`
EOF
fi

echo "::notice::Badge update completed successfully"
