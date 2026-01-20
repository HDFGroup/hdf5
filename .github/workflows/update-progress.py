#!/usr/bin/env python3
"""
GitHub Project Release Blocker Progress Tracker
Fetches release blocker issues from the HDF5 project and calculates completion percentage.
"""

import os
import sys

import requests
from typing import Dict, Any, Optional


class ProjectFieldMissingError(Exception):
    """Raised when a critical project field is missing or renamed."""
    pass


class ProjectDataError(Exception):
    """Raised when project data is invalid or incomplete."""
    pass

# Configuration: Expected field names in GitHub Project
# Update these if the project field names change
FIELD_RELEASE_GATING = "Release gating"
FIELD_STATUS = "Status"

# Expected values for Release gating field
VALUE_RELEASE_BLOCKER = "Release_Blocker"
VALUE_RELEASE_MUST_DO = "Release_Must Do"

# Expected value for Status field when an item is completed
VALUE_STATUS_DONE = "Done"


class GitHubProjectTracker:
    """Tracks release blocker progress in GitHub projects."""

    def __init__(self, token: str, owner: str, project_number: int):
        self.api_url = "https://api.github.com/graphql"
        self.headers = {
            "Authorization": f"bearer {token}",
            "Content-Type": "application/json"
        }
        self.owner = owner
        self.project_number = project_number
        
    def _get_query(self) -> str:
        """Returns the GraphQL query for fetching project items."""
        return """
        query($owner: String!, $projectNumber: Int!, $cursor: String) {
          organization(login: $owner) {
            projectV2(number: $projectNumber) {
              items(first: 100, after: $cursor) {
                pageInfo { hasNextPage, endCursor }
                nodes {
                  id
                  fieldValues(first: 20) {
                    nodes {
                      __typename
                      ... on ProjectV2ItemFieldTextValue { 
                        text, field { ... on ProjectV2Field { name } } 
                      }
                      ... on ProjectV2ItemFieldSingleSelectValue { 
                        name, field { ... on ProjectV2SingleSelectField { name } } 
                      }
                      ... on ProjectV2ItemFieldIterationValue { 
                        title, field { ... on ProjectV2IterationField { name } } 
                      }
                      ... on ProjectV2ItemFieldNumberValue { 
                        number, field { ... on ProjectV2Field { name } } 
                      }
                      ... on ProjectV2ItemFieldDateValue { 
                        date, field { ... on ProjectV2Field { name } } 
                      }
                    }
                  }
                  content { ... on Issue { id, title, url } }
                }
              }
            }
          }
        }
        """
    
    def _extract_field_value(self, field_data: Dict[str, Any]) -> Optional[str]:
        """Extracts value from a field based on its type."""
        type_name = field_data.get("__typename")
        
        value_map = {
            "ProjectV2ItemFieldSingleSelectValue": "name",
            "ProjectV2ItemFieldIterationValue": "title", 
            "ProjectV2ItemFieldTextValue": "text",
            "ProjectV2ItemFieldNumberValue": "number",
            "ProjectV2ItemFieldDateValue": "date"
        }
        
        value_key = value_map.get(type_name)
        return field_data.get(value_key) if value_key else None
    
    def _parse_item_fields(self, item: Dict[str, Any]) -> Dict[str, str]:
        """Parses field values from a project item."""
        fields = {}
        
        for field_data in item.get("fieldValues", {}).get("nodes", []):
            field_name = field_data.get("field", {}).get("name")
            if not field_name:
                continue
                
            value = self._extract_field_value(field_data)
            if value is not None:
                fields[field_name] = str(value)
                
        return fields
    
    def fetch_release_blocker_stats(self) -> Dict[str, int]:
        """
        Fetches release blocker and must-do statistics from the GitHub project.

        Returns:
            Dict with 'total', 'done', 'percentage', 'blocker_total', 'blocker_done',
            'mustdo_total', 'mustdo_done' keys
        """
        blocker_total = 0
        blocker_done = 0
        mustdo_total = 0
        mustdo_done = 0
        cursor = None

        # Track if we've seen the expected fields at least once
        seen_release_gating = False
        seen_status = False

        while True:
            variables = {
                "owner": self.owner,
                "projectNumber": self.project_number,
                "cursor": cursor
            }

            try:
                response = requests.post(
                    self.api_url,
                    json={'query': self._get_query(), 'variables': variables},
                    headers=self.headers,
                    timeout=30
                )
                response.raise_for_status()
                result = response.json()

                if "errors" in result:
                    raise Exception(f"GraphQL errors: {result['errors']}")

            except requests.RequestException as e:
                raise Exception(f"API request failed: {e}")

            # Parse response
            project = result.get("data", {}).get("organization", {}).get("projectV2", {})
            items = project.get("items", {})

            for item in items.get("nodes", []):
                if not item.get("content"):
                    continue

                fields = self._parse_item_fields(item)

                # Validate expected fields exist
                if FIELD_RELEASE_GATING in fields:
                    seen_release_gating = True
                if FIELD_STATUS in fields:
                    seen_status = True

                release_gating = fields.get(FIELD_RELEASE_GATING, "")
                status = fields.get(FIELD_STATUS, "")

                if release_gating == VALUE_RELEASE_BLOCKER:
                    blocker_total += 1
                    if status == VALUE_STATUS_DONE:
                        blocker_done += 1
                elif release_gating == VALUE_RELEASE_MUST_DO:
                    mustdo_total += 1
                    if status == VALUE_STATUS_DONE:
                        mustdo_done += 1

            # Check for next page
            page_info = items.get("pageInfo", {})
            if not page_info.get("hasNextPage", False):
                break
            cursor = page_info.get("endCursor")

        # Validate that expected fields were found - FAIL HARD if missing
        # This prevents false positives where field renames would cause 0 blockers to be reported
        if not seen_release_gating:
            print(f"ERROR: Critical field '{FIELD_RELEASE_GATING}' not found in any project items.",
                  file=sys.stderr)
            print("This field is required to identify release blockers and must-do items.",
                  file=sys.stderr)
            print("Possible causes:", file=sys.stderr)
            print(f"  1. Field '{FIELD_RELEASE_GATING}' was renamed in the project", file=sys.stderr)
            print("  2. Project structure changed", file=sys.stderr)
            print("  3. Project is empty or inaccessible", file=sys.stderr)
            print("Action required: Update FIELD_RELEASE_GATING constant in this script.", file=sys.stderr)
            raise ProjectFieldMissingError(f"Critical field '{FIELD_RELEASE_GATING}' not found")

        if not seen_status:
            print(f"ERROR: Critical field '{FIELD_STATUS}' not found in any project items.",
                  file=sys.stderr)
            print("This field is required to determine completion status.", file=sys.stderr)
            print("Possible causes:", file=sys.stderr)
            print(f"  1. Field '{FIELD_STATUS}' was renamed in the project", file=sys.stderr)
            print("  2. Project structure changed", file=sys.stderr)
            print("  3. Project is empty or inaccessible", file=sys.stderr)
            print("Action required: Update FIELD_STATUS constant in this script.", file=sys.stderr)
            raise ProjectFieldMissingError(f"Critical field '{FIELD_STATUS}' not found")

        total = blocker_total + mustdo_total
        done = blocker_done + mustdo_done

        # Validate that we found at least some items
        # If total is 0, either the project is empty or field matching failed
        if total == 0:
            print("ERROR: No release blocker or must-do items found (total=0).", file=sys.stderr)
            print("This likely indicates:", file=sys.stderr)
            print(f"  1. The '{FIELD_RELEASE_GATING}' field values changed", file=sys.stderr)
            print(f"     Expected values: '{VALUE_RELEASE_BLOCKER}' or '{VALUE_RELEASE_MUST_DO}'", file=sys.stderr)
            print("  2. Project has no items with these field values", file=sys.stderr)
            print("  3. Field matching logic needs to be updated", file=sys.stderr)
            print("Refusing to report 0% or 100% with no items to prevent false positives.", file=sys.stderr)
            raise ProjectDataError("No release items found - refusing to report false completion status")

        percentage = round((done / total * 100), 1)

        return {
            'total': total,
            'done': done,
            'percentage': percentage,
            'blocker_total': blocker_total,
            'blocker_done': blocker_done,
            'mustdo_total': mustdo_total,
            'mustdo_done': mustdo_done
        }


def main():
    """Main function to run the tracker."""
    # Configuration - can be overridden by environment variables
    TOKEN = os.getenv("GITHUB_TOKEN")
    OWNER = os.getenv("GITHUB_OWNER", "HDFGroup")
    PROJECT_NUMBER = int(os.getenv("GITHUB_PROJECT_NUMBER", "39"))
    
    try:
        tracker = GitHubProjectTracker(TOKEN, OWNER, PROJECT_NUMBER)
        stats = tracker.fetch_release_blocker_stats()
        
        # Output for GitHub Actions
        github_output = os.getenv("GITHUB_OUTPUT")
        if github_output:
            with open(github_output, "a") as f:
                f.write(f"percentage={stats['percentage']}\n")
                f.write(f"done={stats['done']}\n")
                f.write(f"total={stats['total']}\n")
                f.write(f"blocker_total={stats['blocker_total']}\n")
                f.write(f"blocker_done={stats['blocker_done']}\n")
                f.write(f"mustdo_total={stats['mustdo_total']}\n")
                f.write(f"mustdo_done={stats['mustdo_done']}\n")

        # Also output to stdout for local testing
        print(f"percentage={stats['percentage']}")
        print(f"Calculated progress: {stats['percentage']}%")
        print(f"Done / Total: {stats['done']} / {stats['total']}")
        print(f"Blockers: {stats['blocker_done']} / {stats['blocker_total']}")
        print(f"Must Do: {stats['mustdo_done']} / {stats['mustdo_total']}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())
