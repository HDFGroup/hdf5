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
VALUE_RELEASE_NICE_TO_HAVE = "Release_Nice to Have"

# Expected value for Status field when an item is completed
VALUE_STATUS_DONE = "Done"

# Milestone filtering
# Set to None to include all milestones, or specify a version like "2.1" to filter
DEFAULT_MILESTONE_FILTER = None  # Will be set from environment or H5public.h


class GitHubProjectTracker:
    """Tracks release blocker progress in GitHub projects."""

    def __init__(self, token: str, owner: str, project_number: int, milestone_filter: Optional[str] = None):
        self.api_url = "https://api.github.com/graphql"
        self.headers = {
            "Authorization": f"bearer {token}",
            "Content-Type": "application/json"
        }
        self.owner = owner
        self.project_number = project_number
        self.milestone_filter = milestone_filter
        
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
                  content {
                    ... on Issue {
                      id, title, url
                      milestone { title }
                    }
                  }
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
        Fetches release blocker, must-do, and nice-to-have statistics from the GitHub project.

        Returns:
            Dict with 'total', 'done', 'percentage', 'blocker_total', 'blocker_done',
            'mustdo_total', 'mustdo_done', 'nicetohave_total', 'nicetohave_done' keys
        """
        blocker_total = 0
        blocker_done = 0
        mustdo_total = 0
        mustdo_done = 0
        nicetohave_total = 0
        nicetohave_done = 0
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

                # Check milestone filter if configured
                content = item.get("content", {})
                milestone = content.get("milestone", {})
                milestone_title = milestone.get("title", "") if milestone else ""

                # Skip if milestone filter is set and doesn't match
                if self.milestone_filter:
                    # Match milestone versions like "2.1" with milestones like "2.1.0" or "HDF5 2.1"
                    if not (milestone_title and self.milestone_filter in milestone_title):
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
                elif release_gating == VALUE_RELEASE_NICE_TO_HAVE:
                    nicetohave_total += 1
                    if status == VALUE_STATUS_DONE:
                        nicetohave_done += 1

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
            if self.milestone_filter:
                print(f"INFO: No release blocker or must-do items found for milestone '{self.milestone_filter}'.", file=sys.stderr)
                print("This may be expected if no items exist for this milestone yet.", file=sys.stderr)
                # Don't fail - return N/A indicators when filtering by milestone with no items
                percentage = -1.0  # Use -1 to indicate N/A
            else:
                print("ERROR: No release blocker or must-do items found (total=0).", file=sys.stderr)
                print("This likely indicates:", file=sys.stderr)
                print(f"  1. The '{FIELD_RELEASE_GATING}' field values changed", file=sys.stderr)
                print(f"     Expected values: '{VALUE_RELEASE_BLOCKER}' or '{VALUE_RELEASE_MUST_DO}'", file=sys.stderr)
                print("  2. Project has no items with these field values", file=sys.stderr)
                print("  3. Field matching logic needs to be updated", file=sys.stderr)
                print("Refusing to report 0% or 100% with no items to prevent false positives.", file=sys.stderr)
                raise ProjectDataError("No release items found - refusing to report false completion status")
        else:
            percentage = round((done / total * 100), 1)

        return {
            'total': total,
            'done': done,
            'percentage': percentage,
            'blocker_total': blocker_total,
            'blocker_done': blocker_done,
            'mustdo_total': mustdo_total,
            'mustdo_done': mustdo_done,
            'nicetohave_total': nicetohave_total,
            'nicetohave_done': nicetohave_done
        }


def get_hdf5_version_from_header(header_path: str = "../../src/H5public.h") -> Optional[str]:
    """
    Extract HDF5 major.minor version from H5public.h
    Returns version string like "2.1" or None if not found.
    """
    try:
        # Try multiple possible paths relative to the script location
        script_dir = os.path.dirname(os.path.abspath(__file__))
        possible_paths = [
            os.path.join(script_dir, header_path),
            os.path.join(script_dir, "../../src/H5public.h"),
            "src/H5public.h",
            "../../src/H5public.h"
        ]

        for path in possible_paths:
            if os.path.exists(path):
                with open(path, 'r') as f:
                    major = None
                    minor = None
                    for line in f:
                        if '#define H5_VERS_MAJOR' in line:
                            major = line.split()[-1]
                        elif '#define H5_VERS_MINOR' in line:
                            minor = line.split()[-1]
                        if major and minor:
                            return f"{major}.{minor}"
                break
    except Exception as e:
        print(f"Warning: Could not read version from H5public.h: {e}", file=sys.stderr)

    return None


def main():
    """Main function to run the tracker."""
    # Configuration - can be overridden by environment variables
    TOKEN = os.getenv("GITHUB_TOKEN")
    OWNER = os.getenv("GITHUB_OWNER", "HDFGroup")
    PROJECT_NUMBER = int(os.getenv("GITHUB_PROJECT_NUMBER", "39"))

    # Milestone filtering - can be set via env var or auto-detected from H5public.h
    MILESTONE_FILTER = os.getenv("MILESTONE_FILTER")
    if MILESTONE_FILTER is None:
        # Try to auto-detect from H5public.h
        MILESTONE_FILTER = get_hdf5_version_from_header()
        if MILESTONE_FILTER:
            print(f"Auto-detected milestone filter from H5public.h: {MILESTONE_FILTER}", file=sys.stderr)

    if MILESTONE_FILTER:
        print(f"Filtering by milestone: {MILESTONE_FILTER}", file=sys.stderr)
    else:
        print("No milestone filter - counting all release items", file=sys.stderr)

    try:
        tracker = GitHubProjectTracker(TOKEN, OWNER, PROJECT_NUMBER, MILESTONE_FILTER)
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
                f.write(f"nicetohave_total={stats['nicetohave_total']}\n")
                f.write(f"nicetohave_done={stats['nicetohave_done']}\n")
                f.write(f"version={MILESTONE_FILTER or 'all'}\n")

        # Also output to stdout for local testing
        print(f"percentage={stats['percentage']}")
        print(f"blocker_done={stats['blocker_done']}")
        print(f"blocker_total={stats['blocker_total']}")
        print(f"mustdo_done={stats['mustdo_done']}")
        print(f"mustdo_total={stats['mustdo_total']}")
        print(f"nicetohave_done={stats['nicetohave_done']}")
        print(f"nicetohave_total={stats['nicetohave_total']}")
        print(f"version={MILESTONE_FILTER or 'all'}")
        print(f"Calculated progress: {stats['percentage']}%")
        print(f"Done / Total: {stats['done']} / {stats['total']}")
        print(f"Blockers: {stats['blocker_done']} / {stats['blocker_total']}")
        print(f"Must Do: {stats['mustdo_done']} / {stats['mustdo_total']}")
        print(f"Nice to Have: {stats['nicetohave_done']} / {stats['nicetohave_total']}")
        if MILESTONE_FILTER:
            print(f"Milestone filter: {MILESTONE_FILTER}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())
