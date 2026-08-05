#!/usr/bin/env python3
"""
GitHub Project Priority Issue Progress Tracker
Fetches critical and high priority issues from the HDF5 project and calculates completion percentage.
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
FIELD_PRIORITY = "Priority"
FIELD_STATUS = "Status"

# Expected values for Priority field
VALUE_CRITICAL = "P0 - Critical"
VALUE_HIGH = "P1 - High"
VALUE_MEDIUM = "P2 - Medium"
VALUE_LOW = "P3 - Low"

# Expected value for Status field when an item is completed
VALUE_STATUS_DONE = "Done"

# Milestone filtering
# Set to None to include all milestones, or specify a version like "2.1" to filter
DEFAULT_MILESTONE_FILTER = None  # Will be set from environment or H5public.h


class GitHubProjectTracker:
    """Tracks priority issue progress in GitHub projects."""

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
                    ... on PullRequest {
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
        Fetches critical, high, medium, and low priority issue statistics from the GitHub project.

        Returns:
            Dict with 'total', 'done', 'percentage', 'blocker_total', 'blocker_done',
            'mustdo_total', 'mustdo_done', 'medium_total', 'medium_done',
            'low_total', 'low_done' keys
        """
        blocker_total = 0
        blocker_done = 0
        mustdo_total = 0
        mustdo_done = 0
        medium_total = 0
        medium_done = 0
        low_total = 0
        low_done = 0
        cursor = None

        # Track if we've seen the expected fields at least once
        seen_priority = False
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
                if FIELD_PRIORITY in fields:
                    seen_priority = True
                if FIELD_STATUS in fields:
                    seen_status = True

                priority = fields.get(FIELD_PRIORITY, "")
                status = fields.get(FIELD_STATUS, "")

                if priority == VALUE_CRITICAL:
                    blocker_total += 1
                    if status == VALUE_STATUS_DONE:
                        blocker_done += 1
                elif priority == VALUE_HIGH:
                    mustdo_total += 1
                    if status == VALUE_STATUS_DONE:
                        mustdo_done += 1
                elif priority == VALUE_MEDIUM:
                    medium_total += 1
                    if status == VALUE_STATUS_DONE:
                        medium_done += 1
                elif priority == VALUE_LOW:
                    low_total += 1
                    if status == VALUE_STATUS_DONE:
                        low_done += 1

            # Check for next page
            page_info = items.get("pageInfo", {})
            if not page_info.get("hasNextPage", False):
                break
            cursor = page_info.get("endCursor")

        # Validate that expected fields were found - FAIL HARD if missing
        # This prevents false positives where field renames would cause 0 items to be reported
        if not seen_priority:
            print(f"ERROR: Critical field '{FIELD_PRIORITY}' not found in any project items.",
                  file=sys.stderr)
            print("This field is required to identify priority items.",
                  file=sys.stderr)
            print("Possible causes:", file=sys.stderr)
            print(f"  1. Field '{FIELD_PRIORITY}' was renamed in the project", file=sys.stderr)
            print("  2. Project structure changed", file=sys.stderr)
            print("  3. Project is empty or inaccessible", file=sys.stderr)
            print("Action required: Update FIELD_PRIORITY constant in this script.", file=sys.stderr)
            raise ProjectFieldMissingError(f"Critical field '{FIELD_PRIORITY}' not found")

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

        total = blocker_total + mustdo_total + medium_total + low_total
        done = blocker_done + mustdo_done + medium_done + low_done

        # Validate that we found at least some items
        # If total is 0, either the project is empty or field matching failed
        if total == 0:
            if self.milestone_filter:
                print(f"INFO: No priority items found for milestone '{self.milestone_filter}'.", file=sys.stderr)
                print("This may be expected if no items exist for this milestone yet.", file=sys.stderr)
                # Don't fail - return N/A indicators when filtering by milestone with no items
                percentage = -1.0  # Use -1 to indicate N/A
            else:
                print("ERROR: No priority items found (total=0).", file=sys.stderr)
                print("This likely indicates:", file=sys.stderr)
                print(f"  1. The '{FIELD_PRIORITY}' field values changed", file=sys.stderr)
                print(f"     Expected values: '{VALUE_CRITICAL}', '{VALUE_HIGH}', '{VALUE_MEDIUM}', or '{VALUE_LOW}'", file=sys.stderr)
                print("  2. Project has no items with these field values", file=sys.stderr)
                print("  3. Field matching logic needs to be updated", file=sys.stderr)
                print("Refusing to report 0% or 100% with no items to prevent false positives.", file=sys.stderr)
                raise ProjectDataError("No priority items found - refusing to report false completion status")
        else:
            percentage = round((done / total * 100), 1)

        return {
            'total': total,
            'done': done,
            'percentage': percentage,
            'blocker_total': blocker_total,
            'blocker_done': blocker_done,
            'mustdo_total': mustdo_total,
            'medium_total': medium_total,
            'medium_done': medium_done,
            'low_total': low_total,
            'low_done': low_done,
            'mustdo_done': mustdo_done
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


def get_latest_series_release(token: str, repo: str, major_version: str) -> Optional[Dict[str, str]]:
    """
    Finds the most recent published (non-draft, non-prerelease) GitHub Release
    whose tag belongs to the given major version series (e.g. "2" matches "2.1.1").

    Returns a dict with 'tag' and 'date' (YYYY-MM-DD) keys, or None if no
    matching release was found or the API call failed. Failures here are
    non-fatal - this is purely informational, unlike the priority field checks.
    """
    try:
        headers = {"Accept": "application/vnd.github.v3+json"}
        if token:
            headers["Authorization"] = f"token {token}"

        response = requests.get(
            f"https://api.github.com/repos/{repo}/releases",
            headers=headers,
            params={"per_page": 30},
            timeout=30
        )
        response.raise_for_status()
        releases = response.json()

        prefix = f"{major_version}."
        for release in releases:
            if release.get("draft") or release.get("prerelease"):
                continue
            tag = release.get("tag_name", "")
            if tag.startswith(prefix):
                published_at = release.get("published_at", "")
                return {
                    "tag": tag,
                    "date": published_at.split("T")[0] if published_at else ""
                }

        print(f"INFO: No published releases found for series '{major_version}.x'", file=sys.stderr)
        return None

    except requests.RequestException as e:
        print(f"Warning: Could not fetch latest release from GitHub API: {e}", file=sys.stderr)
        return None


def get_milestone_due_date(token: str, repo: str, milestone_filter: str) -> Optional[str]:
    """
    Finds the open GitHub milestone whose title contains milestone_filter
    (e.g. "2.2" matches "HDF5 2.2.0", the same substring match used for
    project item filtering) and returns its due date (YYYY-MM-DD), or None
    if no matching milestone exists or it has no due date set. Non-fatal.
    """
    try:
        headers = {"Accept": "application/vnd.github.v3+json"}
        if token:
            headers["Authorization"] = f"token {token}"

        response = requests.get(
            f"https://api.github.com/repos/{repo}/milestones",
            headers=headers,
            params={"state": "open", "per_page": 100},
            timeout=30
        )
        response.raise_for_status()
        milestones = response.json()

        for milestone in milestones:
            title = milestone.get("title", "")
            if milestone_filter in title:
                due_on = milestone.get("due_on")
                return due_on.split("T")[0] if due_on else None

        print(f"INFO: No open milestone matching '{milestone_filter}' found", file=sys.stderr)
        return None

    except requests.RequestException as e:
        print(f"Warning: Could not fetch milestone due date from GitHub API: {e}", file=sys.stderr)
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

    # Look up the most recent published release in the current major version series
    # (e.g. "2" -> "2.1.1"), independent of the project-board data above.
    REPO = os.getenv("GITHUB_REPOSITORY", "HDFGroup/hdf5")
    MAJOR_VERSION = MILESTONE_FILTER.split(".")[0] if MILESTONE_FILTER else None
    latest_release = get_latest_series_release(TOKEN, REPO, MAJOR_VERSION) if MAJOR_VERSION else None
    LATEST_RELEASE_TAG = latest_release["tag"] if latest_release else ""
    LATEST_RELEASE_DATE = latest_release["date"] if latest_release else ""

    # Look up the target due date for the in-development milestone (e.g. "HDF5 2.2.0"),
    # to annotate the Next Release badge.
    MILESTONE_DUE_DATE = get_milestone_due_date(TOKEN, REPO, MILESTONE_FILTER) if MILESTONE_FILTER else None
    MILESTONE_DUE_DATE = MILESTONE_DUE_DATE or ""

    # Write these independent of the project-board query below, so a project-board
    # failure doesn't discard release/milestone data that was already fetched.
    github_output = os.getenv("GITHUB_OUTPUT")
    if github_output:
        with open(github_output, "a") as f:
            f.write(f"version={MILESTONE_FILTER or 'all'}\n")
            f.write(f"latest_release_tag={LATEST_RELEASE_TAG}\n")
            f.write(f"latest_release_date={LATEST_RELEASE_DATE}\n")
            f.write(f"milestone_due_date={MILESTONE_DUE_DATE}\n")
    print(f"version={MILESTONE_FILTER or 'all'}")
    print(f"latest_release_tag={LATEST_RELEASE_TAG}")
    print(f"latest_release_date={LATEST_RELEASE_DATE}")
    print(f"milestone_due_date={MILESTONE_DUE_DATE}")
    if LATEST_RELEASE_TAG:
        print(f"Latest {MAJOR_VERSION}.x release: {LATEST_RELEASE_TAG} ({LATEST_RELEASE_DATE})")
    if MILESTONE_DUE_DATE:
        print(f"Milestone due date: {MILESTONE_DUE_DATE}")

    try:
        tracker = GitHubProjectTracker(TOKEN, OWNER, PROJECT_NUMBER, MILESTONE_FILTER)
        stats = tracker.fetch_release_blocker_stats()

        # Output for GitHub Actions
        if github_output:
            with open(github_output, "a") as f:
                f.write(f"percentage={stats['percentage']}\n")
                f.write(f"done={stats['done']}\n")
                f.write(f"total={stats['total']}\n")
                f.write(f"blocker_total={stats['blocker_total']}\n")
                f.write(f"blocker_done={stats['blocker_done']}\n")
                f.write(f"mustdo_total={stats['mustdo_total']}\n")
                f.write(f"mustdo_done={stats['mustdo_done']}\n")
                f.write(f"medium_total={stats['medium_total']}\n")
                f.write(f"medium_done={stats['medium_done']}\n")
                f.write(f"low_total={stats['low_total']}\n")
                f.write(f"low_done={stats['low_done']}\n")

        # Also output to stdout for local testing
        print(f"percentage={stats['percentage']}")
        print(f"blocker_done={stats['blocker_done']}")
        print(f"blocker_total={stats['blocker_total']}")
        print(f"mustdo_done={stats['mustdo_done']}")
        print(f"mustdo_total={stats['mustdo_total']}")
        print(f"medium_done={stats['medium_done']}")
        print(f"medium_total={stats['medium_total']}")
        print(f"low_done={stats['low_done']}")
        print(f"low_total={stats['low_total']}")
        print(f"Calculated progress: {stats['percentage']}%")
        print(f"Done / Total: {stats['done']} / {stats['total']}")
        print(f"Critical Priority: {stats['blocker_done']} / {stats['blocker_total']}")
        print(f"High Priority: {stats['mustdo_done']} / {stats['mustdo_total']}")
        print(f"Medium Priority: {stats['medium_done']} / {stats['medium_total']}")
        print(f"Low Priority: {stats['low_done']} / {stats['low_total']}")
        if MILESTONE_FILTER:
            print(f"Milestone filter: {MILESTONE_FILTER}")

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())
