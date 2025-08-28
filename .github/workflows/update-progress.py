#!/usr/bin/env python3
"""
GitHub Project Release Blocker Progress Tracker
Fetches release blocker issues from the HDF5 project and calculates completion percentage.
"""

import requests
from typing import Dict, Any, Optional

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
        Fetches release blocker statistics from the GitHub project.
        
        Returns:
            Dict with 'total', 'done', and 'percentage' keys
        """
        total = 0
        done = 0
        cursor = None
        
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
                
                if fields.get("Release gating") == "Release_Blocker":
                    total += 1
                    if fields.get("Status") == "Done":
                        done += 1
            
            # Check for next page
            page_info = items.get("pageInfo", {})
            if not page_info.get("hasNextPage", False):
                break
            cursor = page_info.get("endCursor")
        
        percentage = round((done / total * 100), 1) if total > 0 else 0
        
        return {
            'total': total,
            'done': done,
            'percentage': percentage
        }


def main():
    """Main function to run the tracker."""
    import os
    import sys
    
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
        
        # Also output to stdout for local testing
        print(f"percentage={stats['percentage']}")
        print(f"Calculated progress: {stats['percentage']}%")
        print(f"Done / Total: {stats['done']} / {stats['total']}")
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())
