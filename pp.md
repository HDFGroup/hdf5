diff --git a/.github/workflows/tarball.yml b/.github/workflows/tarball.yml
index bea662e53f5..2ad69cb4978 100644
--- a/.github/workflows/tarball.yml
+++ b/.github/workflows/tarball.yml
@@ -106,7 +106,7 @@ jobs:
         id: version
         run: |
           cd "$GITHUB_WORKSPACE/hdfsrc"
-          echo "SOURCE_TAG=$(bin/h5vers)" >> $GITHUB_OUTPUT
+          echo "SOURCE_TAG=$(grep '#define H5_VERS_STR' src/H5public.h | sed 's/.*"\(.*\)".*/\1/')" >> $GITHUB_OUTPUT
 
       - name: Set file base name
         id: set-file-base
diff --git a/.github/workflows/update-progress.py b/.github/workflows/update-progress.py
index 45c6b93f37b..6d626474303 100644
--- a/.github/workflows/update-progress.py
+++ b/.github/workflows/update-progress.py
@@ -7,9 +7,22 @@ Fetches release blocker issues from the HDF5 project and calculates completion p
 import requests
 from typing import Dict, Any, Optional
 
+# Configuration: Expected field names in GitHub Project
+# Update these if the project field names change
+FIELD_RELEASE_GATING = "Release gating"
+FIELD_STATUS = "Status"
+
+# Expected values for Release gating field
+VALUE_RELEASE_BLOCKER = "Release_Blocker"
+VALUE_RELEASE_MUST_DO = "Release_Must Do"
+
+# Expected value for Status field when an item is completed
+VALUE_STATUS_DONE = "Done"
+
+
 class GitHubProjectTracker:
     """Tracks release blocker progress in GitHub projects."""
-    
+
     def __init__(self, token: str, owner: str, project_number: int):
         self.api_url = "https://api.github.com/graphql"
         self.headers = {
@@ -89,22 +102,29 @@ class GitHubProjectTracker:
     
     def fetch_release_blocker_stats(self) -> Dict[str, int]:
         """
-        Fetches release blocker statistics from the GitHub project.
-        
+        Fetches release blocker and must-do statistics from the GitHub project.
+
         Returns:
-            Dict with 'total', 'done', and 'percentage' keys
+            Dict with 'total', 'done', 'percentage', 'blocker_total', 'blocker_done',
+            'mustdo_total', 'mustdo_done' keys
         """
-        total = 0
-        done = 0
+        blocker_total = 0
+        blocker_done = 0
+        mustdo_total = 0
+        mustdo_done = 0
         cursor = None
-        
+
+        # Track if we've seen the expected fields at least once
+        seen_release_gating = False
+        seen_status = False
+
         while True:
             variables = {
                 "owner": self.owner,
                 "projectNumber": self.project_number,
                 "cursor": cursor
             }
-            
+
             try:
                 response = requests.post(
                     self.api_url,
@@ -114,40 +134,69 @@ class GitHubProjectTracker:
                 )
                 response.raise_for_status()
                 result = response.json()
-                
+
                 if "errors" in result:
                     raise Exception(f"GraphQL errors: {result['errors']}")
-                    
+
             except requests.RequestException as e:
                 raise Exception(f"API request failed: {e}")
-            
+
             # Parse response
             project = result.get("data", {}).get("organization", {}).get("projectV2", {})
             items = project.get("items", {})
-            
+
             for item in items.get("nodes", []):
                 if not item.get("content"):
                     continue
-                    
+
                 fields = self._parse_item_fields(item)
-                
-                if fields.get("Release gating") == "Release_Blocker":
-                    total += 1
-                    if fields.get("Status") == "Done":
-                        done += 1
-            
+
+                # Validate expected fields exist
+                if FIELD_RELEASE_GATING in fields:
+                    seen_release_gating = True
+                if FIELD_STATUS in fields:
+                    seen_status = True
+
+                release_gating = fields.get(FIELD_RELEASE_GATING, "")
+                status = fields.get(FIELD_STATUS, "")
+
+                if release_gating == VALUE_RELEASE_BLOCKER:
+                    blocker_total += 1
+                    if status == VALUE_STATUS_DONE:
+                        blocker_done += 1
+                elif release_gating == VALUE_RELEASE_MUST_DO:
+                    mustdo_total += 1
+                    if status == VALUE_STATUS_DONE:
+                        mustdo_done += 1
+
             # Check for next page
             page_info = items.get("pageInfo", {})
             if not page_info.get("hasNextPage", False):
                 break
             cursor = page_info.get("endCursor")
-        
+
+        # Validate that expected fields were found
+        if not seen_release_gating:
+            import sys
+            print(f"WARNING: '{FIELD_RELEASE_GATING}' field not found in any project items. "
+                  f"Field may have been renamed or project structure changed.", file=sys.stderr)
+        if not seen_status:
+            import sys
+            print(f"WARNING: '{FIELD_STATUS}' field not found in any project items. "
+                  f"Field may have been renamed or project structure changed.", file=sys.stderr)
+
+        total = blocker_total + mustdo_total
+        done = blocker_done + mustdo_done
         percentage = round((done / total * 100), 1) if total > 0 else 0
-        
+
         return {
             'total': total,
             'done': done,
-            'percentage': percentage
+            'percentage': percentage,
+            'blocker_total': blocker_total,
+            'blocker_done': blocker_done,
+            'mustdo_total': mustdo_total,
+            'mustdo_done': mustdo_done
         }
 
 
@@ -172,11 +221,17 @@ def main():
                 f.write(f"percentage={stats['percentage']}\n")
                 f.write(f"done={stats['done']}\n")
                 f.write(f"total={stats['total']}\n")
-        
+                f.write(f"blocker_total={stats['blocker_total']}\n")
+                f.write(f"blocker_done={stats['blocker_done']}\n")
+                f.write(f"mustdo_total={stats['mustdo_total']}\n")
+                f.write(f"mustdo_done={stats['mustdo_done']}\n")
+
         # Also output to stdout for local testing
         print(f"percentage={stats['percentage']}")
         print(f"Calculated progress: {stats['percentage']}%")
         print(f"Done / Total: {stats['done']} / {stats['total']}")
+        print(f"Blockers: {stats['blocker_done']} / {stats['blocker_total']}")
+        print(f"Must Do: {stats['mustdo_done']} / {stats['mustdo_total']}")
         
     except Exception as e:
         print(f"Error: {e}", file=sys.stderr)
diff --git a/.github/workflows/update-progress.yml b/.github/workflows/update-progress.yml
index e0af91d4236..15ee8ca3614 100644
--- a/.github/workflows/update-progress.yml
+++ b/.github/workflows/update-progress.yml
@@ -115,12 +115,23 @@ jobs:
           exit 1
         fi
         
+        # Extract blocker and must-do counts
+        BLOCKER_DONE=$(grep "^blocker_done=" progress_output.txt | cut -d'=' -f2 | head -1)
+        BLOCKER_TOTAL=$(grep "^blocker_total=" progress_output.txt | cut -d'=' -f2 | head -1)
+        MUSTDO_DONE=$(grep "^mustdo_done=" progress_output.txt | cut -d'=' -f2 | head -1)
+        MUSTDO_TOTAL=$(grep "^mustdo_total=" progress_output.txt | cut -d'=' -f2 | head -1)
+
         # Set outputs for use in subsequent steps
         echo "percentage=$PERCENTAGE" >> $GITHUB_OUTPUT
         echo "done=$DONE" >> $GITHUB_OUTPUT
         echo "total=$TOTAL" >> $GITHUB_OUTPUT
-        
+        echo "blocker_done=$BLOCKER_DONE" >> $GITHUB_OUTPUT
+        echo "blocker_total=$BLOCKER_TOTAL" >> $GITHUB_OUTPUT
+        echo "mustdo_done=$MUSTDO_DONE" >> $GITHUB_OUTPUT
+        echo "mustdo_total=$MUSTDO_TOTAL" >> $GITHUB_OUTPUT
+
         echo "::notice::Progress calculation successful: ${PERCENTAGE}% (${DONE}/${TOTAL})"
+        echo "::notice::  Blockers: ${BLOCKER_DONE}/${BLOCKER_TOTAL}, Must Do: ${MUSTDO_DONE}/${MUSTDO_TOTAL}"
         
         # Clean up
         rm -f progress_output.txt
@@ -138,6 +149,10 @@ jobs:
         PERCENTAGE="${{ steps.progress.outputs.percentage }}"
         DONE="${{ steps.progress.outputs.done }}"
         TOTAL="${{ steps.progress.outputs.total }}"
+        BLOCKER_DONE="${{ steps.progress.outputs.blocker_done }}"
+        BLOCKER_TOTAL="${{ steps.progress.outputs.blocker_total }}"
+        MUSTDO_DONE="${{ steps.progress.outputs.mustdo_done }}"
+        MUSTDO_TOTAL="${{ steps.progress.outputs.mustdo_total }}"
         
         echo "::notice::Updating badge with: ${PERCENTAGE}% (${DONE}/${TOTAL})"
         
@@ -159,39 +174,84 @@ jobs:
         fi
         
         echo "::notice title=Release Progress::${PERCENTAGE}% Complete (${DONE}/${TOTAL}) - ${STATUS}"
-        
-        # Create badge JSON for shields.io endpoint with proper escaping (\($done)/\($total))
-        BADGE_JSON=$(jq -n \
-          --arg percentage "$PERCENTAGE" \
-          --arg done "$DONE" \
-          --arg total "$TOTAL" \
-          --arg color "$COLOR" \
-          '{
-            "schemaVersion": 1,
-            "label": "Release Progress",
-            "message": "\($percentage)%",
-            "color": $color,
-            "style": "flat-square"
-          }')
-          
-        # Validate JSON was created successfully
-        if [ -z "$BADGE_JSON" ] || ! echo "$BADGE_JSON" | jq empty 2>/dev/null; then
-          echo "::error::Failed to generate valid badge JSON"
+
+        # Function to determine badge color based on percentage
+        get_badge_color() {
+          local percentage_int="${1%.*}"
+          if [ "$percentage_int" -ge 90 ]; then
+            echo "brightgreen"
+          elif [ "$percentage_int" -ge 60 ]; then
+            echo "yellow"
+          elif [ "$percentage_int" -ge 40 ]; then
+            echo "orange"
+          else
+            echo "red"
+          fi
+        }
+
+        # Function to create badge JSON
+        create_badge_json() {
+          local label="$1"
+          local done="$2"
+          local total="$3"
+          local percentage="$4"
+          local color="$5"
+
+          jq -n \
+            --arg label "$label" \
+            --arg percentage "$percentage" \
+            --arg done "$done" \
+            --arg total "$total" \
+            --arg color "$color" \
+            '{
+              "schemaVersion": 1,
+              "label": $label,
+              "message": "\($done)/\($total) (\($percentage)%)",
+              "color": $color,
+              "style": "flat-square"
+            }'
+        }
+
+        # Calculate percentages for each category
+        BLOCKER_PERCENTAGE=$(awk "BEGIN {printf \"%.1f\", ($BLOCKER_DONE / $BLOCKER_TOTAL * 100)}" 2>/dev/null || echo "0")
+        MUSTDO_PERCENTAGE=$(awk "BEGIN {printf \"%.1f\", ($MUSTDO_DONE / $MUSTDO_TOTAL * 100)}" 2>/dev/null || echo "0")
+
+        # Determine colors using the shared function
+        BLOCKER_COLOR=$(get_badge_color "$BLOCKER_PERCENTAGE")
+        MUSTDO_COLOR=$(get_badge_color "$MUSTDO_PERCENTAGE")
+
+        # Create badge JSONs using the shared function
+        BLOCKER_BADGE_JSON=$(create_badge_json "Release Blockers" "$BLOCKER_DONE" "$BLOCKER_TOTAL" "$BLOCKER_PERCENTAGE" "$BLOCKER_COLOR")
+        MUSTDO_BADGE_JSON=$(create_badge_json "Release Must Do" "$MUSTDO_DONE" "$MUSTDO_TOTAL" "$MUSTDO_PERCENTAGE" "$MUSTDO_COLOR")
+
+        # Validate JSONs were created successfully
+        if [ -z "$BLOCKER_BADGE_JSON" ] || ! echo "$BLOCKER_BADGE_JSON" | jq empty 2>/dev/null; then
+          echo "::error::Failed to generate valid blocker badge JSON"
           exit 1
         fi
-          
-        # The filename in the Gist must match the one created manually
-        GIST_NAME="release-progress-${GITHUB_REPOSITORY##*/}.json"
-        echo "::notice::Updating Gist file: $GIST_NAME"
-        
-        # Create the request payload
+        if [ -z "$MUSTDO_BADGE_JSON" ] || ! echo "$MUSTDO_BADGE_JSON" | jq empty 2>/dev/null; then
+          echo "::error::Failed to generate valid must-do badge JSON"
+          exit 1
+        fi
+
+        # The filenames in the Gist
+        BLOCKER_GIST_NAME="release-blocker-${GITHUB_REPOSITORY##*/}.json"
+        MUSTDO_GIST_NAME="release-mustdo-${GITHUB_REPOSITORY##*/}.json"
+        echo "::notice::Updating Gist files: $BLOCKER_GIST_NAME, $MUSTDO_GIST_NAME"
+
+        # Create the request payload with both files
         REQUEST_PAYLOAD=$(jq -n \
-          --arg filename "$GIST_NAME" \
-          --argjson content "$BADGE_JSON" \
+          --arg blocker_filename "$BLOCKER_GIST_NAME" \
+          --arg mustdo_filename "$MUSTDO_GIST_NAME" \
+          --argjson blocker_content "$BLOCKER_BADGE_JSON" \
+          --argjson mustdo_content "$MUSTDO_BADGE_JSON" \
           '{
             "files": {
-              ($filename): {
-                "content": ($content | tostring)
+              ($blocker_filename): {
+                "content": ($blocker_content | tostring)
+              },
+              ($mustdo_filename): {
+                "content": ($mustdo_content | tostring)
               }
             }
           }')
@@ -216,30 +276,45 @@ jobs:
         fi
         
         echo "::notice::Gist updated successfully"
-        
-        # Generate badge URL for use in README
-        BADGE_URL="https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/${GITHUB_REPOSITORY_OWNER}-Bot/${GIST_ID}/raw/${GIST_NAME}"
+
+        # Generate badge URLs for use in README
+        BLOCKER_BADGE_URL="https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/${GITHUB_REPOSITORY_OWNER}-Bot/${GIST_ID}/raw/${BLOCKER_GIST_NAME}"
+        MUSTDO_BADGE_URL="https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/${GITHUB_REPOSITORY_OWNER}-Bot/${GIST_ID}/raw/${MUSTDO_GIST_NAME}"
         PROJECT_URL="https://github.com/${GITHUB_REPOSITORY}/projects/39"
-        
-        echo "::notice::Badge URL generated: $BADGE_URL"
-        echo "badge_url=$BADGE_URL" >> $GITHUB_OUTPUT
+
+        echo "::notice::Blocker Badge URL: $BLOCKER_BADGE_URL"
+        echo "::notice::Must-Do Badge URL: $MUSTDO_BADGE_URL"
+        echo "blocker_badge_url=$BLOCKER_BADGE_URL" >> $GITHUB_OUTPUT
+        echo "mustdo_badge_url=$MUSTDO_BADGE_URL" >> $GITHUB_OUTPUT
         echo "project_url=$PROJECT_URL" >> $GITHUB_OUTPUT
-          
+
         # Create enhanced step summary
         echo "## 📊 Release Progress: ${PERCENTAGE}%" >> $GITHUB_STEP_SUMMARY
         echo "" >> $GITHUB_STEP_SUMMARY
         echo "**Status:** ${STATUS}" >> $GITHUB_STEP_SUMMARY
-        echo "**Progress:** ${DONE} of ${TOTAL} release blockers completed" >> $GITHUB_STEP_SUMMARY
-        echo "**Badge Color:** ${COLOR}" >> $GITHUB_STEP_SUMMARY
+        echo "**Overall Progress:** ${DONE} of ${TOTAL} items completed" >> $GITHUB_STEP_SUMMARY
+        echo "" >> $GITHUB_STEP_SUMMARY
+        echo "### 🚫 Release Blockers" >> $GITHUB_STEP_SUMMARY
+        echo "**Progress:** ${BLOCKER_DONE} of ${BLOCKER_TOTAL} completed (${BLOCKER_PERCENTAGE}%)" >> $GITHUB_STEP_SUMMARY
+        echo "**Badge Color:** ${BLOCKER_COLOR}" >> $GITHUB_STEP_SUMMARY
+        echo "" >> $GITHUB_STEP_SUMMARY
+        echo "### ✅ Release Must Do" >> $GITHUB_STEP_SUMMARY
+        echo "**Progress:** ${MUSTDO_DONE} of ${MUSTDO_TOTAL} completed (${MUSTDO_PERCENTAGE}%)" >> $GITHUB_STEP_SUMMARY
+        echo "**Badge Color:** ${MUSTDO_COLOR}" >> $GITHUB_STEP_SUMMARY
+        echo "" >> $GITHUB_STEP_SUMMARY
         echo "**Gist ID:** ${GIST_ID}" >> $GITHUB_STEP_SUMMARY
         echo "" >> $GITHUB_STEP_SUMMARY
         echo "### Badge URLs" >> $GITHUB_STEP_SUMMARY
-        echo "**Markdown:** \`[![Release Progress](${BADGE_URL})](${PROJECT_URL})\`" >> $GITHUB_STEP_SUMMARY
-        echo "**Image Only:** \`![Release Progress](${BADGE_URL})\`" >> $GITHUB_STEP_SUMMARY
+        echo "**Blocker Markdown:** \`[![Release Blocker Progress](${BLOCKER_BADGE_URL})](${PROJECT_URL})\`" >> $GITHUB_STEP_SUMMARY
+        echo "**Must-Do Markdown:** \`[![Release Must Do Progress](${MUSTDO_BADGE_URL})](${PROJECT_URL})\`" >> $GITHUB_STEP_SUMMARY
         echo "" >> $GITHUB_STEP_SUMMARY
         echo "### Badge JSON Preview" >> $GITHUB_STEP_SUMMARY
         echo '```json' >> $GITHUB_STEP_SUMMARY
-        echo "$BADGE_JSON" >> $GITHUB_STEP_SUMMARY
+        echo "// Blocker Badge" >> $GITHUB_STEP_SUMMARY
+        echo "$BLOCKER_BADGE_JSON" >> $GITHUB_STEP_SUMMARY
+        echo "" >> $GITHUB_STEP_SUMMARY
+        echo "// Must-Do Badge" >> $GITHUB_STEP_SUMMARY
+        echo "$MUSTDO_BADGE_JSON" >> $GITHUB_STEP_SUMMARY
         echo '```' >> $GITHUB_STEP_SUMMARY
         
     - name: Cleanup on failure
diff --git a/CITATION.cff b/CITATION.cff
index c96341f138c..20d7348c128 100644
--- a/CITATION.cff
+++ b/CITATION.cff
@@ -10,3 +10,7 @@ authors:
 repository-code: 'https://github.com/HDFGroup/hdf5'
 url: 'https://www.hdfgroup.org/HDF5/'
 repository-artifact: 'https://support.hdfgroup.org/downloads/index.html'
+identifiers:
+  - type: doi
+    value: 10.5281/zenodo.17808614
+    description: 'Zenodo DOI for all versions'
diff --git a/CMakeLists.txt b/CMakeLists.txt
index 6c4532a2b77..f6f2b821586 100644
--- a/CMakeLists.txt
+++ b/CMakeLists.txt
@@ -350,6 +350,12 @@ string (REGEX REPLACE ".*#define[ \t]+H5_VERS_RELEASE[ \t]+([0-9]*).*$"
     "\\1" H5_VERS_RELEASE ${_h5public_h_contents})
 string (REGEX REPLACE ".*#define[ \t]+H5_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z._-]*)\".*$"
     "\\1" H5_VERS_SUBRELEASE ${_h5public_h_contents})
+# Validate that H5_VERS_SUBRELEASE was properly extracted (must be a quoted string in H5public.h)
+if (H5_VERS_SUBRELEASE STREQUAL _h5public_h_contents)
+  message (FATAL_ERROR "Failed to extract H5_VERS_SUBRELEASE from src/H5public.h. "
+                       "Ensure H5_VERS_SUBRELEASE is defined as a quoted string literal, "
+                       "e.g., #define H5_VERS_SUBRELEASE \"\" or #define H5_VERS_SUBRELEASE \"-snap0\"")
+endif ()
 message (TRACE "VERSION: ${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}-${H5_VERS_SUBRELEASE}")
 
 #-----------------------------------------------------------------------------
diff --git a/HDF5Examples/JAVA/README-MAVEN.md b/HDF5Examples/JAVA/README-MAVEN.md
index c08d5823e54..07e5d055f6d 100644
--- a/HDF5Examples/JAVA/README-MAVEN.md
+++ b/HDF5Examples/JAVA/README-MAVEN.md
@@ -1,581 +1,178 @@
-# HDF5 Java Examples Maven Integration
+# HDF5 Java Examples & Maven Integration
 
-This directory contains Java examples demonstrating the usage of HDF5 Java bindings, organized into categories and deployable as a Maven artifact.
+This directory contains Java examples demonstrating the usage of HDF5 Java bindings (JNI and FFM), organized into categories and deployable as a Maven artifact.
 
 ## Directory Structure
 
-```
+```text
 HDF5Examples/JAVA/
 ├── H5D/          # Dataset operations examples
 ├── H5T/          # Datatype operations examples
 ├── H5G/          # Group operations examples
 ├── TUTR/         # Tutorial examples
-├── pom-examples.xml.in    # Maven POM template for examples
-├── CMakeLists.txt         # CMake configuration
-└── README-MAVEN.md        # This file
+├── pom-examples.xml.in    # Maven POM template
+└── README.md              # This file
 ```
 
-## Maven Artifact Usage
-
-### Using Examples as Dependency
+## 1. Getting Started
 
-```xml
-<dependency>
-    <groupId>org.hdfgroup</groupId>
-    <artifactId>hdf5-java-examples</artifactId>
-    <version>2.0.0</version>
-</dependency>
-```
+To run these examples, you need two things:
+1.  **The Java Dependencies** (Managed by Maven)
+2.  **The Native HDF5 Libraries** (Installed on your OS)
 
-### Platform-Specific Dependencies
+### Step 1: Maven Dependency Configuration
+Add the examples and the specific platform binding to your `pom.xml`.
 
-The examples depend on platform-specific HDF5 Java libraries:
+**Note:** Replace `X.Y.Z` with your target HDF5 version (e.g., `1.14.0`).
 
 ```xml
-<!-- Linux -->
-<dependency>
-    <groupId>org.hdfgroup</groupId>
-    <artifactId>hdf5-java</artifactId>
-    <version>2.0.0</version>
-    <classifier>linux-x86_64</classifier>
-</dependency>
-
-<!-- Windows -->
-<dependency>
-    <groupId>org.hdfgroup</groupId>
-    <artifactId>hdf5-java</artifactId>
-    <version>2.0.0</version>
-    <classifier>windows-x86_64</classifier>
-</dependency>
-
-<!-- macOS Intel -->
-<dependency>
-    <groupId>org.hdfgroup</groupId>
-    <artifactId>hdf5-java</artifactId>
-    <version>2.0.0</version>
-    <classifier>macos-x86_64</classifier>
-</dependency>
-
-<!-- macOS Apple Silicon -->
-<dependency>
-    <groupId>org.hdfgroup</groupId>
-    <artifactId>hdf5-java</artifactId>
-    <version>2.0.0</version>
-    <classifier>macos-aarch64</classifier>
-</dependency>
-```
-
-## Building Examples with Maven
+<dependencies>
+    <!-- Core Examples -->
+    <dependency>
+        <groupId>org.hdfgroup</groupId>
+        <artifactId>hdf5-java-examples</artifactId>
+        <version>X.Y.Z</version>
+    </dependency>
+
+    <!-- Platform-Specific Native Bindings (Select ONE) -->
+    
+    <!-- Linux -->
+    <dependency>
+        <groupId>org.hdfgroup</groupId>
+        <artifactId>hdf5-java</artifactId>
+        <version>X.Y.Z</version>
+        <classifier>linux-x86_64</classifier>
+    </dependency>
+
+    <!-- Windows -->
+    <dependency>
+        <groupId>org.hdfgroup</groupId>
+        <artifactId>hdf5-java</artifactId>
+        <version>X.Y.Z</version>
+        <classifier>windows-x86_64</classifier>
+    </dependency>
+
+    <!-- macOS (Intel) -->
+    <dependency>
+        <groupId>org.hdfgroup</groupId>
+        <artifactId>hdf5-java</artifactId>
+        <version>X.Y.Z</version>
+        <classifier>macos-x86_64</classifier>
+    </dependency>
+</dependencies>
+```
+
+### Step 2: Native Library Configuration (Crucial)
+Maven provides the Java JARs, but **it does not install the system-level HDF5 libraries** required for execution. You must have the HDF5 binaries installed or built on your system.
+
+**If you see `UnsatisfiedLinkError: no hdf5_java`, the application cannot find your native HDF5 installation.**
+
+#### Method A: Using Environment Variables (Recommended)
+If you have built HDF5 from source or installed it to a custom location, point to it:
+
+*   **Linux/macOS:** `export LD_LIBRARY_PATH=/path/to/hdf5/lib:$LD_LIBRARY_PATH`
+*   **Windows:** Add `C:\path\to\hdf5\bin` to your system `PATH`.
+
+#### Method B: System Installation
+If you installed via package manager (e.g., `apt install libhdf5-dev` or `brew install hdf5`), standard paths generally work automatically.
+
+---
+
+## 2. Building and Running Examples
 
 ### Compile All Examples
-
 ```bash
 cd HDF5Examples/JAVA
 mvn compile -f pom-examples.xml
 ```
 
-### Run Representative Examples
+### Run a Specific Example
+To run an example (e.g., `H5Ex_D_ReadWrite`), use the `exec:java` goal. Ensure your native library path is set (see Step 2 above).
 
+**JNI (Standard):**
 ```bash
-mvn test -Prun-examples -f pom-examples.xml
+mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
 ```
 
-### Create Examples JAR
-
+**FFM (Foreign Function & Memory - Requires Java 25+):**
 ```bash
-mvn package -f pom-examples.xml
+# Note: Ensure you are using a JDK supporting FFM
+mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
 ```
 
-This creates:
-- `hdf5-java-examples-{version}.jar` - Compiled examples
-- `hdf5-java-examples-{version}-sources.jar` - Source code
-- `hdf5-java-examples-{version}-javadoc.jar` - Documentation
-
-## Testing Maven Artifacts
-
-Two standalone scripts are provided to test HDF5 Maven artifacts against the examples in this directory:
-
-### test-maven-jni.sh - Test JNI Implementation
-
-Tests the JNI (Java Native Interface) implementation, compatible with Java 11+.
-
-**Usage:**
+### Create a Standalone JAR
 ```bash
-./test-maven-jni.sh [VERSION] [REPOSITORY_URL] [BUILD_DIR]
+mvn package -f pom-examples.xml
 ```
+This generates `target/hdf5-java-examples-{version}.jar` containing all compiled examples.
 
-**Examples:**
-```bash
-# Test latest snapshot from HDFGroup
-./test-maven-jni.sh 2.0.1-SNAPSHOT
+---
 
-# Test specific version from custom repository
-./test-maven-jni.sh 2.0.0 https://maven.pkg.github.com/myorg/hdf5
+## 3. Example Categories
 
-# Use custom build directory
-./test-maven-jni.sh 2.0.1-SNAPSHOT https://maven.pkg.github.com/HDFGroup/hdf5 /tmp/test
-```
+| Directory | Description | Key Concepts |
+| :--- | :--- | :--- |
+| **H5D** | **Dataset Operations** | Read/write, chunking, filters (gzip, nbit), fill values, allocation. |
+| **H5T** | **Datatype Operations** | Compound types, arrays, enums, opaque types, variable-length strings. |
+| **H5G** | **Group Operations** | Creating groups, iteration, hierarchy traversal, links. |
+| **TUTR** | **Tutorials** | Step-by-step introductory examples. |
 
-**What it does:**
-1. Downloads `hdf5-java-jni` artifact from Maven repository
-2. Verifies JAR contains HDF5 classes (not just dependencies)
-3. Compiles all 55 HDF5 v2.0+ examples from `compat/` subdirectories
-4. Executes 12 comprehensive tests covering major HDF5 features
-5. Reports results with detailed pass/fail summary
+---
 
-**Prerequisites:**
-- Java 21 or later (class version 65.0)
-- Maven 3.6.0 or later
-- GitHub authentication (for GitHub Packages)
-- Optional: HDF5 native libraries or `HDF5_HOME` for execution tests
+## 4. Repo Verification & Testing Scripts
+*(For Maintainers and CI/CD)*
 
-### test-maven-ffm.sh - Test FFM Implementation
+We provide scripts to verify the integrity of Maven artifacts against these examples. These scripts simulate a clean environment to ensure the JARs are structured correctly.
 
-Tests the FFM (Foreign Function & Memory) implementation, requires Java 25+.
+### Script: `test-maven-jni.sh` (Standard)
+Tests the JNI implementation (Java 11+).
 
 **Usage:**
 ```bash
-./test-maven-ffm.sh [VERSION] [REPOSITORY_URL] [BUILD_DIR]
+./test-maven-jni.sh [VERSION] [REPO_URL] [BUILD_DIR]
 ```
 
-**Examples:**
-```bash
-# Test FFM snapshot
-./test-maven-ffm.sh 2.0.1-SNAPSHOT
+**Workflow:**
+1.  Downloads the `hdf5-java-jni` artifact.
+2.  Compiles 55+ examples.
+3.  **Note:** During this *artifact structure test*, an `UnsatisfiedLinkError` regarding native libraries is **expected behavior** if the runner lacks a local HDF5 installation. The test verifies that the Java classes load, not that the local machine has the binaries.
 
-# Test specific version
-./test-maven-ffm.sh 2.0.0-3 https://maven.pkg.github.com/HDFGroup/hdf5
-```
+### Script: `test-maven-ffm.sh` (Experimental)
+Tests the Foreign Function & Memory implementation (Requires Java 25+).
 
-**What it does:**
-1. Downloads `hdf5-java-ffm` artifact from Maven repository
-2. Verifies JAR contains FFM bindings (`org.hdfgroup.javahdf5.*`)
-3. Compiles 52 HDF5 v2.0+ examples from `compat/` subdirectories
-4. Executes 12 comprehensive tests covering major HDF5 features
-5. Reports results with detailed pass/fail summary
-
-**Note:** 3 callback-based examples are excluded (H5Ex_G_Visit, H5Ex_G_Intermediate, H5Ex_G_Traverse) as FFM callback handling differs from JNI and these examples have not yet been adapted.
-
-**Prerequisites:**
-- Java 25 or later (class version 69.0)
-- Maven 3.6.0 or later
-- GitHub authentication (for GitHub Packages)
-- Optional: HDF5 native libraries or `HDF5_HOME` for execution tests
-
-### Build Directory Pattern
-
-Both scripts use a separate build directory to keep the source tree clean:
-
-**Default locations:**
-- JNI: `HDF5Examples/JAVA/build/maven-test-jni/`
-- FFM: `HDF5Examples/JAVA/build/maven-test-ffm/`
-
-**Generated files:**
-```
-build/
-├── maven-test-jni/
-│   ├── pom-examples.xml       # Generated Maven POM
-│   ├── target/                # Compiled classes
-│   │   └── classes/
-│   └── *.h5                   # Output HDF5 files
-└── maven-test-ffm/
-    ├── pom-examples.xml
-    ├── target/
-    └── *.h5
+**Usage:**
+```bash
+./test-maven-ffm.sh [VERSION] [REPO_URL] [BUILD_DIR]
 ```
 
-**Benefits:**
-- ✅ Source tree stays clean (no generated files)
-- ✅ Easy cleanup: `rm -rf build/`
-- ✅ Multiple parallel tests possible
-- ✅ CMake-like out-of-source build pattern
+**Notes:**
+*   Excludes callback-based examples (`H5Ex_G_Visit`, etc.) as FFM callback handling differs from JNI.
+*   Requires valid GitHub authentication if pulling from GitHub Packages.
 
 ### GitHub Authentication
-
-For testing artifacts from GitHub Packages, authentication is required:
-
-**Option 1: GitHub CLI (Recommended)**
+For testing artifacts hosted on GitHub Packages:
 ```bash
+# Recommended: GitHub CLI
 gh auth login
 gh auth refresh --scopes read:packages
 ```
 
-Scripts automatically detect GitHub CLI authentication.
+---
 
-**Option 2: Maven settings.xml**
-```bash
-# Scripts can create settings.xml automatically if gh is authenticated
-# Or create manually:
-cat > ~/.m2/settings.xml <<EOF
-<settings>
-  <servers>
-    <server>
-      <id>github-hdfgroup-hdf5</id>
-      <username>YOUR_GITHUB_USERNAME</username>
-      <password>YOUR_GITHUB_TOKEN</password>
-    </server>
-  </servers>
-</settings>
-EOF
-```
-
-### Running Additional Examples
+## 5. Troubleshooting
 
-After initial test succeeds, you can run more examples:
-
-**JNI:**
-```bash
-cd build/maven-test-jni
-mvn exec:java -Dexec.mainClass="H5Ex_T_String" -f pom-examples.xml
-```
+**Q: `UnsatisfiedLinkError: no hdf5_java in java.library.path`**
+*   **Cause:** Java found the classes, but not the native C library.
+*   **Fix:** Ensure HDF5 is installed and `LD_LIBRARY_PATH` (Linux), `DYLD_LIBRARY_PATH` (macOS), or `PATH` (Windows) includes the folder containing `libhdf5.so`, `libhdf5.dylib`, or `hdf5.dll`.
 
-**FFM:**
-```bash
-cd build/maven-test-ffm
-mvn exec:java -Dexec.mainClass="H5Ex_T_String" -f pom-examples.xml
-```
+**Q: `package org.hdfgroup.hdf5 does not exist`**
+*   **Cause:** Maven dependencies are not resolving.
+*   **Fix:** Run `mvn dependency:resolve` and check your `pom.xml` version matches the available release.
 
-### Cleanup
-
-**Remove single test build:**
-```bash
-rm -rf build/maven-test-jni
-rm -rf build/maven-test-ffm
-```
-
-**Remove all test builds:**
-```bash
-rm -rf build/
-```
-
-**Clean with Maven (keeps directory structure):**
-```bash
-mvn clean -f build/maven-test-jni/pom-examples.xml
-mvn clean -f build/maven-test-ffm/pom-examples.xml
-```
-
-### Troubleshooting Test Scripts
-
-**"Failed to download artifact"**
-- Check GitHub authentication: `gh auth status`
-- Verify repository URL is correct
-- Ensure version exists in repository
-
-**"JAR does not contain HDF5 classes"**
-- Indicates incomplete Maven artifact (build issue)
-- This is what the verification step catches!
-- Report to maintainers if public artifact is incomplete
-
-**"Java version too old"**
-- JNI requires Java 11+
-- FFM requires Java 25+
-- Check: `java -version`
-
-**"UnsatisfiedLinkError: no hdf5_java"**
-- This is expected during Maven-only testing
-- Indicates JAR structure is correct
-- Native libraries would be needed for full execution
-
-## Example Categories
-
-### H5D - Dataset Operations
-- Basic read/write operations
-- Chunking and compression
-- External storage
-- Fill values and allocation
-- Filters (gzip, checksum, nbit, etc.)
-
-### H5T - Datatype Operations
-- Array datatypes
-- Compound datatypes
-- Enumerated datatypes
-- Opaque datatypes
-- String handling
-- Variable-length datatypes
-
-### H5G - Group Operations
-- Creating and managing groups
-- Group iteration
-- Intermediate group creation
-- Group hierarchy traversal
-
-### TUTR - Tutorial Examples
-- Step-by-step learning examples
-- Basic concepts demonstration
-- Progressive complexity
-
-## CI/CD Integration
-
-The examples are automatically tested in CI:
-
-1. **Compilation Testing**: All examples must compile successfully
-2. **Execution Testing**: Examples are run and output validated
-3. **Cross-Platform Testing**: Tested on Linux, Windows, and macOS
-4. **Maven Integration Testing**: Tests against staging Maven artifacts
-
-### Maven-Only Testing Behavior
-
-**Expected Native Library Errors**: During Maven-only testing (without HDF5 installation), examples will compile successfully but fail at runtime with:
-```
-UnsatisfiedLinkError: no hdf5_java in java.library.path
-```
-
-This is **expected behavior** and indicates:
-- ✅ **JAR structure is correct**
-- ✅ **Dependencies resolve properly**
-- ✅ **Compilation succeeds**
-- ⚠️ **Native HDF5 libraries not available** (expected in Maven-only environment)
-
-### Running Examples Successfully
-
-To actually execute examples (not just compile them), you need HDF5 native libraries installed:
-
-#### Option 1: Install HDF5 from Package Manager (Recommended)
-
-**Linux (Ubuntu/Debian):**
-```bash
-sudo apt-get update
-sudo apt-get install libhdf5-dev hdf5-tools
-```
-
-**Linux (Fedora/RHEL):**
-```bash
-sudo dnf install hdf5 hdf5-devel
-```
-
-**macOS (Homebrew):**
-```bash
-brew install hdf5
-```
-
-**Windows:**
-- Download pre-built binaries from [HDF Group Downloads](https://www.hdfgroup.org/downloads/hdf5/)
-- Set `HDF5_HOME` environment variable to installation directory
-- Alternatively, add HDF5 `bin` directory to system PATH
-
-#### Option 2: Build HDF5 from Source
-
-Build HDF5 with Java support enabled:
-
-```bash
-# Clone HDF5 repository
-git clone https://github.com/HDFGroup/hdf5.git
-cd hdf5
-
-# Build with Java (JNI)
-cmake --preset ci-StdShar-GNUC --fresh
-cmake --build build/ci-StdShar-GNUC
-sudo cmake --install build/ci-StdShar-GNUC
-
-# Or build with Java (FFM) - requires Java 25+
-cmake --preset ci-StdShar-GNUC-FFM --fresh
-cmake --build build/ci-StdShar-GNUC-FFM
-sudo cmake --install build/ci-StdShar-GNUC-FFM
-```
-
-#### Option 3: Set HDF5_HOME (Recommended for Custom Installations)
-
-If HDF5 is installed in a non-standard location, set `HDF5_HOME`:
-
-**Linux/macOS:**
-```bash
-# Point to HDF5 installation directory
-export HDF5_HOME=/path/to/hdf5/installation
-
-# Then run examples (scripts automatically find libraries)
-cd HDF5Examples/JAVA
-./test-maven-jni.sh 2.0.1-SNAPSHOT
-
-# Or run Maven directly
-cd build/maven-test-jni
-mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
-```
-
-**Windows (PowerShell):**
-```powershell
-# Set HDF5_HOME environment variable
-$env:HDF5_HOME = "C:\path\to\hdf5\installation"
-
-# Run Maven examples
-cd build\maven-test-jni
-mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
-```
-
-**Windows (CMD):**
-```cmd
-REM Set HDF5_HOME environment variable
-set HDF5_HOME=C:\path\to\hdf5\installation
-
-REM Run Maven examples
-cd build\maven-test-jni
-mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
-```
-
-**Note:** The test scripts automatically add `${HDF5_HOME}/lib` (Unix) or `%HDF5_HOME%\bin` (Windows) to the library path.
-
-#### Option 4: Specify Library Path in Java (Advanced)
-
-**Note:** This is an advanced option. Prefer using `HDF5_HOME` (Option 3) instead.
-
-```bash
-# Run with explicit library path
-java -Djava.library.path=/path/to/hdf5/lib \
-     -cp "target/classes:~/.m2/repository/org/hdfgroup/hdf5-java-jni/2.0.1-SNAPSHOT/*" \
-     H5Ex_D_ReadWrite
-```
-
-#### Verify Native Libraries Are Found
-
-After installing HDF5, verify the libraries are accessible:
-
-**Linux:**
-```bash
-# Check library is in system path
-ldconfig -p | grep hdf5
-
-# Or find library location
-find /usr -name "libhdf5.so*" 2>/dev/null
-```
-
-**macOS:**
-```bash
-# Check library location
-find /usr/local -name "libhdf5*.dylib" 2>/dev/null
-```
-
-**Windows:**
-```cmd
-# Check PATH includes HDF5 bin directory
-echo %PATH%
-
-# Verify DLL exists
-where hdf5.dll
-```
-
-#### Running Examples After Library Installation
-
-Once native libraries are installed, examples should run successfully:
-
-**JNI Examples:**
-```bash
-cd build/maven-test-jni
-mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
-
-# Expected output:
-# Dataset successfully created and written
-# Data read from dataset: [1, 2, 3, 4, ...]
-```
-
-**FFM Examples:**
-```bash
-cd build/maven-test-ffm
-mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
-
-# Expected output:
-# Dataset successfully created and written
-# Data read from dataset: [1, 2, 3, 4, ...]
-```
-
-#### Why Maven Artifacts Don't Include Native Libraries
-
-Maven artifacts contain only:
-- ✅ Java bytecode (.class files)
-- ✅ Java source code (in -sources.jar)
-- ✅ Javadoc (in -javadoc.jar)
-
-They do **not** include:
-- ❌ Native shared libraries (.so, .dll, .dylib)
-- ❌ Platform-specific binaries
-
-**Reason:** Native libraries are platform-specific and typically hundreds of MB. Maven artifacts should be small (~2-5 MB) and platform-independent where possible. The JNI/FFM bindings provide the Java interface, but you must install the native HDF5 libraries separately.
-
-### Pattern-Based Output Validation
-
-Examples are validated using pattern matching for:
-- **Success patterns**: `dataset|datatype|group|success|created|written|read`
-- **Expected failures**: `UnsatisfiedLinkError.*hdf5_java.*java.library.path` (Maven-only testing)
-- **Unexpected failures**: Other errors indicating JAR or compilation issues
-
-### Non-Blocking Failures
-
-- Individual example failures don't block CI
-- Native library errors are treated as **expected** in Maven-only testing
-- Multi-platform failures for the same example trigger alerts
-- Results are uploaded as artifacts for debugging
-
-## Development Workflow
-
-### Adding New Examples
-
-1. Add `.java` file to appropriate category directory
-2. Update CMakeLists.txt if needed
-3. Examples are automatically discovered by Maven and CI
-
-### Testing Changes
-
-```bash
-# Test specific category
-cd H5D && javac -cp "../../../maven-artifacts/*.jar" *.java
-
-# Run example
-java -cp ".:../../../maven-artifacts/*" H5Ex_D_ReadWrite
-```
-
-### Expected Output Files
-
-Expected outputs for validation are stored in version control:
-- `tfiles/min_hdf_version/H5Ex_D_ReadWrite.txt`
-- Pattern-based validation for flexibility
-- Platform-specific outputs handled automatically
-
-## Deployment
-
-Examples are deployed alongside main HDF5 Maven artifacts:
-
-1. Built during Maven staging workflow
-2. Tested in dedicated Java examples workflow
-3. Deployed to GitHub Packages
-4. Available for Maven Central deployment
-
-## Usage in Projects
-
-### Quick Start
-
-```java
-import hdf.hdf5lib.H5;
-import hdf.hdf5lib.HDF5Constants;
-
-// Use examples as reference
-// Source code available in JAR resources at examples/
-```
-
-### Maven Archetype (Future)
-
-```bash
-mvn archetype:generate \
-  -DgroupId=com.example \
-  -DartifactId=my-hdf5-project \
-  -DarchetypeGroupId=org.hdfgroup \
-  -DarchetypeArtifactId=hdf5-java-archetype
-```
-
-## Troubleshooting
-
-### Common Issues
-
-1. **Platform Mismatch**: Ensure correct classifier for your platform
-2. **Native Library Path**: HDF5 native libraries loaded automatically
-3. **Java Version**: Requires Java 11 or higher
-
-### Debug Information
-
-Examples JAR includes manifest entries:
-- `HDF5-Version`: HDF5 library version
-- `HDF5-Platform`: Target platform
-- `Examples-Count`: Number of included examples
+**Q: "Java version too old"**
+*   **Cause:** FFM examples require cutting-edge Java versions.
+*   **Fix:** Use JNI for standard production environments (Java 11+). Only use FFM if you are targeting Java 25+.
 
 ## Support
-
-- GitHub Issues: https://github.com/HDFGroup/hdf5/issues
-- Documentation: https://support.hdfgroup.org/documentation/
-- Examples Source: Included in JAR resources
+*   **Issues:** [GitHub Issue Tracker](https://github.com/HDFGroup/hdf5/issues)
+*   **Documentation:** [HDF Support Portal](https://support.hdfgroup.org/documentation/)
\ No newline at end of file
diff --git a/README.md b/README.md
index 7c8aae66a54..4624f0cf60d 100644
--- a/README.md
+++ b/README.md
@@ -1,30 +1,16 @@
-HDF5 version 2.0.1 currently under development
-
-> [!WARNING]
-> **Heads Up: HDF5 Dropped Autotools March 10th**
->
-> It's happened—the day we've all been dreading—or eagerly anticipating, depending on your perspective. Yes, we have switched to CMake-only builds in HDF5.
->
-> The [PR stripping all autotools](https://github.com/HDFGroup/hdf5/pull/5308) was merged into the "develop" branch on **March 10, 2025**. Starting with HDF5 2.0, *only* the CMake build system is supported.
+<div align="center">
 
 ![HDF5 Logo][u3]
 
-[![develop cmake build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/call-workflows.yml?branch=develop&label=HDF5%20develop%20CMake%20CI)](https://github.com/HDFGroup/hdf5/actions/workflows/call-workflows.yml?query=branch%3Adevelop)
-[![HDF5 develop daily build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/daily-schedule.yml?branch=develop&label=HDF5%20develop%20daily%20build)](https://github.com/HDFGroup/hdf5/actions/workflows/daily-schedule.yml?query=branch%3Adevelop)
-[![HDF-EOS5 build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/hdfeos5.yml?branch=develop&label=HDF-EOS5)](https://github.com/HDFGroup/hdf5/actions/workflows/hdfeos5.yml?query=branch%3Adevelop)
-[![netCDF build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/netcdf.yml?branch=develop&label=netCDF)](https://github.com/HDFGroup/hdf5/actions/workflows/netcdf.yml?query=branch%3Adevelop)
-[![h5py build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/h5py.yml?branch=develop&label=h5py)](https://github.com/HDFGroup/hdf5/actions/workflows/h5py.yml?query=branch%3Adevelop)
-[![CVE regression](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/cve.yml?branch=develop&label=CVE)](https://github.com/HDFGroup/hdf5/actions/workflows/cve.yml?query=branch%3Adevelop)
-[![HDF5 VOL connectors build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/vol.yml?branch=develop&label=HDF5-VOL)](https://github.com/HDFGroup/hdf5/actions/workflows/vol.yml?query=branch%3Adevelop)
-[![HDF5 VFD build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/vfd.yml?branch=develop&label=HDF5-VFD)](https://github.com/HDFGroup/hdf5/actions/workflows/vfd.yml?query=branch%3Adevelop)
 [![BSD](https://img.shields.io/badge/License-BSD-blue.svg)](https://github.com/HDFGroup/hdf5/blob/develop/LICENSE)
-[![OSS-Fuzz Status](https://oss-fuzz-build-logs.storage.googleapis.com/badges/hdf5.svg)](https://oss-fuzz-build-logs.storage.googleapis.com/index.html#hdf5)
-[![Link Checker Status](https://github.com/HDFGroup/hdf5/actions/workflows/linkchecker.yml/badge.svg)](https://github.com/HDFGroup/hdf5/actions/workflows/linkchecker.yml)
-[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17808614.svg)](https://doi.org/10.5281/zenodo.17808614)
+[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.17808614-blue)](https://doi.org/10.5281/zenodo.17808614)
+[![develop cmake build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/call-workflows.yml?branch=develop&label=CMake%20CI)](https://github.com/HDFGroup/hdf5/actions/workflows/call-workflows.yml?query=branch%3Adevelop)
+
+</div>
 
-[HPC configure/build/test results](https://my.cdash.org/index.php?project=HDF5)
+---
 
-*Please refer to the release_docs/INSTALL file for installation/usage instructions.*
+## What is HDF5?
 
 This repository contains a high-performance library's source code and a file format
 specification that implements the HDF5® data model. The model has been adopted across
@@ -33,115 +19,157 @@ in science, engineering, and research communities worldwide.
 
 The HDF Group is the developer, maintainer, and steward of HDF5 software. Find more
 information about The HDF Group, the HDF5 Community, and other HDF5 software projects,
-tools, and services at [The HDF Group's website](https://www.hdfgroup.org/). 
+tools, and services at [The HDF Group's website](https://www.hdfgroup.org/).
 
-DOCUMENTATION
--------------
-Documentation for all HDF software is available at:
+## Quick Start
+
+- **New to HDF5?** Start with the [INSTALL](release_docs/INSTALL) guide for compilation and installation instructions.
+
+- **Ready to build?** See [INSTALL_CMake.txt](release_docs/INSTALL_CMake.txt) for CMake-based builds.
 
-   https://support.hdfgroup.org/documentation/index.html
+- **Running on HPC?** Check out [README_HPC.md](release_docs/README_HPC.md) for parallel HDF5 configuration.
 
-The latest documentation for the HDF5 library can be found at:
+## Table of Contents
 
-   https://support.hdfgroup.org/documentation/hdf5/latest
+- [What is HDF5?](#what-is-hdf5)
+- [Quick Start](#quick-start)
+- [Documentation](#documentation)
+- [Help and Support](#help-and-support)
+- [Forum and News](#forum-and-news)
+- [Release Schedule](#release-schedule)
+- [Downloads and Source Code](#downloads-and-source-code)
+- [Java Maven Artifacts](#java-maven-artifacts)
+- [Contributing](#contributing)
+- [How to Cite HDF5](#how-to-cite-hdf5)
+- [Build Status](#build-status)
+
+## Documentation
+
+Documentation for all HDF software is available at:
+- **All HDF Documentation**: https://support.hdfgroup.org/documentation/index.html
+- **Latest HDF5 Library**: https://support.hdfgroup.org/documentation/hdf5/latest
 
 See the [CHANGELOG.md][u1] file in the [release_docs/][u4] directory for information specific
 to the features and updates included in this release of the library.
 
-Several more files are located within the [release_docs/][u4] directory with specific
-details for several common platforms and configurations.
-- INSTALL - Start Here. General instructions for compiling and installing the library or using an installed library
-- INSTALL_CMAKE - instructions for building with CMake (Kitware.com)
-- README_HPC.md - instructions for building and configuring Parallel HDF5 on HPC systems
-- INSTALL_Windows and INSTALL_Cygwin - MS Windows installations.
-- USING_HDF5_CMake - Build and Install HDF5 Applications with CMake
-- USING_CMake_Examples - Build and Test HDF5 Examples with CMake
+### Platform-Specific Guides
 
+Several files in the [release_docs/][u4] directory provide platform-specific details:
 
+| File | Description |
+|------|-------------|
+| [INSTALL](release_docs/INSTALL) | General compilation and installation instructions (start here) |
+| [INSTALL_CMake.txt](release_docs/INSTALL_CMake.txt) | Building with CMake |
+| [README_HPC.md](release_docs/README_HPC.md) | Building and configuring Parallel HDF5 on HPC systems |
+| [INSTALL_Windows.txt](release_docs/INSTALL_Windows.txt) | Windows installation |
+| [INSTALL_Cygwin.txt](release_docs/INSTALL_Cygwin.txt) | Cygwin installation |
+| [USING_HDF5_CMake.txt](release_docs/USING_HDF5_CMake.txt) | Building HDF5 applications with CMake |
+| [USING_CMake_Examples.txt](release_docs/USING_CMake_Examples.txt) | Building and testing HDF5 examples with CMake |
 
-HELP AND SUPPORT
-----------------
-The HDF Group staffs a free Help Desk accessible at [https://help.hdfgroup.org](https://help.hdfgroup.org) and also monitors the [Forum](https://forum.hdfgroup.org). Our free support service is community-based and handled as time allows. We’ll do our best to respond to your question as soon as possible, but please note that response times may vary depending on the complexity of the issue and staff availability.
+## Help and Support
 
-If you're interested in guaranteed response and resolution times, a dedicated technical account manager, and more benefits (all while supporting the open-source work of The HDF Group), please check out [Priority Support](https://www.hdfgroup.org/solutions/priority-support/).
+The HDF Group staffs a free Help Desk accessible at https://help.hdfgroup.org and also monitors the [Forum](https://forum.hdfgroup.org). Our free support service is community-based and handled as time allows. We'll do our best to respond to your question as soon as possible, but please note that response times may vary depending on the complexity of the issue and staff availability.
 
+If you're interested in guaranteed response and resolution times, a dedicated technical account manager, and more benefits (all while supporting the open-source work of The HDF Group), please check out [Priority Support](https://www.hdfgroup.org/solutions/priority-support/).
 
+## Forum and News
 
-FORUM and NEWS
---------------
 The [HDF Forum](https://forum.hdfgroup.org) is provided for public announcements, technical questions, and discussions
 of interest to the general HDF5 Community.
 
-   - News and Announcements
-   https://forum.hdfgroup.org/c/news-and-announcements-from-the-hdf-group
-
-   - HDF5 Topics
-   https://forum.hdfgroup.org/c/hdf5
+- [News and Announcements](https://forum.hdfgroup.org/c/news-and-announcements-from-the-hdf-group)
+- [HDF5 Topics](https://forum.hdfgroup.org/c/hdf5)
 
 These forums are provided as an open and public service for searching and reading.
 Posting requires completing a simple registration and allows one to join in the
-conversation.  Please read the [instructions](https://forum.hdfgroup.org/t/quickstart-guide-welcome-to-the-new-hdf-forum
-) for more information on how to get started.
+conversation. Please read the [quickstart guide](https://forum.hdfgroup.org/t/quickstart-guide-welcome-to-the-new-hdf-forum) for more information on how to get started.
 
-RELEASE SCHEDULE
-----------------
+## Release Schedule
 
-![HDF5 release schedule][u2] 
+![HDF5 release schedule][u2]
 
 HDF5 does not follow a regular release schedule. Instead, updates are based on the
 introduction of new features and the resolution of bugs. However, we aim to have at
 least one annual release for each maintenance branch.
 
-| Release | New Features |
-| ------- | ------------ |
-| 2.0.0 | Drop Autotools support, drop the HDF5 <--> GIF tools, add complex number support, update library defaults (cache sizes, etc.) |
-| FUTURE | Multi-threaded HDF5, crashproofing / metadata journaling, Full (VFD) SWMR, encryption, digital signatures, sparse datasets, improved storage for variable-length datatypes, better Unicode support (especially on Windows) |
-
 ### Release Progress
 
-[![Release Progress](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/HDFGroup-Bot/0ad2eabb63b28eb90d69f5e5b2c1496f/raw/release-progress-hdf5.json)](https://github.com/orgs/HDFGroup/projects/39/views/24)
+[![Release Blockers](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/brtnfld/a4fbdde293677bf3f63f3cfd3aef6b44/raw/release-blockers.json)](https://github.com/orgs/HDFGroup/projects/39)
+
+[![Release Must Do](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/brtnfld/8a077a919d048ce5dc9f56350bec10bc/raw/release-mustdo.json)](https://github.com/orgs/HDFGroup/projects/39)
+
+The badges above show the current progress of **release-blocking** and **must-do** issues with colors that reflect completion status:
+
+- 🟢 **Green (90%+)**: Readying for Deployment - most issues completed
+- 🟡 **Yellow (60-89%)**: Nearing Completion - on track for release
+- 🟠 **Orange (40-59%)**: In Development - attention needed
+- 🔴 **Red (<40%)**: Initial Phase - significant issues remain
 
-The badge above shows the current progress of release-blocking issues with colors that reflect completion status:
+Click the badges to view the detailed project board with current release items.
 
-- **🟢 Green (90%+)**:  Readying for Deployment - most blockers completed
-- **🟡 Yellow (60-89%)**:  Nearing Completion - on track for release
-- **🟠 Orange (40-59%)**:  In Development - attention needed
-- **🔴 Red (<40%)**:  Initial Phase - significant blockers remain
+## Downloads and Source Code
 
-Click the badge to view the detailed project board with current release-blocking issues.
+### Snapshots and Releases
 
-SNAPSHOTS, PREVIOUS RELEASES AND SOURCE CODE
---------------------------------------------
-Periodically development code snapshots are provided at the following URL:
+- **Development Snapshots**: https://github.com/HDFGroup/hdf5/releases/tag/snapshot
+- **Latest Release**: https://github.com/HDFGroup/hdf5/releases
+- **Previous Releases**: https://support.hdfgroup.org/archive/support/ftp/HDF5/releases/index.html
+- **Development Code**: https://github.com/HDFGroup/hdf5.git
 
-   https://github.com/HDFGroup/hdf5/releases/tag/snapshot
+### HPC Testing Results
 
-Source packages for current and previous releases are located at:
+[View HPC configure/build/test results on CDash](https://my.cdash.org/index.php?project=HDF5)
 
-   [Latest HDF5 release](https://github.com/HDFGroup/hdf5/releases)
-   [Previous releases](https://support.hdfgroup.org/archive/support/ftp/HDF5/releases/index.html)
+## Java Maven Artifacts
 
-Maven artifacts for Java bindings and examples are available at:
+HDF5 Java bindings and examples are available as Maven artifacts. For detailed usage instructions including dependency configuration, repository setup, and platform-specific builds, see [HDF5Examples/JAVA/README-MAVEN.md](HDF5Examples/JAVA/README-MAVEN.md).
 
-   GitHub Packages:
-   https://maven.pkg.github.com/HDFGroup/hdf5
+## Contributing
 
-   Maven Central (coming soon):
-   https://central.sonatype.com/artifact/org.hdfgroup/hdf5-java
+We welcome contributions to HDF5! Whether you're fixing bugs, adding features, or improving documentation, your help is appreciated.
 
-Java Examples Maven Integration:
-   - **org.hdfgroup:hdf5-java** - HDF5 Java bindings with platform-specific JARs (linux-x86_64, windows-x86_64, macos-x86_64, macos-aarch64)
-   - **org.hdfgroup:hdf5-java-examples** - Complete collection of Java examples (platform-independent)
-   - Cross-platform CI/CD testing and deployment
-   - Comprehensive Maven integration with automated testing
-   - See HDF5Examples/JAVA/README-MAVEN.md for complete usage instructions
+### How to Contribute
 
-Development code is available at our Github location:
+1. **Report Issues**: Use our [GitHub Issues](https://github.com/HDFGroup/hdf5/issues) to report bugs or request features
+2. **Submit Pull Requests**: Fork the repository, make your changes, and submit a PR
+3. **Join Discussions**: Participate in the [HDF Forum](https://forum.hdfgroup.org)
 
-   https://github.com/HDFGroup/hdf5.git
+For detailed contribution guidelines, please contact us through the [Help Desk](https://help.hdfgroup.org).
+
+## How to Cite HDF5
+
+If you use HDF5 in your research, please cite it. This repository includes a [`CITATION.cff`](CITATION.cff) file containing standard citation metadata.
+
+**Quick DOI:** [10.5281/zenodo.17808614](https://doi.org/10.5281/zenodo.17808614)
+
+## Build Status
+
+<details>
+<summary>Click to expand detailed build status</summary>
+
+### Continuous Integration
+
+[![HDF5 develop daily build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/daily-schedule.yml?branch=develop&label=Daily%20Build)](https://github.com/HDFGroup/hdf5/actions/workflows/daily-schedule.yml?query=branch%3Adevelop)
+[![CVE regression](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/cve.yml?branch=develop&label=CVE%20Tests)](https://github.com/HDFGroup/hdf5/actions/workflows/cve.yml?query=branch%3Adevelop)
+[![OSS-Fuzz Status](https://oss-fuzz-build-logs.storage.googleapis.com/badges/hdf5.svg)](https://oss-fuzz-build-logs.storage.googleapis.com/index.html#hdf5)
+[![Link Checker Status](https://github.com/HDFGroup/hdf5/actions/workflows/linkchecker.yml/badge.svg)](https://github.com/HDFGroup/hdf5/actions/workflows/linkchecker.yml)
+
+### Integration Testing
+
+[![HDF-EOS5 build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/hdfeos5.yml?branch=develop&label=HDF-EOS5)](https://github.com/HDFGroup/hdf5/actions/workflows/hdfeos5.yml?query=branch%3Adevelop)
+[![netCDF build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/netcdf.yml?branch=develop&label=netCDF)](https://github.com/HDFGroup/hdf5/actions/workflows/netcdf.yml?query=branch%3Adevelop)
+[![h5py build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/h5py.yml?branch=develop&label=h5py)](https://github.com/HDFGroup/hdf5/actions/workflows/h5py.yml?query=branch%3Adevelop)
+
+### VOL and VFD Testing
+
+[![HDF5 VOL connectors build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/vol.yml?branch=develop&label=VOL%20Connectors)](https://github.com/HDFGroup/hdf5/actions/workflows/vol.yml?query=branch%3Adevelop)
+[![HDF5 VFD build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/vfd.yml?branch=develop&label=VFD%20Tests)](https://github.com/HDFGroup/hdf5/actions/workflows/vfd.yml?query=branch%3Adevelop)
+
+</details>
+
+---
 
 [u1]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md
 [u2]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/img/release-schedule.png
 [u3]: https://github.com/HDFGroup/hdf5/blob/develop/doxygen/img/HDF5.png
 [u4]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs
-
diff --git a/bin/README.md b/bin/README.md
index a32eccc8309..842e1c4b126 100644
--- a/bin/README.md
+++ b/bin/README.md
@@ -9,7 +9,6 @@
 |`genparser`|Creates the flex/bison-based parser files in the high-level library|
 |`h5cc.in`|Input file from which h5cc is created|
 |`h5redeploy.in`|Input file from which h5redeploy is created|
-|`h5vers`|Updates the library version number|
 |`make_err`|Generates the H5E header files|
 |`make_vers`|Generates H5version.h|
 |`make_overflow`|Generates H5overflow.h|
diff --git a/bin/h5vers b/bin/h5vers
deleted file mode 100755
index 4ad2d193a63..00000000000
--- a/bin/h5vers
+++ /dev/null
@@ -1,538 +0,0 @@
-#! /bin/sh
-perl -x -S $0 "$@"
-exit
-
-#! perl
-require 5.003;
-use strict;
-
-# Copyright by The HDF Group.
-# All rights reserved.
-#
-# This file is part of HDF5.  The full HDF5 copyright notice, including
-# terms governing use, modification, and redistribution, is contained in
-# the LICENSE file, which can be found at the root of the source code
-# distribution tree, or in https://www.hdfgroup.org/licenses.
-# If you do not have access to either file, you may request a copy from
-# help@hdfgroup.org.
-#
-
-### Purpose
-# Increments the hdf5 version number by changing the value of
-# constants in the src/H5public.h file.  The new version number is
-# printed on the standard output. An alternate source file name can be
-# specified as an argument.  In any case, the original file is saved
-# by appending a tilde `~' to the name.
-
-### Usage:
-# h5vers [OPTIONS] [FILE]
-
-# Without options this program only displays the current version and
-# doesn't modify any files or create backups.  The default is to print
-# the version number like X.Y.Z-A where X is the major version number,
-# Y is the minor version number, Z is the release number, and A is
-# a short annotation string (the `-' is printed only if A is not empty).
-# If the `-v' switch is given the version will be printed like:
-#
-#    version X.Y release Z (A)
-#
-# The space and parentheses around A are only printed if A is not empty.
-#
-# The `-s VERSION' switch will set the version as specified.  If the
-# string contains a dotted triple then it will be used as the version
-# number, otherwise up to three numbers will be read from the end of
-# the string and used as the major version, minor version, and release
-# number.  If any numbers are missing then zero is assumed.  This
-# allows versions to be specified like `-s "version 2.1 release 8"' or
-# `-s hdf5-2.1.8.tar.bz2'.  If the new version is less than the old
-# version then a warning message is generated on standard error. The
-# annotation string, A, is set only if it appears immediately after the
-# third number, separated by a dash (e.g., `1.2.3-pre1') or in parentheses
-# (e.g., `version 1.2 release 3 (pre1)').
-#
-# The `-i [major|minor|release|annot|last]' option increments the major
-# number, minor number, release number, or annotation string. The `last'
-# switch increments the annotation string if present, otherwise the
-# release number. If the release number is incremented then the annotation
-# string is cleared.  If the minor number is incremented then the release
-# number is set to zero and the annotation string is cleared; if the major
-# number is incremented then the minor and release numbers are set to zero
-# and the annotation string is cleared.
-#
-# If a file is specified then that file is used instead of
-# ./H5public.h or ./src/H5public.h.
-#
-# If the version number is changed (either `-s' or `-i' was used on
-# the command line) then the version line of the README.md and CHANGELOG.md files
-# one directory above the H5public.h file is also modified so it looks
-# something like: This is hdf5-2.0.1-1 currently under development.
-# Version changes are also reflected in the Windows-maintained H5pubconf.h
-# file.
-#
-# Whenever the version changes, this script will increment the revision
-# field in HDF5's libtool shared library version in config/lt_vers.am,
-# which is included in src/Makefile.am.  Incrementing the revision field
-# indicates that the source code has changed since the last version
-# (which it probably has).
-##############################################################################
-
-sub getvers {
-  local ($_) = @_;
-  my (@vers);
-
-  ($vers[0]) = /^\#\s*define\s+H5_VERS_MAJOR\s+(\d+)/m;
-  ($vers[1]) = /^\#\s*define\s+H5_VERS_MINOR\s+(\d+)/m;
-  ($vers[2]) = /^\#\s*define\s+H5_VERS_RELEASE\s+(\d+)/m;
-  ($vers[3]) = /^\#\s*define\s+H5_VERS_SUBRELEASE\s+\"([^\"]*)\"/m;
-  return @vers;
-}
-
-sub setvers {
-  my ($contents, @vers) = @_;
-  $_[0] =~ s/^(\#\s*define\s+H5_VERS_MAJOR\s+)\d+/$1$vers[0]/m;
-  $_[0] =~ s/^(\#\s*define\s+H5_VERS_MINOR\s+)\d+/$1$vers[1]/m;
-  $_[0] =~ s/^(\#\s*define\s+H5_VERS_RELEASE\s+)\d+/$1$vers[2]/m;
-  $_[0] =~ s/^(\#\s*define\s+H5_VERS_SUBRELEASE\s+\")[^\"]*/$1$vers[3]/m;
-  $_[0] =~ s/^(\#\s*define\s+H5_VERS_STR\s+\")[^\"]*/
-    sprintf("%s%d.%d.%d%s%s", $1, @vers[0,1,2], $vers[3]?"-":"", $vers[3])/me;
-  $_[0] =~ s/^(\#\s*define\s+H5_VERS_INFO\s+\")[^\"]*/
-    sprintf("%sHDF5 library version: %d.%d.%d%s%s", $1, @vers[0,1,2],
-	    $vers[3]?"-":"", $vers[3])/me;
-}
-
-sub usage {
-  my ($prog) = $0 =~ /([^\/]+)$/;
-  print STDERR <<EOF;
-Usage: $prog [OPTS] [FILE]
-    -i major|minor|release|annot
-        Increment specified version component and set following components
-        to zero.
-    -s VERSION
-        Set the version as specified. The version number can be embedded in
-        some other string such as \"hdf5-1.1.0-pre1.tar.bz2\" or even
-        \"this is HDF5 library version 1.1 release 0 (pre1)\" for convenience.
-    -v
-        Instead of displaying only a dotted triple version number a line such
-        as \"version 1.1 release 0 (pre1)\" will be printed.
-    FILE
-        The name of the file that contains version information.  This is
-        seldom necessary since the file H5public.h is checked for current
-        working directory at the top level, one level down, or in the src 
-        directory where H5public.h resides.
-EOF
-  exit 1;
-}
-
-
-my ($verbose, $set, $inc, $file, $rc);
-my (@files) = ("H5public.h", "src/H5public.h", "../src/H5public.h");
-while ($_ = shift) {
-  $_ eq "-s" && do {
-    die "-s switch needs a version number\n" unless @ARGV;
-    $set = shift;
-    next;
-  };
-
-  $_ eq "-i" && do {
-    if (@ARGV && $ARGV[0]=~/^(major|minor|release|annot)$/) {
-      $inc = shift;
-    } else {
-      $inc = "last";
-    }
-    next;
-  };
-
-  $_ eq "-v" && do {
-    $verbose = 1;
-    next;
-  };
-
-  /^-(h|\?|-?help)$/ && usage;
-  /^-/ && die "unrecognized option: $_\n";
-  die "only one file name can be specified\n" if $file;
-  $file = $_;
-}
-die "mutually exclusive options given\n" if $set && $inc;
-
-# Determine file to use as H5public.h, README.md,
-# release_docs/CHANGELOG.md, windows/src/H5pubconf.h
-# config/lt_vers.am and config/cmake/scripts/HDF5config.cmake. 
-# The README.md, release_docs/CHANGELOG.md, # windows/src/H5pubconf.h,
-# config/lt_vers.am and config/cmake/scripts/HDF5config.cmake
-# files are always in the directory above H5public.h
-unless ($file) {
-  for (@files) {
-    ($file=$_,last) if -f $_;
-  }
-}
-die "unable to find source files\n" unless defined $file;
-die "unable to read file: $file\n" unless -r $file;
-# config/lt_vers.am
-my $LT_VERS = $file;
-$LT_VERS =~ s/[^\/]*$/..\/config\/lt_vers.am/;
-die "unable to read file: $LT_VERS\n" unless -r $file;
-# config/cmake/scripts/HDF5config.cmake
-my $HDF5CONFIGCMAKE = $file;
-$HDF5CONFIGCMAKE =~ s/[^\/]*$/..\/config\/cmake\/scripts\/HDF5config.cmake/;
-die "unable to read file: $HDF5CONFIGCMAKE\n" unless -r $file;
-
-my $HDF5EXCONFCMAKE = $file;
-$HDF5EXCONFCMAKE =~ s/[^\/]*$/..\/config\/examples\/HDF5AsSubdirMacros.cmake/;
-die "unable to read file: $HDF5EXCONFCMAKE\n" unless -r $file;
-# README.md
-my $README = $file;
-$README =~ s/[^\/]*$/..\/README.md/;
-die "unable to read file: $README\n" unless -r $file;
-# release_docs/CHANGELOG.md
-my $CHANGELOG = $file;
-$CHANGELOG =~ s/[^\/]*$/..\/release_docs\/CHANGELOG.md/;
-die "unable to read file: $CHANGELOG\n" unless -r $file;
-my $H5_JAVA = $file;
-$H5_JAVA =~ s/[^\/]*$/..\/java\/hdf\/hdf5lib\/H5.java/;
-die "unable to read file: $H5_JAVA\n" unless -r $file;
-my $TESTH5_JAVA = $file;
-$TESTH5_JAVA =~ s/[^\/]*$/..\/java\/test\/TestH5.java/;
-die "unable to read file: $TESTH5_JAVA\n" unless -r $file;
-my $H5_JNI_JAVA = $file;
-$H5_JNI_JAVA =~ s/[^\/]*$/..\/java\/src-jni\/hdf\/hdf5lib\/H5.java/;
-die "unable to read file: $H5_JNI_JAVA\n" unless -r $file;
-my $TESTH5_JNI_JAVA = $file;
-$TESTH5_JNI_JAVA =~ s/[^\/]*$/..\/java\/src-jni\/test\/TestH5.java/;
-die "unable to read file: $TESTH5_JNI_JAVA\n" unless -r $file;
-my $REPACK_LAYOUT_PLUGIN_VERSION = $file;
-$REPACK_LAYOUT_PLUGIN_VERSION =~ s/[^\/]*$/..\/tools\/test\/h5repack\/expected\/h5repack_layout.h5-plugin_version_test.ddl/;
-die "unable to read file: $REPACK_LAYOUT_PLUGIN_VERSION\n" unless -r $file;
-
-# Get the current version number.
-open FILE, $file or die "unable to open $file: $!\n";
-my ($contents) = join "", <FILE>;
-close FILE;
-my (@curver) = getvers $contents;
-
-# Determine the new version number.
-my @newver; #new version
-if ($set) {
-  if ($set =~ /(\d+)\.(\d+)\.(\d+)(-([\da-zA-Z]\w*))?/) {
-    @newver = ($1, $2, $3, $5);
-  } elsif ($set =~ /(\d+)\D+(\d+)\D+(\d+)(\s*\(([a-zA-Z]\w*)\))?\D*$/) {
-    @newver = ($1, $2, $3, $5);
-  } elsif ($set =~ /(\d+)\D+(\d+)\D*$/) {
-    @newver = ($1, $2, 0, "");
-  } elsif ($set =~ /(\d+)\D*$/) {
-    @newver = ($1, 0, 0, "");
-  } else {
-    die "illegal version number specified: $set\n";
-  }
-} elsif ($inc) {
-  $inc = $curver[3] eq "" ? 'release' : 'annot' if $inc eq 'last';
-  if ($inc eq "major") {
-    $newver[0] = $curver[0]+1;
-    @newver[1,2,3] = (0,0,"");
-  } elsif ($inc eq "minor") {
-    $newver[0] = $curver[0];
-    $newver[1] = $curver[1]+1;
-    @newver[2,3] = (0,"");
-  } elsif ($inc eq "release") {
-    @newver[0,1] = @curver[0,1];
-    $newver[2] = $curver[2]+1;
-    $newver[3] = "";
-  } elsif ($inc eq "annot") {
-    @newver[0,1,2] = @curver[0,1,2];
-    $newver[3] = $curver[3];
-    $newver[3] =~ s/(\d+)\D*$/$1+1/e or
-      die "Annotation \"".$newver[3]."\" cannot be incremented.\n";
-  } else {
-    die "unknown increment field: $inc\n";
-  }
-} else {
-  # Nothing to do but print result
-  $README = "";
-  $CHANGELOG = "";
-  $LT_VERS = "";       
-  $HDF5CONFIGCMAKE = "";
-  $HDF5EXCONFCMAKE = "";
-  @newver = @curver;
-}
-
-# Note if the version increased or decreased
-my $version_increased="";
-# Print a warning if the version got smaller (don't check annot field)
-if ($newver[0]*1000000 + $newver[1]*1000 + $newver[2] <
-    $curver[0]*1000000 + $curver[1]*1000 + $curver[2]) {
-  printf STDERR "Warning: version decreased from %d.%d.%d to %d.%d.%d\n",
-    @curver[0,1,2], @newver[0,1,2];
-}
-if ($newver[0]*1000000 + $newver[1]*1000 + $newver[2] >
-    $curver[0]*1000000 + $curver[1]*1000 + $curver[2]) {
-  $version_increased="true";
-}
-
-# Update the version number if it changed.
-if ($newver[0]!=$curver[0] ||
-    $newver[1]!=$curver[1] ||
-    $newver[2]!=$curver[2] ||
-    $newver[3]ne$curver[3]) {
-  setvers $contents, @newver or die "unable to set version\n";
-  rename $file, "$file~" or die "unable to save backup file\n";
-  open FILE, ">$file" or die "unable to open $file but backup saved!\n";
-  print FILE $contents;
-  close FILE;
-}
-
-# Update the libtool shared library version in src/Makefile.am if
-# the version number has increased.
-if ($LT_VERS && $version_increased) {
-  open FILE, $LT_VERS or die "$LT_VERS: $!\n";
-  my ($contentsy) = join "", <FILE>;
-  close FILE;
-
-  local($_) = $contentsy;
-
-# As of the HDF5 v1.8.16 release, h5vers should not increment
-# the LT_VERS numbers, so the next 6 lines are commented out.
-# A future version may copy the numbers to H5public.h, so this
-# section is retained for future reference.
-#  my ($lt_revision) = /^LT_VERS_REVISION\s*=\s*(\d+)/m;
-#  my $new_lt_revision = $lt_revision+1;
-#  ($contentsy) =~ s/^(LT_VERS_REVISION\s*=\s*)\d+/$1$new_lt_revision/m;
-
-#  open FILE, ">$LT_VERS" or die "$LT_VERS: $!\n";
-#  print FILE $contentsy;
-#  close FILE;
-}
-
-# Update the README.md file
-if ($README) {
-  open FILE, $README or die "$README: $!\n";
-  my @contents = <FILE>;
-  close FILE;
-  $contents[0] = sprintf("HDF5 version %d.%d.%d%s %s",
-			 @newver[0,1,2],
-			 $newver[3] eq "" ? "" : "-".$newver[3],
-			 "currently under development\n");
-  open FILE, ">$README" or die "$README: $!\n";
-  print FILE @contents;
-  close FILE;
-}
-
-# Update the release_docs/CHANGELOG.md file
-if ($CHANGELOG) {
-  open FILE, $CHANGELOG or die "$CHANGELOG: $!\n";
-  my @contents = <FILE>;
-  close FILE;
-  $contents[0] = sprintf("HDF5 version %d.%d.%d%s %s",
-			 @newver[0,1,2],
-			 $newver[3] eq "" ? "" : "-".$newver[3],
-			 "currently under development\n");
-
-  for (my $i = 0; $i < $#contents; ++$i) {
-    if ($contents[$i] =~ /^# 🔆 Executive Summary: HDF5 Version/) {
-      $contents[$i] = sprintf("# 🔆 Executive Summary: HDF5 Version %d.%d.%d\n", @newver[0,1,2]);
-      last;
-    }
-  }
-
-  open FILE, ">$CHANGELOG" or die "$CHANGELOG: $!\n";
-  print FILE @contents;
-  close FILE;
-}
-
-# Update the config/cmake/scripts/HDF5config.cmake file
-if ($HDF5CONFIGCMAKE) {
-  my $data = read_file($HDF5CONFIGCMAKE);
-#  my $sub_rel_ver_str = "";
-  my $sub_rel_ver_str = (
-     $newver[3] eq "" 
-     ? sprintf("\"%s\"", "") 
-     : sprintf("\"%s\"", "-".$newver[3])
-     );
-  my $version_string = sprintf("\"%d.%d.%d\"", @newver[0,1,2]);
-
-  $data =~ s/set \(CTEST_SOURCE_VERSION .*\)/set \(CTEST_SOURCE_VERSION $version_string\)/;
-  $data =~ s/set \(CTEST_SOURCE_VERSEXT .*\)/set \(CTEST_SOURCE_VERSEXT $sub_rel_ver_str\)/;
-
-  write_file($HDF5CONFIGCMAKE, $data);  
-}
-
-# Update the config/examples/HDF5AsSubdirMacros.cmake file
-if ($HDF5EXCONFCMAKE) {
-  my $data = read_file($HDF5EXCONFCMAKE);
-#  my $sub_rel_ver_str = "";
-  my $sub_rel_ver_str = (
-     $newver[3] eq "" 
-     ? sprintf("\"%s\"", "") 
-     : sprintf("\"%s\"", "-".$newver[3])
-     );
-  my $version_string = sprintf("\"%d.%d.%d\"", @newver[0,1,2]);
-
-  $data =~ s/set \(HDF5_VERSION .*\)/set \(HDF5_VERSION $version_string\)/;
-  $data =~ s/set \(HDF5_VERSEXT .*\)/set \(HDF5_VERSEXT $sub_rel_ver_str\)/;
-
-  write_file($HDF5EXCONFCMAKE, $data);  
-}
-
-# Update the java/hdf/hdf5lib/H5.java file
-if ($H5_JAVA) {
-  my $data = read_file($H5_JAVA);
-#  my $sub_rel_ver_str = "";
-  my $sub_rel_ver_str = (
-     $newver[3] eq ""
-     ? sprintf("\"%s\"", "")
-     : sprintf("\"%s\"", "-".$newver[3].", currently under development")
-     );
-  my $version_string1 = sprintf("%d.%d.%d", @newver[0,1,2]);
-  my $version_string2 = sprintf("%d, %d, %d", @newver[0,1,2]);
-
-  $data =~ s/\@version HDF5 .* <BR>/\@version HDF5 $version_string1 <BR>/;
-  $data =~ s/    public final static int LIB_VERSION\[\] = \{\d*,.\d*,.\d*\};/    public final static int LIB_VERSION[] = \{$version_string2\};/;
-
-  write_file($H5_JAVA, $data);
-}
-
-# Update the java/test/TestH5.java file
-if ($TESTH5_JAVA) {
-  my $data = read_file($TESTH5_JAVA);
-#  my $sub_rel_ver_str = "";
-  my $sub_rel_ver_str = (
-     $newver[3] eq ""
-     ? sprintf("\"%s\"", "")
-     : sprintf("\"%s\"", "-".$newver[3].", currently under development")
-     );
-  my $version_string1 = sprintf("%d, %d, %d", @newver[0,1,2]);
-  my $version_string2 = sprintf("int majnum = %d, minnum = %d, relnum = %d", @newver[0,1,2]);
-
-  $data =~ s/        int libversion\[\] = \{.*\};/        int libversion\[\] = \{$version_string1\};/;
-  $data =~ s/        int majnum = \d*, minnum = \d*, relnum = \d*;/        $version_string2;/;
-
-  write_file($TESTH5_JAVA, $data);
-}
-
-# Update the java/src-jni/hdf/hdf5lib/H5.java file
-if ($H5_JNI_JAVA) {
-  my $data = read_file($H5_JNI_JAVA);
-#  my $sub_rel_ver_str = "";
-  my $sub_rel_ver_str = (
-     $newver[3] eq ""
-     ? sprintf("\"%s\"", "")
-     : sprintf("\"%s\"", "-".$newver[3].", currently under development")
-     );
-  my $version_string1 = sprintf("%d.%d.%d", @newver[0,1,2]);
-  my $version_string2 = sprintf("%d, %d, %d", @newver[0,1,2]);
-
-  $data =~ s/\@version HDF5 .* <BR>/\@version HDF5 $version_string1 <BR>/;
-  $data =~ s/    public final static int LIB_VERSION\[\] = \{\d*,.\d*,.\d*\};/    public final static int LIB_VERSION[] = \{$version_string2\};/;
-
-  write_file($H5_JNI_JAVA, $data);
-}
-
-# Update the java/src-jni/test/TestH5.java file
-if ($TESTH5_JNI_JAVA) {
-  my $data = read_file($TESTH5_JNI_JAVA);
-#  my $sub_rel_ver_str = "";
-  my $sub_rel_ver_str = (
-     $newver[3] eq ""
-     ? sprintf("\"%s\"", "")
-     : sprintf("\"%s\"", "-".$newver[3].", currently under development")
-     );
-  my $version_string1 = sprintf("%d, %d, %d", @newver[0,1,2]);
-  my $version_string2 = sprintf("int majnum = %d, minnum = %d, relnum = %d", @newver[0,1,2]);
-
-  $data =~ s/        int libversion\[\] = \{.*\};/        int libversion\[\] = \{$version_string1\};/;
-  $data =~ s/        int majnum = \d*, minnum = \d*, relnum = \d*;/        $version_string2;/;
-
-  write_file($TESTH5_JNI_JAVA, $data);
-}
-
-# Update the tools/test/h5repack/expected/h5repack_layout.h5-plugin_version_test.ddl file
-if ($REPACK_LAYOUT_PLUGIN_VERSION) {
-  my $data = read_file($REPACK_LAYOUT_PLUGIN_VERSION);
-  my $version_string = sprintf("%d %d %d", @newver[0,1,2]);
-
-  $data =~ s/            PARAMS \{ 9 \d* \d* \d* \}/            PARAMS \{ 9 $version_string \}/g;
-
-  write_file($REPACK_LAYOUT_PLUGIN_VERSION, $data);
-}
-
-# helper function to read the file for updating
-# config/cmake/scripts/HDF5Config.cmake, and java files.
-# The version string in that file is not at the top, so the string replacement
-# is not for the first line, and reading/writing the entire file as one string
-# facilitates the substring replacement.
-#Presumably these will also work for resetting the version in HDF5config.cmake.
-sub read_file {
-   my ($filename) = @_;
-
-    open my $in, $filename or die "Could not open '$filename' for reading $!";
-    local $/ = undef;
-    my $all = <$in>;
-    close $in;
-
-    return $all;
-}
-
-# helper function to write the file for updating
-# config/cmake/scripts/HDF5config.cmake and java files.
-sub write_file {
-    my ($filename, $content) = @_;
-
-    open my $out, ">$filename" or die "Could not open '$filename' for writing $!";;
-    print $out $content;
-    close $out;
-
-    return;
-}
-
-sub gen_h5pubconf {
-    my ($name, $pubconf, @vers) = @_;
-
-    my $namelc = lc($name);
-    my $nameuc = uc($name);
-
-    open FILE, $pubconf or die "$pubconf: $!\n";
-    my @contents = <FILE>;
-    close FILE;
-
-    for (my $i = 0; $i < $#contents; ++$i) {
-	if ($contents[$i] =~ /\#\s*define\s+H5_PACKAGE\s+/) {
-	    $contents[$i] = "\#define H5_PACKAGE \"$namelc\"\n";
-	} elsif ($contents[$i] =~ /\#\s*define\s+H5_PACKAGE_NAME\s+/) {
-	    $contents[$i] = "\#define H5_PACKAGE_NAME \"$nameuc\"\n";
-	} elsif ($contents[$i] =~ /\#\s*define\s+H5_PACKAGE_STRING\s+/) {
-	    $contents[$i] = sprintf("\#define H5_PACKAGE_STRING \"$nameuc %d.%d.%d%s\"\n",
-				    @vers[0,1,2],
-				    $newver[3] eq "" ? "" : "-".$newver[3]);
-	} elsif ($contents[$i] =~ /\#\s*define\s+H5_PACKAGE_TARNAME\s+/) {
-	    $contents[$i] = "\#define H5_PACKAGE_TARNAME \"$namelc\"\n";
-	} elsif ($contents[$i] =~ /\#\s*define\s+H5_PACKAGE_VERSION\s+/) {
-	    $contents[$i] = sprintf("\#define H5_PACKAGE_VERSION \"%d.%d.%d%s\"\n",
-				    @vers[0,1,2],
-				    $newver[3] eq "" ? "" : "-".$newver[3]);
-	} elsif ($contents[$i] =~ /\#\s*define\s+H5_VERSION\s+/) {
-	    $contents[$i] = sprintf("\#define H5_VERSION \"%d.%d.%d%s\"\n",
-				    @vers[0,1,2],
-				    $newver[3] eq "" ? "" : "-".$newver[3]);
-	}
-    }
-
-    open FILE, ">$pubconf" or die "$pubconf: $!\n";
-    print FILE @contents;
-    close FILE;
-}
-
-# Print the new version number
-if ($verbose) {
-  printf("version %d.%d release %d%s\n", @newver[0,1,2],
-	 $newver[3] eq "" ? "" : " (".$newver[3].")");
-} else {
-  printf("%d.%d.%d%s\n", @newver[0,1,2],
-	 $newver[3] eq "" ? "" : "-".$newver[3]);
-}
-
-exit 0;
-
-# Because the first line of this file looks like a Bourne shell script, we
-# must tell XEmacs explicitly that this is really a perl script.
-#
-# Local Variables:
-# mode:perl
-# End:
diff --git a/bin/release b/bin/release
index 4c05bf78651..396ccfa2308 100755
--- a/bin/release
+++ b/bin/release
@@ -15,6 +15,46 @@
 
 # Function definitions
 #
+# Function to get version from H5public.h
+get_version()
+{
+    grep '#define H5_VERS_STR' src/H5public.h | sed 's/.*"\(.*\)".*/\1/'
+}
+
+# Function to set version in H5public.h
+# Usage: set_version "2.0.1-of20250116"
+set_version()
+{
+    if [ $# -ne 1 ]; then
+        echo "usage: set_version <version_string>"
+        return 1
+    fi
+
+    new_vers=$1
+
+    # Parse version string (format: major.minor.release or major.minor.release-subrelease)
+    major=$(echo $new_vers | sed 's/^\([0-9]*\)\..*/\1/')
+    minor=$(echo $new_vers | sed 's/^[0-9]*\.\([0-9]*\)\..*/\1/')
+    release=$(echo $new_vers | sed 's/^[0-9]*\.[0-9]*\.\([0-9]*\).*/\1/')
+    subrelease=$(echo $new_vers | sed 's/^[0-9]*\.[0-9]*\.[0-9]*-*\(.*\)/\1/')
+
+    # If subrelease is same as new_vers, there was no subrelease part
+    if [ "$subrelease" = "$new_vers" ]; then
+        subrelease=""
+    fi
+
+    # Update H5public.h
+    # Note: H5_VERS_STR and H5_VERS_INFO are automatically derived from the base version macros
+    sed -i.bak \
+        -e "s/^\(#define H5_VERS_MAJOR[[:space:]]*\)[0-9]*/\1$major/" \
+        -e "s/^\(#define H5_VERS_MINOR[[:space:]]*\)[0-9]*/\1$minor/" \
+        -e "s/^\(#define H5_VERS_RELEASE[[:space:]]*\)[0-9]*/\1$release/" \
+        -e "s/^\(#define H5_VERS_SUBRELEASE[[:space:]]*\)\".*\"/\1\"$subrelease\"/" \
+        src/H5public.h
+
+    rm -f src/H5public.h.bak
+}
+
 # Print Usage page
 USAGE()
 {
@@ -139,7 +179,7 @@ tar2zip()
 
 # Defaults
 DEST=releases
-VERS=`perl bin/h5vers`
+VERS=$(get_version)
 VERS_OLD=
 test "$VERS" || exit 1
 verbose=yes
@@ -156,7 +196,7 @@ RESTORE_VERSION()
         echo restoring version information back to $VERS_OLD
         rm -f config/lt_vers.am
         cp $tmpdir/lt_vers.am config/lt_vers.am
-        bin/h5vers -s $VERS_OLD
+        set_version $VERS_OLD
         VERS_OLD=
     fi
 }
@@ -219,10 +259,9 @@ if [ X$pmode = Xyes ]; then
     # "undo" changes to it.
     cp config/lt_vers.am $tmpdir
     # Set version information to m.n.r-of$today.
-    # (h5vers does not correctly handle just m.n.r-$today.)
     VERS=`echo $VERS | sed -e s/-.*//`-of$today
     echo Private release of $VERS
-    bin/h5vers -s $VERS
+    set_version $VERS
 fi
 
 if [ X$revmode = Xyes ]; then
@@ -236,12 +275,11 @@ if [ X$revmode = Xyes ]; then
     fi
     revision=`git rev-parse --short HEAD`
     # Set version information to m.n.r-r$revision.
-    # (h5vers does not correctly handle just m.n.r-$today.)
     VERS=`echo $VERS | sed -e s/-.*//`-$revision
     echo Private release of $VERS
     HDF5_VERS=hdf5-$BRANCHNAME-$revision
     echo file base of $HDF5_VERS
-    bin/h5vers -s $VERS
+    set_version $VERS
     # use a generic directory name for revision releases
     HDF5_IN_VERS=hdfsrc
 else
diff --git a/config/cmake/HDF5VersionParsing.cmake b/config/cmake/HDF5VersionParsing.cmake
new file mode 100644
index 00000000000..0356cd83c2b
--- /dev/null
+++ b/config/cmake/HDF5VersionParsing.cmake
@@ -0,0 +1,114 @@
+#
+# Copyright by The HDF Group.
+# All rights reserved.
+#
+# This file is part of HDF5. The full HDF5 copyright notice, including
+# terms governing use, modification, and redistribution, is contained in
+# the LICENSE file, which can be found at the root of the source code
+# distribution tree, or in https://www.hdfgroup.org/licenses.
+# If you do not have access to either file, you may request a copy from
+# help@hdfgroup.org.
+#
+
+#
+# HDF5VersionParsing.cmake
+#
+# Provides a macro to parse version information from H5public.h
+# This ensures consistent version extraction across all CMake scripts.
+#
+
+#[=======================================================================[.rst:
+HDF5VersionParsing
+------------------
+
+Provides macros for extracting HDF5 version information from H5public.h
+
+parse_hdf5_version
+^^^^^^^^^^^^^^^^^^
+
+Parses version constants from H5public.h and sets variables in parent scope.
+
+.. code-block:: cmake
+
+  parse_hdf5_version(<path_to_H5public.h>
+                     MAJOR_VAR <major_var_name>
+                     MINOR_VAR <minor_var_name>
+                     RELEASE_VAR <release_var_name>
+                     [SUBRELEASE_VAR <subrelease_var_name>])
+
+Reads the specified H5public.h file and extracts version numbers from the
+H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE, and optionally H5_VERS_SUBRELEASE
+macros. The extracted values are set in the specified variables in the parent scope.
+
+Arguments:
+  - ``<path_to_H5public.h>``: Path to the H5public.h file to parse
+  - ``MAJOR_VAR``: Variable name to store H5_VERS_MAJOR value
+  - ``MINOR_VAR``: Variable name to store H5_VERS_MINOR value
+  - ``RELEASE_VAR``: Variable name to store H5_VERS_RELEASE value
+  - ``SUBRELEASE_VAR``: (Optional) Variable name to store H5_VERS_SUBRELEASE value
+
+Example:
+  .. code-block:: cmake
+
+    include(HDF5VersionParsing)
+    parse_hdf5_version("${CMAKE_SOURCE_DIR}/src/H5public.h"
+                       MAJOR_VAR H5_VERS_MAJOR
+                       MINOR_VAR H5_VERS_MINOR
+                       RELEASE_VAR H5_VERS_RELEASE
+                       SUBRELEASE_VAR H5_VERS_SUBRELEASE)
+    message(STATUS "HDF5 Version: ${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")
+
+#]=======================================================================]
+
+macro(parse_hdf5_version H5PUBLIC_H_PATH)
+  # Parse arguments
+  set(options "")
+  set(oneValueArgs MAJOR_VAR MINOR_VAR RELEASE_VAR SUBRELEASE_VAR)
+  set(multiValueArgs "")
+  cmake_parse_arguments(PARSE_VER "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
+
+  # Validate required arguments
+  if(NOT PARSE_VER_MAJOR_VAR OR NOT PARSE_VER_MINOR_VAR OR NOT PARSE_VER_RELEASE_VAR)
+    message(FATAL_ERROR "parse_hdf5_version requires MAJOR_VAR, MINOR_VAR, and RELEASE_VAR arguments")
+  endif()
+
+  # Validate H5public.h exists
+  if(NOT EXISTS "${H5PUBLIC_H_PATH}")
+    message(FATAL_ERROR "H5public.h not found at: ${H5PUBLIC_H_PATH}")
+  endif()
+
+  # Read H5public.h
+  file(STRINGS "${H5PUBLIC_H_PATH}" _h5_vers_contents REGEX "^#define H5_VERS_(MAJOR|MINOR|RELEASE|SUBRELEASE)")
+
+  # Extract version numbers using regex
+  string(REGEX MATCH "H5_VERS_MAJOR[ \t]+([0-9]+)" _match "${_h5_vers_contents}")
+  if(NOT CMAKE_MATCH_1)
+    message(FATAL_ERROR "Failed to parse H5_VERS_MAJOR from ${H5PUBLIC_H_PATH}")
+  endif()
+  set(${PARSE_VER_MAJOR_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)
+
+  string(REGEX MATCH "H5_VERS_MINOR[ \t]+([0-9]+)" _match "${_h5_vers_contents}")
+  if(NOT CMAKE_MATCH_1)
+    message(FATAL_ERROR "Failed to parse H5_VERS_MINOR from ${H5PUBLIC_H_PATH}")
+  endif()
+  set(${PARSE_VER_MINOR_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)
+
+  string(REGEX MATCH "H5_VERS_RELEASE[ \t]+([0-9]+)" _match "${_h5_vers_contents}")
+  if(NOT CMAKE_MATCH_1)
+    message(FATAL_ERROR "Failed to parse H5_VERS_RELEASE from ${H5PUBLIC_H_PATH}")
+  endif()
+  set(${PARSE_VER_RELEASE_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)
+
+  # Extract subrelease if requested
+  if(PARSE_VER_SUBRELEASE_VAR)
+    string(REGEX MATCH "H5_VERS_SUBRELEASE[ \t]+\"([^\"]*)\"" _match "${_h5_vers_contents}")
+    if(NOT CMAKE_MATCH_1)
+      message(FATAL_ERROR "Failed to parse H5_VERS_SUBRELEASE from ${H5PUBLIC_H_PATH}")
+    endif()
+    set(${PARSE_VER_SUBRELEASE_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)
+  endif()
+
+  # Clean up temporary variables
+  unset(_h5_vers_contents)
+  unset(_match)
+endmacro()
diff --git a/config/cmake/scripts/HDF5config.cmake b/config/cmake/scripts/HDF5config.cmake
index 5df69b3d361..30ec71818e4 100644
--- a/config/cmake/scripts/HDF5config.cmake
+++ b/config/cmake/scripts/HDF5config.cmake
@@ -38,8 +38,26 @@ cmake_minimum_required (VERSION 3.26)
 #     CTEST_SOURCE_NAME  -  source folder
 ##############################################################################
 
-set (CTEST_SOURCE_VERSION "2.0.1")
-set (CTEST_SOURCE_VERSEXT "")
+#-----------------------------------------------------------------------------
+# Version is extracted from H5public.h at configure time
+# If not set in parent scope, read it from H5public.h now
+#-----------------------------------------------------------------------------
+if (NOT DEFINED H5_VERS_MAJOR)
+  # Use shared version parsing module
+  include(${CTEST_SCRIPT_DIRECTORY}/../HDF5VersionParsing.cmake)
+  parse_hdf5_version("${CTEST_SCRIPT_DIRECTORY}/../src/H5public.h"
+                     MAJOR_VAR H5_VERS_MAJOR
+                     MINOR_VAR H5_VERS_MINOR
+                     RELEASE_VAR H5_VERS_RELEASE
+                     SUBRELEASE_VAR H5_VERS_SUBRELEASE)
+endif ()
+
+set (CTEST_SOURCE_VERSION "${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")
+if (H5_VERS_SUBRELEASE)
+  set (CTEST_SOURCE_VERSEXT "-${H5_VERS_SUBRELEASE}")
+else ()
+  set (CTEST_SOURCE_VERSEXT "")
+endif ()
 
 ##############################################################################
 # handle input parameters to script.
diff --git a/config/examples/HDF5AsSubdirMacros.cmake b/config/examples/HDF5AsSubdirMacros.cmake
index 62e46c547ac..d03d08773a4 100644
--- a/config/examples/HDF5AsSubdirMacros.cmake
+++ b/config/examples/HDF5AsSubdirMacros.cmake
@@ -17,9 +17,27 @@
 # and build it.  The HDF5 options should be set after the FetchContent_Declare command and before
 # the add_subdirectory command..
 macro (EXTERNAL_HDF5_LIBRARY compress_type)
-  set (HDF5_VERSION "2.0.1")
-  set (HDF5_VERSEXT "")
-  set (HDF5_VERSION_MAJOR "2.0")
+  #-----------------------------------------------------------------------------
+  # Version is extracted from H5public.h
+  # If not set in parent scope, read it from H5public.h now
+  #-----------------------------------------------------------------------------
+  if (NOT DEFINED H5_VERS_MAJOR)
+    # Use shared version parsing module
+    include(${CMAKE_CURRENT_LIST_DIR}/../cmake/HDF5VersionParsing.cmake)
+    parse_hdf5_version("${CMAKE_CURRENT_LIST_DIR}/../../src/H5public.h"
+                       MAJOR_VAR H5_VERS_MAJOR
+                       MINOR_VAR H5_VERS_MINOR
+                       RELEASE_VAR H5_VERS_RELEASE
+                       SUBRELEASE_VAR H5_VERS_SUBRELEASE)
+  endif ()
+
+  set (HDF5_VERSION "${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")
+  if (H5_VERS_SUBRELEASE)
+    set (HDF5_VERSEXT "-${H5_VERS_SUBRELEASE}")
+  else ()
+    set (HDF5_VERSEXT "")
+  endif ()
+  set (HDF5_VERSION_MAJOR "${H5_VERS_MAJOR}.${H5_VERS_MINOR}")
   set (HDF5LIB_TGZ_NAME "hdf5.tar.gz" CACHE STRING "Use HDF5LIB from compressed file" FORCE)
   set (HDF5LIB_TGZ_ORIGPATH "https://github.com/HDFGroup/hdf5/releases/download/snapshot" CACHE STRING "Use HDF5LIB from original location" FORCE)
   set (HDF5LIB_USE_LOCALCONTENT ON CACHE BOOL "Use local file for HDF5LIB FetchContent" FORCE)
diff --git a/config/lt_vers.am b/config/lt_vers.am
index f2674c0fc90..0a05473dca7 100644
--- a/config/lt_vers.am
+++ b/config/lt_vers.am
@@ -27,9 +27,8 @@ LT_VERS_REVISION = 0
 ## version.
 ##
 ## 4. If the source changes but there are no API changes, increment
-## LT_VERS_REVISION.  This will happen automatically when
-## bin/h5vers is run, but doing it manually shouldn't hurt
-## anything.
+## LT_VERS_REVISION. This should be done manually when updating the
+## version in src/H5public.h.
 ##
 ## Note that this versioning system doesn't attempt to handle
 ## the effects of the H5_V1_x_COMPAT flag.
diff --git a/doxygen/Doxyfile.in b/doxygen/Doxyfile.in
index 0551a1452c1..467663c4633 100644
--- a/doxygen/Doxyfile.in
+++ b/doxygen/Doxyfile.in
@@ -804,6 +804,7 @@ EXCLUDE_PATTERNS       = */fortran/test/* \
                          */hl/fortran/src/*.h \
                          */HDF5Examples/FORTRAN/* \
                          */sanitizer/* \
+                         */README.md \
                          */CONTRIBUTING.md \
                          */CHANGELOG.md \
                          */HISTORY-*.md
diff --git a/java/hdf/hdf5lib/CMakeLists.txt b/java/hdf/hdf5lib/CMakeLists.txt
index 76e0f7ddc44..87a2a2d7e0b 100644
--- a/java/hdf/hdf5lib/CMakeLists.txt
+++ b/java/hdf/hdf5lib/CMakeLists.txt
@@ -102,11 +102,24 @@ set (HDF5_JAVADOC_HDF_HDF5_STRUCTS_SOURCES
     structs/package-info.java
 )
 
+#-----------------------------------------------------------------------------
+# Generate H5Version.java with version information from H5public.h
+# This ensures the Java version constants match the C library version
+# Uses shared template from java/templates/
+#-----------------------------------------------------------------------------
+configure_file (
+    ${HDF5_SOURCE_DIR}/java/templates/H5Version.java.in
+    ${CMAKE_CURRENT_BINARY_DIR}/H5Version.java
+    @ONLY
+)
+
+# Use sources from both source and binary directories
 set (HDF5_JAVA_HDF_HDF5_SOURCES
     HDFArray.java
     HDF5Constants.java
     HDFNativeData.java
     H5.java
+    ${CMAKE_CURRENT_BINARY_DIR}/H5Version.java
     VLDataConverter.java
 )
 
diff --git a/java/hdf/hdf5lib/H5.java b/java/hdf/hdf5lib/H5.java
index 317f8414bd6..233cfd67c87 100644
--- a/java/hdf/hdf5lib/H5.java
+++ b/java/hdf/hdf5lib/H5.java
@@ -296,9 +296,11 @@ public class H5 implements java.io.Serializable {
      * <li>LIB_VERSION[1]: The minor version of the library.</li>
      * <li>LIB_VERSION[2]: The release number of the library.</li>
      * </ul>
-     * Make sure to update the versions number when a different library is used.
+     * NOTE: This version is automatically synchronized with H5public.h via the auto-generated
+     *       H5Version class. To update the version, edit src/H5public.h (H5_VERS_MAJOR,
+     *       H5_VERS_MINOR, H5_VERS_RELEASE). Do NOT manually edit the version numbers.
      */
-    public final static int LIB_VERSION[] = {2, 0, 1};
+    public final static int LIB_VERSION[] = {H5Version.MAJOR, H5Version.MINOR, H5Version.RELEASE};
 
     private final static LinkedHashSet<Long> OPEN_IDS = new LinkedHashSet<Long>();
     private static boolean isLibraryLoaded            = false;
diff --git a/java/src-jni/hdf/hdf5lib/CMakeLists.txt b/java/src-jni/hdf/hdf5lib/CMakeLists.txt
index 33889754889..4aac46026f2 100644
--- a/java/src-jni/hdf/hdf5lib/CMakeLists.txt
+++ b/java/src-jni/hdf/hdf5lib/CMakeLists.txt
@@ -103,11 +103,24 @@ set (HDF5_JAVADOC_HDF_HDF5_STRUCTS_SOURCES
     structs/package-info.java
 )
 
+#-----------------------------------------------------------------------------
+# Generate H5Version.java with version information from H5public.h
+# This ensures the Java version constants match the C library version
+# Uses shared template from java/templates/
+#-----------------------------------------------------------------------------
+configure_file (
+    ${HDF5_SOURCE_DIR}/java/templates/H5Version.java.in
+    ${CMAKE_CURRENT_BINARY_DIR}/H5Version.java
+    @ONLY
+)
+
+# Use sources from both source and binary directories
 set (HDF5_JAVA_HDF_HDF5_SOURCES
     HDFArray.java
     HDF5Constants.java
     HDFNativeData.java
     H5.java
+    ${CMAKE_CURRENT_BINARY_DIR}/H5Version.java
 )
 
 set (HDF5_JAVADOC_HDF_HDF5_SOURCES
diff --git a/java/src-jni/hdf/hdf5lib/H5.java b/java/src-jni/hdf/hdf5lib/H5.java
index d22f103ef8a..3a70628f12c 100644
--- a/java/src-jni/hdf/hdf5lib/H5.java
+++ b/java/src-jni/hdf/hdf5lib/H5.java
@@ -271,9 +271,11 @@ public class H5 implements java.io.Serializable {
      * <li>LIB_VERSION[1]: The minor version of the library.</li>
      * <li>LIB_VERSION[2]: The release number of the library.</li>
      * </ul>
-     * Make sure to update the versions number when a different library is used.
+     * NOTE: This version is automatically synchronized with H5public.h via the auto-generated
+     *       H5Version class. To update the version, edit src/H5public.h (H5_VERS_MAJOR,
+     *       H5_VERS_MINOR, H5_VERS_RELEASE). Do NOT manually edit the version numbers.
      */
-    public final static int LIB_VERSION[] = {2, 0, 1};
+    public final static int LIB_VERSION[] = {H5Version.MAJOR, H5Version.MINOR, H5Version.RELEASE};
 
     /**
      * @ingroup JH5
diff --git a/java/templates/H5Version.java.in b/java/templates/H5Version.java.in
new file mode 100644
index 00000000000..f0b45892fb9
--- /dev/null
+++ b/java/templates/H5Version.java.in
@@ -0,0 +1,38 @@
+/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
+ * Copyright by The HDF Group.                                               *
+ * All rights reserved.                                                      *
+ *                                                                           *
+ * This file is part of HDF5.  The full HDF5 copyright notice, including     *
+ * terms governing use, modification, and redistribution, is contained in    *
+ * the LICENSE file, which can be found at the root of the source code       *
+ * distribution tree, or in https://www.hdfgroup.org/licenses.               *
+ * If you do not have access to either file, you may request a copy from     *
+ * help@hdfgroup.org.                                                        *
+ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
+
+package hdf.hdf5lib;
+
+/**
+ * HDF5 library version information.
+ *
+ * This file is automatically generated from src/H5public.h by CMake.
+ * DO NOT EDIT THIS FILE DIRECTLY - it will be overwritten during the build.
+ *
+ * To update the version, edit src/H5public.h and modify:
+ *   H5_VERS_MAJOR
+ *   H5_VERS_MINOR
+ *   H5_VERS_RELEASE
+ */
+public class H5Version {
+    /** Major version number */
+    public static final int MAJOR = @H5_VERS_MAJOR@;
+
+    /** Minor version number */
+    public static final int MINOR = @H5_VERS_MINOR@;
+
+    /** Release number */
+    public static final int RELEASE = @H5_VERS_RELEASE@;
+
+    /** Version string */
+    public static final String VERSION = "@H5_VERS_MAJOR@.@H5_VERS_MINOR@.@H5_VERS_RELEASE@";
+}
diff --git a/release_docs/AutotoolsToCMakeOptions.md b/release_docs/AutotoolsToCMakeOptions.md
index 7b3f4130709..de694e88e61 100644
--- a/release_docs/AutotoolsToCMakeOptions.md
+++ b/release_docs/AutotoolsToCMakeOptions.md
@@ -1,4 +1,4 @@
-# <img src="Cmake_logo.svg" alt="Cmake logo" width=24> CMake Installations
+# CMake Installations
 
 CMake produces the following set of folders; bin, include, lib and share. The LICENSE and CHANGELOG.md file are placed in the share folder.
 
diff --git a/release_docs/CHANGELOG.md b/release_docs/CHANGELOG.md
index 9c65fe4ee46..471d2e5ff1e 100644
--- a/release_docs/CHANGELOG.md
+++ b/release_docs/CHANGELOG.md
@@ -1,4 +1,6 @@
-HDF5 version 2.0.1 currently under development
+***
+
+v2.0.0 --- November 10 , 2025
 
 # 🔺 HDF5 Changelog
 All notable changes to this project will be documented in this file. This document describes the differences between this release and the previous
diff --git a/release_docs/Cmake_logo.svg b/release_docs/Cmake_logo.svg
deleted file mode 100644
index 42b7be8e518..00000000000
--- a/release_docs/Cmake_logo.svg
+++ /dev/null
@@ -1,70 +0,0 @@
-<?xml version="1.0" encoding="UTF-8" standalone="no"?>
-<!-- Generator: Adobe Illustrator 22.1.0, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
-
-<svg
-   xmlns:dc="http://purl.org/dc/elements/1.1/"
-   xmlns:cc="http://creativecommons.org/ns#"
-   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
-   xmlns:svg="http://www.w3.org/2000/svg"
-   xmlns="http://www.w3.org/2000/svg"
-   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
-   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
-   x="0px"
-   y="0px"
-   viewBox="0 0 64 63.999998"
-   xml:space="preserve"
-   version="1.1"
-   id="svg39"
-   sodipodi:docname="cmake-icon.svg"
-   width="64"
-   height="64"
-   inkscape:version="0.92.4 (5da689c313, 2019-01-14)"><metadata
-   id="metadata45"><rdf:RDF><cc:Work
-       rdf:about=""><dc:format>image/svg+xml</dc:format><dc:type
-         rdf:resource="http://purl.org/dc/dcmitype/StillImage" /><dc:title></dc:title></cc:Work></rdf:RDF></metadata><defs
-   id="defs43">                </defs><sodipodi:namedview
-   pagecolor="#ffffff"
-   bordercolor="#666666"
-   borderopacity="1"
-   objecttolerance="10"
-   gridtolerance="10"
-   guidetolerance="10"
-   inkscape:pageopacity="0"
-   inkscape:pageshadow="2"
-   inkscape:window-width="1920"
-   inkscape:window-height="1001"
-   id="namedview41"
-   showgrid="false"
-   inkscape:zoom="2.4266285"
-   inkscape:cx="69.849998"
-   inkscape:cy="22.700001"
-   inkscape:window-x="-9"
-   inkscape:window-y="-9"
-   inkscape:window-maximized="1"
-   inkscape:current-layer="svg39" /> <style
-   type="text/css"
-   id="style2"> .st0{fill:#064F8C;} .st1{fill:#249847;} .st2{fill:#BE2128;} .st3{fill:#172C36;} .st4{fill:#CDCDCE;} </style> <g
-   id="Layer_2"
-   transform="translate(0,18.599998)"> </g> <g
-   id="g922"
-   transform="matrix(2.2190473,0,0,2.2190473,-14.603456,-34.571419)"><polygon
-     transform="matrix(0.79452357,0,0,0.79452357,3.8001251,12.63966)"
-     class="st0"
-     points="21.3,3.8 3.6,38.8 22.9,22.4 "
-     id="polygon6"
-     style="fill:#064f8c" /><polygon
-     transform="matrix(0.79452357,0,0,0.79452357,3.8001251,12.63966)"
-     class="st1"
-     points="38.6,39.9 14.8,30.3 3.5,39.9 "
-     id="polygon8"
-     style="fill:#249847" /><polygon
-     transform="matrix(0.79452357,0,0,0.79452357,3.8001251,12.63966)"
-     class="st2"
-     points="39.8,39.6 22.1,4.4 24.7,33.5 "
-     id="polygon10"
-     style="fill:#be2128" /><polygon
-     transform="matrix(0.79452357,0,0,0.79452357,3.8001251,12.63966)"
-     style="fill:#cdcdce"
-     id="polygon34"
-     points="23.9,33.2 23,23.3 15.4,29.8 "
-     class="st4" /></g> </svg>
\ No newline at end of file
diff --git a/release_docs/README_HPC.md b/release_docs/README_HPC.md
index 10de30201be..40b7b6913c4 100644
--- a/release_docs/README_HPC.md
+++ b/release_docs/README_HPC.md
@@ -79,7 +79,7 @@ For release or snapshot tar files, extract them to your working directory.
 
 > **Note:** When using the ctest automated build method (Section 4), the source
 > directory should be named `hdf5-<version>`, where version uses format `1.xx.xx`
-> or `2.xx.xx`. Use `bin/h5vers` to determine the version string if needed.
+> or `2.xx.xx`. Check `H5_VERS_STR` in `src/H5public.h` to determine the version string if needed.
 
 ---
 
diff --git a/release_docs/RELEASE_PROCESS.md b/release_docs/RELEASE_PROCESS.md
index a263deffee2..94008cba4ab 100644
--- a/release_docs/RELEASE_PROCESS.md
+++ b/release_docs/RELEASE_PROCESS.md
@@ -86,15 +86,15 @@ For more information on the HDF5 versioning and backward and forward compatibili
     - or create the new branch in GitHub GUI.
 4. Check that required CMake files point to the specific versions of the third-party software (szip, zlib and plugins) that they depend on.
     - Update as needed.
-5. Change the **support** branch to X.Y.{Z+1}-1 (\<dash>1) using the [bin/h5vers][u10] script: 
+5. Change the **support** branch to X.Y.{Z+1}-1 (\<dash>1) by editing [src/H5public.h][u11]:
     - `$ git checkout hdf5_X_Y`
-    - `$ bin/h5vers -s X.Y.{Z+1}-1;`
+    - Edit `src/H5public.h` to update version defines: `H5_VERS_MAJOR`, `H5_VERS_MINOR`, `H5_VERS_RELEASE`, `H5_VERS_SUBRELEASE`, `H5_VERS_STR`, and `H5_VERS_INFO`
     - `$ git commit -m "Updated support branch version number to X.Y.{Z+1}-1"`
     - `$ git push`
-6. Change the **release preparation branch**'s version number to X.Y.Z.1 using the [bin/h5vers][u10]/bin/h5vers script: 
-    - `$ git checkout hdf5_X_Y_Z;` 
-    - `$ bin/h5vers -s X.Y.Z.1;` 
-    - `$ git commit -m "Updated release preparation branch version number to X.Y.Z.1"` 
+6. Change the **release preparation branch**'s version number to X.Y.Z.1 by editing [src/H5public.h][u11]:
+    - `$ git checkout hdf5_X_Y_Z;`
+    - Edit `src/H5public.h` to update version defines: `H5_VERS_MAJOR`, `H5_VERS_MINOR`, `H5_VERS_RELEASE`, `H5_VERS_SUBRELEASE`, `H5_VERS_STR`, and `H5_VERS_INFO`
+    - `$ git commit -m "Updated release preparation branch version number to X.Y.Z.1"`
     - `$ git push` 
 7. ** OBSOLETE CURRENTLY **
    Update default configuration mode
@@ -178,8 +178,8 @@ For more information on the HDF5 versioning and backward and forward compatibili
 3. Update Release Notes in **release** branch (Release Manager)
 
 ### 10. Package and Distribute Release (Release Manager)
-1. h5vers could run genparser, which can change the generated files if certain code files have been changed since the files generated by genparser were committed on the release branch.  This should be checked by running `git status --ignored;`, then running genparser, then repeating `git status --ignored;`. If there are modified files from either git status command, they should be committed (or deleted if there are backup files or an autom4te.cache directory), and at least minimal testing should be done to see that the software is still good with the changes. 
-2. Set version for release, removing the subrelease string, initially `$ bin/h5vers -s X.Y.Z;`. Any subsequent patch releases will need the subrelease number.
+1. Running genparser can change the generated files if certain code files have been changed since the files generated by genparser were committed on the release branch. This should be checked by running `git status --ignored;`, then running genparser, then repeating `git status --ignored;`. If there are modified files from either git status command, they should be committed (or deleted if there are backup files or an autom4te.cache directory), and at least minimal testing should be done to see that the software is still good with the changes.
+2. Set version for release by editing `src/H5public.h`, removing the subrelease string (set `H5_VERS_SUBRELEASE` to `""`). Update `H5_VERS_STR` to `"X.Y.Z"` and `H5_VERS_INFO` to `"HDF5 library version: X.Y.Z"`. Any subsequent patch releases will need the subrelease number.
 3. Run `bin/release` (similar to 8.2) and commit all the changed files.
 4. Select the actions tab and the release build workflow, then click the 'Run workflow' drop-down.
     - Choose the release branch
@@ -224,8 +224,7 @@ For more information on the HDF5 versioning and backward and forward compatibili
 [u7]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/INSTALL_CMake.txt
 [u8]: https://github.com/HDFGroup/hdf5/blob/develop/.github/workflows/release.yml
 [u9]: https://github.com/HDFGroup/hdf5/blob/develop/config/lt_vers.am
-[u10]: https://github.com/HDFGroup/hdf5/blob/develop/bin/h5vers
-[u11]: https://github.com/HDFGroup/hdf5/blob/develop/src/CMakeLists.txt
+[u11]: https://github.com/HDFGroup/hdf5/blob/develop/src/H5public.h
 [u12]: https://github.com/HDFGroup/hdf5/blob/develop/configure.ac
 [u13]: https://support.hdfgroup.org/documentation/hdf5/latest/api-compat-macros.html
 [u14]: https://github.com/HDFGroup/hdf5/releases/tag/snapshot-1.14
diff --git a/release_docs/img/release-schedule.plantuml b/release_docs/img/release-schedule.plantuml
index 17c1c009a2a..d930f85362f 100644
--- a/release_docs/img/release-schedule.plantuml
+++ b/release_docs/img/release-schedule.plantuml
@@ -10,37 +10,36 @@ projectscale monthly
 Project starts 2023-01-01
 
 [1.8] starts 2023-01-01 and lasts 5 weeks
-[1.8.23] happens 2023-01-31
+[1.8.23 (EOL)] happens 2023-01-31
 [1.8] is colored in #F76969
 
 [1.10] starts 2023-01-01 and lasts 39 weeks
 [1.10.10] happens 2023-03-31
-[1.10.11] happens 2023-09-30
-[1.10.11] displays on same row as [1.10.10]
+[1.10.11 (EOL)] happens 2023-09-30
+[1.10.11 (EOL)] displays on same row as [1.10.10]
 [1.10] is colored in #F6DD60
 
 [1.12] starts 2023-01-01 and lasts 48 weeks
-[1.12.3] happens 2023-11-30
+[1.12.3 (EOL)] happens 2023-11-30
 [1.12] is colored in #88CCEE
 
 [1.14] starts at 2023-01-01 and lasts 110 weeks
 [1.14.1] happens at 2023-04-30
 [1.14.2] happens at 2023-08-31
 [1.14.3] happens at 2023-10-31
-[1.14.4.2] happens at 2024-04-15
-[1.14.4.3] happens at 2024-05-22
 [1.14.5] happens at 2024-09-30
-[1.14.6] happens at 2025-01-30
+[1.14.6 (EOL)] happens at 2025-01-30
 [1.14.1] displays on same row as [1.14.1]
 [1.14.2] displays on same row as [1.14.1]
 [1.14.3] displays on same row as [1.14.1]
-[1.14.4.2] displays on same row as [1.14.1]
-[1.14.5] displays on same row as [1.14.1]
-[1.14.6] displays on same row as [1.14.1]
+[1.14.5] displays on same row as [1.14.1] 
+[1.14.6 (EOL)] displays on same row as [1.14.1]
 [1.14] is colored in #B187CF
 
-[2.0] starts at 2024-09-30 and lasts 64 weeks
+[2.0 Active] starts at 2024-09-30 and lasts 85 weeks
 [2.0.0] happens at 2025-09-30
-[2.0] is colored in #02BFA0
+[2.1.0] happens at 2026-1-30
+[2.1.0] displays on same row as [2.0.0]
+[2.0 Active] is colored in #02BFA0
 
 @endgantt
\ No newline at end of file
diff --git a/release_docs/img/release-schedule.png b/release_docs/img/release-schedule.png
index 3c7d5290aaa..a8740b1d063 100644
Binary files a/release_docs/img/release-schedule.png and b/release_docs/img/release-schedule.png differ
diff --git a/src/H5public.h b/src/H5public.h
index 9a28f69e11c..c3232e464a3 100644
--- a/src/H5public.h
+++ b/src/H5public.h
@@ -65,23 +65,47 @@
 /**
  * For minor interface/format changes
  */
-#define H5_VERS_MINOR 0
+#define H5_VERS_MINOR 1
 /**
  * For tweaks, bug-fixes, or development
  */
-#define H5_VERS_RELEASE 1
+#define H5_VERS_RELEASE 0
 /**
  * For pre-releases like \c snap0. Empty string for official releases.
  */
+/*
+ * IMPORTANT: This MUST be a string literal (quoted), not an unquoted value.
+ *
+ *   Valid:   #define H5_VERS_SUBRELEASE ""
+ *   Valid:   #define H5_VERS_SUBRELEASE "-snap0"
+ *   Invalid: #define H5_VERS_SUBRELEASE -snap0
+ *
+ */
 #define H5_VERS_SUBRELEASE ""
+
+/* Derived version strings - automatically generated from the above */
 /**
- * Short version string
+ * Short version string - automatically derived from H5_VERS_MAJOR/MINOR/RELEASE/SUBRELEASE
+ *
+ * This macro uses C preprocessor string concatenation. The H5_VERS_MAJOR, H5_VERS_MINOR,
+ * and H5_VERS_RELEASE values are stringified and concatenated with dots, then concatenated
+ * with H5_VERS_SUBRELEASE (which must already be a string literal).
  */
-#define H5_VERS_STR "2.0.1"
+#define H5_VERS_STR_HELPER(major, minor, release)      #major "." #minor "." #release
+#define H5_VERS_STR_CONCAT(major, minor, release, sub) H5_VERS_STR_HELPER(major, minor, release) sub
+#define H5_VERS_STR                                    H5_VERS_STR_CONCAT(H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE, H5_VERS_SUBRELEASE)
+
+/*
+ * Compile-time check: Ensure H5_VERS_SUBRELEASE is a string literal.
+ * If H5_VERS_SUBRELEASE is not properly quoted (e.g., -snap0 instead of "-snap0"),
+ * this line will cause a compilation error, catching the mistake early.
+ */
+typedef char H5_vers_subrelease_must_be_quoted_string_literal[sizeof("" H5_VERS_SUBRELEASE) > 0 ? 1 : -1];
+
 /**
- * Full version string
+ * Full version string - automatically derived from H5_VERS_STR
  */
-#define H5_VERS_INFO "HDF5 library version: 2.0.1"
+#define H5_VERS_INFO "HDF5 library version: " H5_VERS_STR
 
 #define H5check() H5check_version(H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE)
 
