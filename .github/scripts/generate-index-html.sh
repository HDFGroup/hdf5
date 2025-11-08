#!/bin/bash
# Generate index.html files for HDF5 release directories
# Usage: generate-index-html.sh <directory> <title> <description> [parent_url]

set -euo pipefail

DIRECTORY="${1:-}"
TITLE="${2:-Index}"
DESCRIPTION="${3:-}"
PARENT_URL="${4:-../}"

if [ -z "$DIRECTORY" ]; then
    echo "Usage: $0 <directory> <title> <description> [parent_url]"
    exit 1
fi

if [ ! -d "$DIRECTORY" ]; then
    echo "Error: Directory '$DIRECTORY' does not exist"
    exit 1
fi

OUTPUT_FILE="$DIRECTORY/index.html"

# Get file information with size and modification time
generate_file_list() {
    local dir="$1"
    local files=()

    # Find all files and directories (excluding index.html itself)
    while IFS= read -r -d '' item; do
        if [ "$(basename "$item")" != "index.html" ]; then
            files+=("$item")
        fi
    done < <(find "$dir" -maxdepth 1 ! -path "$dir" -print0 | sort -z)

    # Generate HTML list items
    for item in "${files[@]}"; do
        local name=$(basename "$item")
        local rel_path="$name"
        local size=""
        local modified=""
        local item_type="file"

        if [ -d "$item" ]; then
            item_type="dir"
            rel_path="$name/"
            # Count items in directory
            local count=$(find "$item" -maxdepth 1 ! -path "$item" | wc -l)
            size="$count items"
        else
            # Get file size in human-readable format
            size=$(ls -lh "$item" | awk '{print $5}')

            # Get modification time
            if [[ "$OSTYPE" == "darwin"* ]]; then
                modified=$(stat -f "%Sm" -t "%Y-%m-%d %H:%M" "$item")
            else
                modified=$(stat -c "%y" "$item" | cut -d'.' -f1)
            fi
        fi

        # Determine file description based on extension
        local desc=""
        case "$name" in
            *.tar.gz|*.tgz)
                desc="Source tarball"
                ;;
            *.zip)
                if [[ "$name" == *"doxygen"* ]]; then
                    desc="Doxygen documentation"
                elif [[ "$name" == *"win"* ]] || [[ "$name" == *"WIN"* ]]; then
                    desc="Windows binary package"
                else
                    desc="Source archive"
                fi
                ;;
            *.msi)
                desc="Windows installer"
                ;;
            *.exe)
                desc="Windows executable installer"
                ;;
            *.dmg)
                desc="macOS disk image"
                ;;
            *.deb)
                desc="Debian/Ubuntu package"
                ;;
            *.rpm)
                desc="Red Hat/Fedora package"
                ;;
            *abi.reports*)
                desc="ABI compatibility reports"
                ;;
            SHA256*)
                desc="SHA256 checksums"
                ;;
            downloads)
                desc="Release binaries and source code"
                ;;
            documentation)
                desc="API documentation and user guides"
                ;;
            doxygen)
                desc="Doxygen API documentation"
                ;;
            compat_report)
                desc="ABI/API compatibility reports"
                ;;
            *)
                if [ "$item_type" == "dir" ]; then
                    desc="Directory"
                else
                    desc="File"
                fi
                ;;
        esac

        # Output HTML row
        if [ "$item_type" == "dir" ]; then
            echo "      <tr class='dir'>"
            echo "        <td class='name'><a href='$rel_path'>📁 $name/</a></td>"
            echo "        <td class='size'>$size</td>"
            echo "        <td class='modified'>-</td>"
            echo "        <td class='description'>$desc</td>"
            echo "      </tr>"
        else
            echo "      <tr class='file'>"
            echo "        <td class='name'><a href='$rel_path'>📄 $name</a></td>"
            echo "        <td class='size'>$size</td>"
            echo "        <td class='modified'>$modified</td>"
            echo "        <td class='description'>$desc</td>"
            echo "      </tr>"
        fi
    done
}

# Generate the index.html file
cat > "$OUTPUT_FILE" << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>INDEX_TITLE_PLACEHOLDER</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            background: #f5f5f5;
            padding: 20px;
        }

        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        header {
            border-bottom: 3px solid #003d7a;
            padding-bottom: 20px;
            margin-bottom: 30px;
        }

        h1 {
            color: #003d7a;
            font-size: 28px;
            margin-bottom: 10px;
        }

        .subtitle {
            color: #666;
            font-size: 16px;
            margin-bottom: 10px;
        }

        .description {
            color: #555;
            font-size: 14px;
            font-style: italic;
        }

        .breadcrumb {
            margin-bottom: 20px;
            font-size: 14px;
        }

        .breadcrumb a {
            color: #0066cc;
            text-decoration: none;
        }

        .breadcrumb a:hover {
            text-decoration: underline;
        }

        .file-list {
            width: 100%;
            border-collapse: collapse;
            margin-top: 20px;
        }

        .file-list thead {
            background: #f0f0f0;
            border-bottom: 2px solid #003d7a;
        }

        .file-list th {
            text-align: left;
            padding: 12px;
            font-weight: 600;
            color: #003d7a;
        }

        .file-list td {
            padding: 10px 12px;
            border-bottom: 1px solid #eee;
        }

        .file-list tr:hover {
            background: #f8f9fa;
        }

        .file-list tr.dir {
            background: #f9f9ff;
        }

        .file-list tr.dir:hover {
            background: #e8e8ff;
        }

        .file-list a {
            color: #0066cc;
            text-decoration: none;
            font-weight: 500;
        }

        .file-list a:hover {
            text-decoration: underline;
        }

        .name {
            min-width: 300px;
        }

        .size {
            width: 100px;
            text-align: right;
            font-family: monospace;
            color: #666;
        }

        .modified {
            width: 180px;
            font-family: monospace;
            font-size: 13px;
            color: #666;
        }

        .description {
            color: #777;
            font-size: 13px;
        }

        footer {
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            text-align: center;
            color: #666;
            font-size: 13px;
        }

        footer a {
            color: #0066cc;
            text-decoration: none;
        }

        footer a:hover {
            text-decoration: underline;
        }

        .empty-message {
            padding: 40px;
            text-align: center;
            color: #999;
            font-style: italic;
        }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>INDEX_TITLE_PLACEHOLDER</h1>
            <div class="subtitle">HDF5 (Hierarchical Data Format 5) Software Library and Utilities</div>
            <div class="description">INDEX_DESCRIPTION_PLACEHOLDER</div>
        </header>

        <div class="breadcrumb">
            <a href="PARENT_URL_PLACEHOLDER">⬆️ Parent Directory</a>
        </div>

        <table class="file-list">
            <thead>
                <tr>
                    <th class="name">Name</th>
                    <th class="size">Size</th>
                    <th class="modified">Modified</th>
                    <th class="description">Description</th>
                </tr>
            </thead>
            <tbody>
FILE_LIST_PLACEHOLDER
            </tbody>
        </table>

        <footer>
            <p>
                <a href="https://www.hdfgroup.org/">The HDF Group</a> |
                <a href="https://portal.hdfgroup.org/documentation/index.html">Documentation</a> |
                <a href="https://github.com/HDFGroup/hdf5">GitHub</a>
            </p>
            <p style="margin-top: 10px;">Copyright © 2006-2025 by The HDF Group</p>
        </footer>
    </div>
</body>
</html>
EOF

# Replace placeholders
sed -i.bak "s|INDEX_TITLE_PLACEHOLDER|$TITLE|g" "$OUTPUT_FILE"
sed -i.bak "s|INDEX_DESCRIPTION_PLACEHOLDER|$DESCRIPTION|g" "$OUTPUT_FILE"
sed -i.bak "s|PARENT_URL_PLACEHOLDER|$PARENT_URL|g" "$OUTPUT_FILE"

# Generate and insert file list
FILE_LIST=$(generate_file_list "$DIRECTORY")

if [ -z "$FILE_LIST" ]; then
    FILE_LIST="        <tr><td colspan='4' class='empty-message'>No files or directories found</td></tr>"
fi

# Create secure temporary file
TEMP_FILE=$(mktemp) || {
    echo "Error: Failed to create temporary file"
    exit 1
}

# Ensure cleanup on exit
trap 'rm -f "$TEMP_FILE"' EXIT

# Use a different delimiter for sed since the content contains slashes
echo "$FILE_LIST" > "$TEMP_FILE"
sed -i.bak "/FILE_LIST_PLACEHOLDER/r $TEMP_FILE" "$OUTPUT_FILE"
sed -i.bak "/FILE_LIST_PLACEHOLDER/d" "$OUTPUT_FILE"

# Clean up backup files
rm -f "$OUTPUT_FILE.bak"

echo "✅ Generated index.html at: $OUTPUT_FILE"
