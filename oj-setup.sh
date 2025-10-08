#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: oj-setup <problem_url>"
    exit 1
fi

URL=$1
TASK_ID=$(echo $URL | grep -oP '(?<=tasks/)[^/]+')

# ディレクトリ作成
mkdir -p "$TASK_ID"
cd "$TASK_ID"

# テストケースダウンロード
oj download "$URL"

# templateからmain.hsを作成
cat ../template.hs > main.hs
echo "Setup complete! Directory: $TASK_ID"
echo "Files created:"
ls -la