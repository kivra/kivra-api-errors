#!/bin/bash

if ! command -v jq &>/dev/null; then
  echo "You need 'jq' to run this script."
  exit 1
fi

echo "Copy-paste to developer.kivra.com's swagger.yml"
echo "<copy-paste>"
echo "    | Code | Short Message | Long Message |"
echo "    | ---- | ------------- | ------------ |"
jq -r 'to_entries | sort_by(.key) | .[] | "    | \(.key) | \(.value.short_message) | \(.value.long_message) |"' <api-errors.json
echo "</copy-paste>"
