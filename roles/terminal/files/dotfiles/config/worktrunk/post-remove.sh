#!/bin/bash

BRANCH="$1"
REPO="$2"
TITLE="$REPO [$BRANCH]"

WS_ID=$(cmux find-window "$TITLE" 2>/dev/null | grep -v 'No matches' | awk '{print $1}')

if [ -n "$WS_ID" ]; then
  cmux close-workspace --workspace "$WS_ID"
fi
