#!/bin/bash

BRANCH="$1"
WORKTREE_PATH="$2"
REPO="$3"
TITLE="$REPO [$BRANCH]"

WS_ID=$(cmux find-window "$TITLE" 2>/dev/null | grep -v 'No matches' | awk '{print $1}')

if [ -n "$WS_ID" ]; then
  cmux select-workspace --workspace "$WS_ID"
else
  WS_ID=$(cmux new-workspace --cwd "$WORKTREE_PATH" --command "cmux workspace-action --action rename --title '$TITLE' >/dev/null 2>&1" | awk '{print $2}')
  cmux select-workspace --workspace "$WS_ID"
fi
