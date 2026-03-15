#!/bin/bash
input=$(cat)

# Colors
RESET='\033[0m'
GRAY='\033[38;5;245m'
DIM='\033[38;5;236m'
CYAN='\033[38;5;81m'
YELLOW='\033[38;5;220m'
RED='\033[38;5;196m'
GREEN='\033[38;5;114m'

# Nerd Font icons (UTF-8 bytes)
I_DIR='\xee\xab\xb7'
I_WT='\xee\xab\xb6'
I_BRANCH='\xee\xa9\xa4'
I_MODEL='\xee\xab\xb2'
I_CTX='\xee\xac\xb1'

# Helpers
section() { printf "${1}${2} %s${RESET}" "$3"; }
sep() { printf "${DIM}\xe2\x94\x82${RESET}"; }
osc_link() { printf '\033]8;;%s\033\\%s\033]8;;\033\\' "$1" "$2"; }
md5hash() { echo "$1" | md5 -q 2>/dev/null || echo "$1" | md5sum 2>/dev/null | cut -d' ' -f1; }

cache_get() {
  local file="$1" ttl="$2"
  [ -f "$file" ] || return 1
  if [ "$ttl" -gt 0 ] 2>/dev/null; then
    local age=$(($(date +%s) - $(stat -f %m "$file" 2>/dev/null || stat -c %Y "$file" 2>/dev/null || echo 0)))
    [ "$age" -ge "$ttl" ] && return 1
  fi
  cat "$file"
}

cache_set() { echo "$2" > "$1"; }

field() { echo "$input" | jq -r "$1"; }

# Sections

git_info() {
  local cwd="$1"
  git -C "$cwd" rev-parse --git-dir > /dev/null 2>&1 || return 1

  local common=$(git -C "$cwd" --no-optional-locks rev-parse --git-common-dir 2>/dev/null)
  local dir=$(git -C "$cwd" --no-optional-locks rev-parse --git-dir 2>/dev/null)

  GIT_BRANCH=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null)
  IS_WORKTREE=false
  [ "$common" != "$dir" ] && IS_WORKTREE=true
}

repo_url() {
  local cwd="$1"
  local cache="/tmp/statusline-repo-$(md5hash "$cwd")"
  if ! cache_get "$cache" 0; then
    local url=$(git -C "$cwd" --no-optional-locks remote get-url origin 2>/dev/null \
      | sed 's|git@github.com:|https://github.com/|;s|\.git$||')
    cache_set "$cache" "$url"
    echo "$url"
  fi
}

pr_info() {
  local cwd="$1" branch="$2"
  local cache="/tmp/statusline-pr-$(md5hash "${cwd}:${branch}")"
  if ! cache_get "$cache" 30; then
    local data=$(gh pr view --json url,number -q '"\(.number) \(.url)"' 2>/dev/null || true)
    cache_set "$cache" "$data"
    echo "$data"
  fi
}

render_dir() {
  local dir="$1" is_wt="$2"
  local icon="$I_DIR" color="$GRAY"
  [ "$is_wt" = true ] && icon="$I_WT" && color="$GREEN"

  section "$color" "$icon" "$dir"
}

render_branch() {
  local branch="$1" pr_num="$2" pr_url="$3" repo_url="$4"
  local color="$GRAY"
  case "$branch" in main|master) ;; *) color="$CYAN" ;; esac

  if [ -n "$repo_url" ]; then
    printf "${color}${I_BRANCH} "
    osc_link "$repo_url" "$branch"
    printf "${RESET}"
  else
    section "$color" "$I_BRANCH" "$branch"
  fi
  if [ -n "$pr_num" ]; then
    printf " ${color}"
    osc_link "$pr_url" "(#${pr_num})"
    printf "${RESET}"
  fi
}

render_context() {
  local pct="$1" used_tokens="$2" total_tokens="$3"
  local color="$GRAY"
  [ "$used_tokens" -gt 300000 ] 2>/dev/null && color="$RED" || { [ "$used_tokens" -gt 150000 ] 2>/dev/null && color="$YELLOW"; }

  local used_k=$(( (used_tokens + 500) / 1000 ))
  local total_k=$(( (total_tokens + 500) / 1000 ))

  section "$color" "$I_CTX" "${pct}% (${used_k}k/${total_k}k)"
  printf " "

  local filled=$((pct * 10 / 100))
  local i
  for i in $(seq 1 10); do
    if [ "$i" -le "$filled" ]; then
      printf "${color}\xe2\x96\xae${RESET}"
    else
      printf "${DIM}\xe2\x96\xae${RESET}"
    fi
  done
}

# Main

CWD=$(field '.workspace.current_dir')
DIR=$(basename "$CWD")
MODEL=$(field '.model.display_name')
PCT=$(field '.context_window.used_percentage // 0' | cut -d. -f1)
USED_TOKENS=$(( PCT * $(field '.context_window.context_window_size // 0') / 100 ))
TOTAL_TOKENS=$(field '.context_window.context_window_size // 0')

GIT_BRANCH=""
IS_WORKTREE=false
REPO=""
PR_NUM=""
PR_URL=""

if git_info "$CWD"; then
  REPO=$(repo_url "$CWD")
  PR_DATA=$(pr_info "$CWD" "$GIT_BRANCH")
  PR_NUM=$(echo "$PR_DATA" | cut -d' ' -f1)
  PR_URL=$(echo "$PR_DATA" | cut -d' ' -f2)
fi

printf "%s %s %s %s %s %s %s" \
  "$(render_dir "$DIR" "$IS_WORKTREE")" "$(sep)" \
  "$(render_branch "$GIT_BRANCH" "$PR_NUM" "$PR_URL" "$REPO")" "$(sep)" \
  "$(section "$GRAY" "$I_MODEL" "$MODEL")" "$(sep)" \
  "$(render_context "$PCT" "$USED_TOKENS" "$TOTAL_TOKENS")"
