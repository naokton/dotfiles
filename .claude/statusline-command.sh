#!/usr/bin/env bash

input=$(cat)
[ -z "$input" ] && printf "Claude" && exit 0

# ===== Colors (truecolor) =====
cyan='\033[38;2;86;182;194m'   # default for all text
green='\033[38;2;80;200;120m'
orange='\033[38;2;255;176;85m'
yellow='\033[38;2;230;200;0m'
red='\033[38;2;235;87;87m'
reset='\033[0m'
sep="${reset} │ ${cyan}"

# minutes -> "Nm" / "Nh" / "Nd Nh"
fmt_time() {
    [ -z "$1" ] && return
    [ "$1" -le 99 ] && { echo "${1}m"; return; }
    h=$(($1 / 60))
    [ "$h" -ge 24 ] && echo "$((h / 24))d$((h % 24))h" || echo "${h}h"
}

# percentage -> color (cyan / yellow / red)
pct_color() { [ "$1" -ge 80 ] && printf '%b' "$red" || { [ "$1" -ge 50 ] && printf '%b' "$yellow" || printf '%b' "$cyan"; }; }

# percentage -> single block character ▁▂▃▄▅▆▇█
pct_block() {
    [ "$1" -eq 0 ] && printf ' ' && return
    local blocks=(▁ ▂ ▃ ▄ ▅ ▆ ▇ █) idx=$(( $1 / 13 ))
    [ "$idx" -gt 7 ] && idx=7
    printf '%s' "${blocks[$idx]}"
}

# bar graph: make_bar <pct> <color>
#   filled cells  → █ fg=color
#   boundary cell → partial block fg=color bg=track
#   empty cells   → █ fg=track
make_bar() {
    local pct=$1 color=$2 total=10
    local sub=$(( pct * total * 8 / 100 ))
    [ "$pct" -gt 0 ] && [ "$sub" -eq 0 ] && sub=1
    local full=$(( sub / 8 ))
    local part=$(( sub % 8 ))
    local empty=$(( total - full - (part > 0) ))

    local track='\033[38;2;40;42;54m'
    local bg_track='\033[48;2;40;42;54m'
    local partials=(' ' '▏' '▎' '▍' '▌' '▋' '▊' '▉')

    local fill pad mid=""
    printf -v fill "%${full}s";  fill="${fill// /█}"
    printf -v pad  "%${empty}s"; pad="${pad// /█}"
    [ "$part" -gt 0 ] && mid="${bg_track}${partials[$part]}"

    printf '%b' "${color}${fill}${mid}${reset}${track}${pad}${reset}"
}

add() { [ -z "$out" ] && out+="$1" || out+="${sep}$1"; }

# ===== Extract all fields in one jq pass (one per line; empties preserved) =====
{
    IFS= read -r model
    IFS= read -r effort
    IFS= read -r agent
    IFS= read -r used
    IFS= read -r rl5
    IFS= read -r rl5_reset
    IFS= read -r rl7
    IFS= read -r rl7_reset
    IFS= read -r cache_read
    IFS= read -r cache_create
    IFS= read -r cwd
    IFS= read -r cost_usd
} < <(
    printf '%s' "$input" | jq -r '
        [ .model.display_name, .effort.level, .agent.name,
          .context_window.used_percentage,
          .rate_limits.five_hour.used_percentage,  .rate_limits.five_hour.resets_at,
          .rate_limits.seven_day.used_percentage,  .rate_limits.seven_day.resets_at,
          .context_window.current_usage.cache_read_input_tokens,
          .context_window.current_usage.cache_creation_input_tokens,
          (.workspace.current_dir // .cwd),
          .cost.total_cost_usd
        ] | map(. // "") | .[]'
)

now=$(date +%s)

# "Claude Opus 4.7 (1M context)" -> "Opus 4.7 (1M)"
model="${model#Claude }"
model="${model/ context/}"

# ===== Assemble =====
out=""

# Git: project dir + branch
branch=""
[ -n "$cwd" ] && branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null)
if [ -n "$branch" ]; then
    proj=$(git -C "$cwd" --no-optional-locks rev-parse --show-toplevel 2>/dev/null)
    add "${proj##*/} ⎇ ${branch}"
elif [ -n "$cwd" ]; then
    add "${cwd##*/}"
fi

# Model block — model + effort + agent + context%, grouped as one unit
mblock="$model"
[ -n "$effort" ] && mblock+="${mblock:+ }$effort"
[ -n "$agent" ]  && mblock+="${mblock:+ }agent:${agent}"
if [ -n "$used" ]; then
    printf -v ctx_pct "%.0f" "$used"
    if   [ "$ctx_pct" -ge 80 ]; then ctx_color="$red"
    elif [ "$ctx_pct" -ge 50 ]; then ctx_color="$orange"
    else ctx_color="$cyan"; fi
    mblock+="${mblock:+ }$(make_bar "$ctx_pct" "$ctx_color") ${ctx_color}${ctx_pct}%${cyan}"
fi
[ -n "$mblock" ] && add "$mblock"

# Rate limits — 5h
if [ -n "$rl5" ]; then
    printf -v f "%.0f" "$rl5"
    t5=""
    [ -n "$rl5_reset" ] && [ "$rl5_reset" -gt "$now" ] 2>/dev/null && t5=$(fmt_time $(( (rl5_reset - now) / 60 )))
    add "$(pct_color "$f")$(pct_block "$f") ${f}%${cyan}${t5:+ ↻${t5}/5h}"
fi

# Rate limits — 7d
if [ -n "$rl7" ]; then
    printf -v s "%.0f" "$rl7"
    t7=""
    [ -n "$rl7_reset" ] && [ "$rl7_reset" -gt "$now" ] 2>/dev/null && t7=$(fmt_time $(( (rl7_reset - now) / 60 )))
    add "$(pct_color "$s")$(pct_block "$s") ${s}%${cyan}${t7:+ ↻${t7}/7d}"
fi

# Cache hit rate
cache_total=$(( ${cache_read:-0} + ${cache_create:-0} ))
if [ "$cache_total" -gt 0 ]; then
    hit_pct=$(( cache_read * 100 / cache_total ))
    if   [ "$hit_pct" -ge 80 ]; then cache_color="$green"
    elif [ "$hit_pct" -ge 50 ]; then cache_color="$cyan"
    else cache_color="$orange"; fi
    add "cache ${cache_color}${hit_pct}%${cyan}"
fi

# Total session cost
[ -n "$cost_usd" ] && add "$(printf '$%.2f' "$cost_usd")"

printf "%b\n" "${cyan}${out}${reset}"
