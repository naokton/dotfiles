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

# seconds + seconds-per-unit -> decimal with one place ("4.5")
#   fmt_decimal <seconds> 3600  -> hours  (e.g. 16200 -> 4.5)
#   fmt_decimal <seconds> 86400 -> days   (e.g. 276480 -> 3.2)
fmt_decimal() {
    [ -z "$1" ] && return
    local tenths=$(( $1 * 10 / $2 ))
    echo "$(( tenths / 10 )).$(( tenths % 10 ))"
}

# percentage ramp: ramp <pct> <lo> <mid> <hi>  (hi at >=80, mid at >=50, lo below)
ramp() { [ "$1" -ge 80 ] && printf '%b' "$4" || { [ "$1" -ge 50 ] && printf '%b' "$3" || printf '%b' "$2"; }; }

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

# rate-limit row: render_rl <pct> <reset_epoch> <seconds_per_unit> <label>
render_rl() {
    [ -z "$1" ] && return
    local p t=""
    printf -v p "%.0f" "$1"
    [ -n "$2" ] && [ "$2" -gt "$now" ] 2>/dev/null && t=$(fmt_decimal $(( $2 - now )) "$3")
    add "$(ramp "$p" "$cyan" "$yellow" "$red")$(pct_block "$p") ${p}%${cyan}${t:+ ↻${t}/$4}"
}

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
    jq -r <<<"$input" '
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
    ctx_color=$(ramp "$ctx_pct" "$cyan" "$orange" "$red")
    mblock+="${mblock:+ }$(make_bar "$ctx_pct" "$ctx_color") ${ctx_color}${ctx_pct}%${cyan}"
fi
[ -n "$mblock" ] && add "$mblock"

# Rate limits — 5h window, 7d window
render_rl "$rl5" "$rl5_reset" 3600  5h
render_rl "$rl7" "$rl7_reset" 86400 7d

# Cache hit rate
cache_total=$(( ${cache_read:-0} + ${cache_create:-0} ))
if [ "$cache_total" -gt 0 ]; then
    hit_pct=$(( cache_read * 100 / cache_total ))
    cache_color=$(ramp "$hit_pct" "$orange" "$cyan" "$green")
    add "cache ${cache_color}${hit_pct}%${cyan}"
fi

# Total session cost
[ -n "$cost_usd" ] && add "$(printf '$%.2f' "$cost_usd")"

printf "%b\n" "${cyan}${out}${reset}"
