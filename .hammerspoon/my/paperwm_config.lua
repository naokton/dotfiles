--
-- WarpMouse
--
WarpMouse = hs.loadSpoon("WarpMouse")
WarpMouse:start()

--
-- PaperWM
--
PaperWM = hs.loadSpoon("PaperWM")
PaperWM:bindHotkeys({
    -- switch to a new focused window in tiled grid
    focus_left  = {{"cmd"}, "left"},
    focus_right = {{"cmd"}, "right"},
    focus_up    = {{"cmd"}, "up"},
    focus_down  = {{"cmd"}, "down"},

    -- move windows around in tiled grid
    swap_left  = {{"cmd", "shift"}, "left"},
    swap_right = {{"cmd", "shift"}, "right"},
    swap_up    = {{"cmd", "shift"}, "up"},
    swap_down  = {{"cmd", "shift"}, "down"},

    -- switch windows by cycling forward/backward
    -- (forward = down or right, backward = up or left)
    focus_prev = {{"alt", "cmd"}, "k"},
    focus_next = {{"alt", "cmd"}, "j"},

    -- position and resize focused window
    center_window        = {{"alt", "cmd"}, "c"},
    full_width           = {{"alt", "cmd"}, "f"},
    cycle_width          = {{"alt", "cmd"}, "r"},
    cycle_height         = {{"alt", "cmd", "shift"}, "r"},

    -- increase/decrease width
    increase_width = {{"alt", "cmd"}, "="},
    decrease_width = {{"alt", "cmd"}, "-"},

    -- move focused window into / out of a column
    -- avoid C-M-i conflicts with browser's dev tool shortcut
    slurp_in = {{"alt", "cmd"}, "left"},
    barf_out = {{"alt", "cmd"}, "right"},

    -- move the focused window into / out of the tiling layer
    -- toggle_floating = {{"alt", "cmd", "shift"}, "escape"},
    -- raise all floating windows on top of tiled windows
    focus_floating  = {{"alt", "cmd", "shift"}, "f"},

    -- focus the first / second / etc window in the current space
    -- don't use so much

    -- switch to a new Mission Control space
    -- Use macOS keyboard shortcut for reliable results

    -- move focused window to a new space and tile
    move_window_1 = {{"alt", "cmd", "shift"}, "1"},
    move_window_2 = {{"alt", "cmd", "shift"}, "2"},
    move_window_3 = {{"alt", "cmd", "shift"}, "3"},
    move_window_4 = {{"alt", "cmd", "shift"}, "4"},
    move_window_5 = {{"alt", "cmd", "shift"}, "5"},
    move_window_6 = {{"alt", "cmd", "shift"}, "6"},
    move_window_7 = {{"alt", "cmd", "shift"}, "7"},
    move_window_8 = {{"alt", "cmd", "shift"}, "8"},
    move_window_9 = {{"alt", "cmd", "shift"}, "9"}
})

local wm_excluded_apps = {
    "System Settings",
    "Finder",
    "Archive Utility",
    "Dictionary",
    "Microsoft Outlook",
    "Microsoft Teams",
    "Slack",
    "CotEditor",
    "TextEdit"
}
for _, name in ipairs(wm_excluded_apps) do
    PaperWM.window_filter:rejectApp(name)
end

-- Toggle floating. Make window 80% height when floating
local float_h_ratio = 0.8

function toggle_floating_with_resize()
    local win = hs.window.focusedWindow()
    if not win then return end

    PaperWM.floating.toggleFloating(win)

    if PaperWM.floating.isFloating(win) then
        local f = win:frame()
        f.h = f.h * float_h_ratio
        win:setFrame(f)
        win:centerOnScreen()
    end
end

hs.hotkey.bind({"alt", "cmd", "shift"}, "escape", toggle_floating_with_resize)

-- Only enables on primary screen
PaperWM.window_filter:setScreens(PRIMARY_SCREEN)
PaperWM.window_gap  =  { top = 0, bottom = 0, left = 4, right = 4 }
PaperWM:start()

hs.window.animationDuration = 0.05
