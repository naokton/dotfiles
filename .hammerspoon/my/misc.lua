-- ================================================================
-- Translate Ctrl+Shift+, and . to prev/next tab in Firefox and Zen
local apps = {Firefox = true, Zen = true}
local nextTab, prevTab

fxWatcher = hs.application.watcher.new(function(appName, eventType, appObject)
    if not apps[appName] then return end

    if eventType == hs.application.watcher.activated then
        nextTab = hs.hotkey.bind({"ctrl", "shift"}, ".", function()
            hs.eventtap.keyStroke({"ctrl"}, "pageDown")
        end)
        prevTab = hs.hotkey.bind({"ctrl", "shift"}, ",", function()
            hs.eventtap.keyStroke({"ctrl"}, "pageUp")
        end)

    elseif eventType == hs.application.watcher.deactivated then
        nextTab:disable()
        prevTab:disable()
    end
end)

fxWatcher:start()

-- ==================================================
-- debug window info
local function debug_window(win)
 print("=== Window Info ===")
    print("Title: " .. (win:title() or "N/A"))
    print("Application: " .. (win:application():name() or "N/A"))
    print("ID: " .. (win:id() or "N/A"))
    print("Screen: " .. hs.inspect(win:screen():name()))
    print("Is Maximizable: " .. tostring(win:isMaximizable()))
    print("Is Standard: " .. tostring(win:isStandard()))
    print("Role: " .. (win:role() or "N/A"))
    print("Subrole: " .. (win:subrole() or "N/A"))
    print("tabCount: " .. tostring(win:tabCount() or "N/A"))
    print("==================")
end

local windowWatcher = hs.window.filter.new()

-- toggle watcher
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "w", function()
    local focusedEvent = hs.window.filter.windowFocused
    if windowWatcher.events[focusedEvent] then
        windowWatcher:unsubscribeAll()
        print("Window watcher stopped")
    else
        windowWatcher:subscribe(focusedEvent, debug_window)
        print("Window watcher started")
    end
end)
