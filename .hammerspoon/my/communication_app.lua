--
-- Launch, move to a space, and arrange communication apps
--

local apps = {
    {
        name = "Microsoft Teams",
        bottomOffset = 0,
        topOffset = 100,
        leftOffset = 0,
        rightOffset = 800,
    },
    {
        name = "Microsoft Outlook",
        bottomOffset = 0,
        topOffset = 50,
        leftOffset = 320,
        rightOffset = 200,
    },
    {
        name = "Slack",
        bottomOffset = 0,
        topOffset = 0,
        leftOffset = 580,
        rightOffset = 480,
    },
}

local WAIT_INTERVAL_SECOND = 0.1
local WAIT_LIMIT_SECONDS = 10

local function ensureAppWindow(apps)
    local startTime = hs.timer.secondsSinceEpoch()
    while true do
        local allAppsReady = true
        for _, appInfo in ipairs(apps) do
            local app = hs.application.get(appInfo.name)
            if not app or #app:allWindows() == 0 then
                allAppsReady = false
                break
            end
        end
        if allAppsReady then
            break
        end

        if hs.timer.secondsSinceEpoch() - startTime > WAIT_LIMIT_SECONDS then
            hs.alert.show("Timeout waiting for apps to launch")
            break
        end
        hs.timer.usleep(WAIT_INTERVAL_SECOND * 1e6)
    end
end

local function getSecondaryScreen()
    local screens = hs.screen.allScreens()
    for _, scr in ipairs(screens) do
        local name = scr:name()
        local isPrimary = false
        for _, primaryName in ipairs(PRIMARY_SCREEN) do
            if name == primaryName then
                isPrimary = true
                break
            end
        end
        if not isPrimary then
            return scr
        end
    end
    return nil
end

local function layout(appConfig, screen)
    -- Move apps to target screen
    local win = hs.application.get(appConfig.name):mainWindow()
    win:moveToScreen(screen)

    -- set size and position based on offsets
    local f = screen:frame()
    local frame = {
        x = f.x + (appConfig.leftOffset or 0),
        y = f.y + (appConfig.topOffset or 0),
        w = f.w - (appConfig.leftOffset or 0) - (appConfig.rightOffset or 0),
        h = f.h - (appConfig.topOffset or 0) - (appConfig.bottomOffset or 0),
    }
    win:setFrame(frame)
end

local function setup()
    for _, appInfo in ipairs(apps) do
        hs.application.launchOrFocus(appInfo.name)
    end

    ensureAppWindow(apps)

    local screen = getSecondaryScreen() or screen.mainScreen()

    for _, appInfo in ipairs(apps) do
        layout(appInfo, screen)
    end
end

hs.hotkey.bind({"ctrl", "alt", "cmd"}, "c", setup)
