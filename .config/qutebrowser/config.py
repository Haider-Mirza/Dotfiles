# THIS DOCUMENT IS MANAGED BY ORGMODE

# Open every tab as a new window, Vimb style
c.tabs.tabs_are_windows = True
c.tabs.last_close = "close"

c.auto_save.session = True
c.scrolling.smooth = True
c.session.lazy_restore = True
c.content.autoplay = False

# Better default fonts
c.fonts.default_family = '"Source Code Pro"'
c.fonts.completion.entry = '11pt "Source Code Pro"'
c.fonts.debug_console = '11pt "Source Code Pro"'
c.fonts.default_size = '11pt'
c.fonts.prompts = 'default_size sans-serif'
c.fonts.statusbar = '10pt "Source Code Pro"'

# Use dark mode where possible
# c.colors.webpage.preferred_color_scheme = "dark"
# c.colors.webpage.darkmode.enabled = True
# c.colors.webpage.darkmode.policy.images = "never"
# c.colors.webpage.bg = "black"

# Scale pages and UI better for hidpi
c.fonts.hints = "bold 15pt monospace"

# Set Downloads Directory
c.downloads.location.directory = '~/Downloads'

# When to show tabs
c.tabs.show = "never"
c.statusbar.show = "never"

# Setting default page for when opening new tabs or new windows with
# commands like :open -t and :open -w .
c.url.default_page = 'https://start.duckduckgo.com/'
c.url.start_pages = 'https://start.duckduckgo.com/'

c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}', 'am': 'https://www.amazon.com/s?k={}', 'aw': 'https://wiki.archlinux.org/?search={}', 'goog': 'https://www.google.com/search?q={}', 'hoog': 'https://hoogle.haskell.org/?hoogle={}', 're': 'https://www.reddit.com/r/{}', 'ub': 'https://www.urbandictionary.com/define.php?term={}', 'wiki': 'https://en.wikipedia.org/wiki/{}', 'yt': 'https://www.youtube.com/results?search_query={}', 'aur': 'https://aur.archlinux.org/packages/?O=0&K={}', 'od': 'https://odysee.com/$/search?q={}'}

c.colors.completion.fg = ['#9cc4ff', 'white', 'white']
c.colors.completion.odd.bg = '#1c1f24'
c.colors.completion.even.bg = '#232429'
c.colors.completion.category.fg = '#e1acff'
c.colors.completion.category.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #000000, stop:1 #232429)'
c.colors.completion.category.border.top = '#3f4147'
c.colors.completion.category.border.bottom = '#3f4147'
c.colors.completion.item.selected.fg = '#282c34'
c.colors.completion.item.selected.bg = '#ecbe7b'
c.colors.completion.item.selected.match.fg = '#c678dd'
c.colors.completion.match.fg = '#c678dd'
c.colors.completion.scrollbar.fg = 'white'
c.colors.downloads.bar.bg = '#282c34'
c.colors.downloads.error.bg = '#ff6c6b'
c.colors.hints.fg = '#282c34'
c.colors.hints.match.fg = '#98be65'
c.colors.messages.info.bg = '#282c34'
c.colors.statusbar.normal.bg = '#282c34'
c.colors.statusbar.insert.fg = 'white'
c.colors.statusbar.insert.bg = '#497920'
c.colors.statusbar.passthrough.bg = '#34426f'
c.colors.statusbar.command.bg = '#282c34'
c.colors.statusbar.url.warn.fg = 'yellow'
c.colors.tabs.bar.bg = '#1c1f34'
c.colors.tabs.odd.bg = '#282c34'
c.colors.tabs.even.bg = '#282c34'
c.colors.tabs.selected.odd.bg = '#282c34'
c.colors.tabs.selected.even.bg = '#282c34'
c.colors.tabs.pinned.odd.bg = 'seagreen'
c.colors.tabs.pinned.even.bg = 'darkseagreen'
c.colors.tabs.pinned.selected.odd.bg = '#282c34'
c.colors.tabs.pinned.selected.even.bg = '#282c34'

# Automatically turn on insert mode when a loaded page focuses a text field
c.input.insert_mode.auto_load = True


# Edit fields in Emacs with Ctrl+E
c.editor.command = ["emacsclient", "+{line}:{column}", "{file}"]

# Make Ctrl+g quit everything like in Emacs
config.bind('<Ctrl-g>', 'leave-mode', mode='insert')
config.bind('<Ctrl-g>', 'leave-mode', mode='command')
config.bind('<Ctrl-g>', 'leave-mode', mode='prompt')
config.bind('<Ctrl-g>', 'leave-mode', mode='hint')
# config.unbind('b') # Re-keybind 'b'
# config.bind('b', 'spawn ~/.config/qutebrowser/Qute.sh')

# Tweak some keybindings
config.unbind('d') # Don't close window on lower-case 'd'
config.bind('yy', 'yank')

# Vim-style movement keys in command mode
config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')

# More binding hints here: https://gitlab.com/Kaligule/qutebrowser-emacs-config/blob/master/config.py

config.bind('X', 'wq')
config.unbind('d') # Dont want to accidentally delete my tab
config.unbind('u') # rekeybind the u key
config.bind('Q', 'bookmark-add')
config.bind('W', 'bookmark-del')
config.bind('E', 'bookmark-list')
config.bind('u', 'undo --window')
config.bind('b', 'set-cmd-text -s :tab-select ', mode='normal')
config.bind('z', 'spawn ~/.config/qutebrowser/scripts/mpv.sh;; spawn mpv --speed=2 --ytdl-raw-options=\'sub-lang=\"en\",write-sub=,write-auto-sub=\' {url}')
config.bind('Z', 'hint links spawn mpv --speed=2 --ytdl-raw-options=\'sub-lang=\"en\",write-sub=,write-auto-sub=\' {hint-url}')
config.bind('t', 'set-cmd-text -s :open -t')
config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')

c.content.javascript.enabled = True
c.content.webgl = True

# Load the autoconfig file (quteconfig.py)
config.load_autoconfig()
