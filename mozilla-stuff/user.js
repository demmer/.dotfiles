
// path and download preferences
user_pref("browser.startup.homepage", "http://my.yahoo.com/");
user_pref("browser.download.dir", "/home/demmer/downloads");
user_pref("browser.download.lastDir", "/tmp");
user_pref("browser.download.openSidebar", true);
user_pref("browser.download.useDownloadDir", false);
user_pref("browser.download.useProgressDialogs", false);

// ignore the annoying popup warnings
user_pref("privacy.popups.firstTime", false);
user_pref("security.warn_entering_secure", false);
user_pref("security.warn_leaving_secure", false);
user_pref("security.warn_submit_insecure", false);
user_pref("security.warn_viewing_mixed", false);

// tab bar prefs
user_pref("browser.tabs.autoHide", false);
user_pref("browser.tabs.warnOnClose", false);

// Instead of annoying error dialog messages, display pages: 
user_pref("browser.xul.error_pages.enabled", true);

// Instead of download progress windows, use the Sidebar: 
user_pref("browser.download.openSidebar", true); 
user_pref("browser.download.useProgressDialogs", false);

// Change to normal Google search: 
user_pref("keyword.URL", "http://www.google.com/search?btnG=Google+Search&q=");
