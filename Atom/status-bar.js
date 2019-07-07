if (atom.packages.isPackageActive('status-bar')) {
  const statusBar = atom.packages.getActivePackage('status-bar').mainModule.statusBar;
  if (statusBar) { showDateTime(statusBar) }
}
atom.packages.onDidActivatePackage(activatedPackage => {
  if (activatedPackage === atom.packages.getActivePackage('status-bar')) {
    const statusBar = activatedPackage.mainModule.statusBar;
    if (statusBar) { showDateTime(statusBar) }
  }
});

function showDateTime(statusBar) {
  const dateTimeElement = document.createElement('div');
  dateTimeElement.classList.add('inline-block');
  statusBar.addRightTile({ item: dateTimeElement, priority: -1 });

  function updateDateTime(repeat) {
    const now = new Date(new Date()
      .toLocaleString("en-US", { timeZone: process.env.TZ })
    );
    if (repeat) setTimeout(() => updateDateTime(true), (60 - now.getSeconds()) * 1000);

    const weekDay = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'][now.getDay()];
    const hour = now.getHours();
    const hour12 = hour == 0 ? 12 : hour > 12 ? hour - 12 : hour;
    const minute = now.getMinutes();
    const minute2digits = minute > 9 ? minute : '0' + minute;
    const dayPeriod = hour > 11 ? 'PM' : 'AM';
    dateTimeElement.textContent =
      now.getFullYear() +'/'+ (now.getMonth()+1) +'/'+ now.getDate() +' '+
      weekDay +' '+ dayPeriod +' '+ hour12 +':'+ minute2digits;
  }

  // update date after computer wakes up from sleep;
  require('electron').remote.powerMonitor.on('resume', () => updateDateTime(false))
  updateDateTime(true);
}

// https://github.com/sebhildebrandt/systeminformation
