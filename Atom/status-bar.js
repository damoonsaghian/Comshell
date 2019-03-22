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

  function updateDateTime() {
    const now = new Date();
    setTimeout(updateDateTime, (60 - now.getSeconds()) * 1000);
    // according to the following link, "setTime" circumvents system sleeps:
    // https://stackoverflow.com/a/38408581

    const weekDay = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'][now.getDay()];
    const hour = now.getHours();
    const hour12 = (hour > 12) ? (hour - 12) : hour;
    const dayPeriod = (hour > 12) ? 'pm' : 'am';
    dateTimeElement.textContent = weekDay +' '+ hour12 +':'+ now.getMinutes() + dayPeriod;
  }

  updateDateTime();
}

// https://github.com/sebhildebrandt/systeminformation
