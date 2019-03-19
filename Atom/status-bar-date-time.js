exports.showDateTime = function() {
  const statusBarPkg = atom.packages.getActivePackage('status-bar');
  if (statusBarPkg.mainModule.statusBar) {
   
    const dateTimeElement = document.createElement('div');
    dateTimeElement.classList.add('inline-block');
    const statusBar = statusBarPkg.mainModule.statusBar;
    statusBar.addRightTile({ item: dateTimeElement, priority: -1 });

    let timeZone = 'UTC';
    // https://freegeoip.app/
    //require('https').get();

    const dateTimeFormatter = new Intl.DateTimeFormat('en-US', {
      weekday: 'short',
      month: 'numeric',
      day: 'numeric',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      timeZoneName: 'short',
      timeZone,
      hour12: true
    }); 

    function updateDateTime() {
      const dateTime = {};
      dateTimeFormatter.formatToParts(Date.now()).forEach(({type, value}) => { 
        switch (type) {
        case 'weekday': dateTime.weekday = value;
        case 'month': dateTime.month = value;
        case 'day': dateTime.day = value;
        case 'year': dateTime.year = value;
        case 'hour': dateTime.hour = value;
        case 'minute': dateTime.minute = value;
        case 'dayPeriod': dateTime.dayPeriod = value;
        case 'dayperiod': dateTime.dayPeriod = value;
        case 'timeZoneName': dateTime.timeZoneName = value;
        default : return; 
        } 
      });
      dateTimeElement.textContent = `(${dateTime.timeZoneName}) ` +
        `${dateTime.year}.${dateTime.month}.${dateTime.day} ` +
        `${dateTime.weekday} ${dateTime.hour}:${dateTime.minute}${dateTime.dayPeriod.toLowerCase()}`;

      setTimeout(updateDateTime, (60 - new Date(Date.now()).getSeconds()) * 1000);
      // according to the following link, "setTime" circumvents system sleeps:
      // https://stackoverflow.com/a/38408581
    }

    updateDateTime();
  }
}
