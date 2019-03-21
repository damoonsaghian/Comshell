// time_zone will be automatically detected from IP;
// but when a problem occurs, "fallBackTimeZone" will be used instead;
const fallBackTimeZone = 'UTC';

function showDateTime(statusBarPkg, timeZone) {
  if (statusBarPkg.mainModule.statusBar) {

    const dateTimeElement = document.createElement('div');
    dateTimeElement.classList.add('inline-block');
    const statusBar = statusBarPkg.mainModule.statusBar;
    statusBar.addRightTile({ item: dateTimeElement, priority: -1 });

    const options = {
      weekday: 'short',
      month: 'numeric',
      day: 'numeric',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      timeZoneName: 'short',
      timeZone: timeZone,
      hour12: true
    };

    let dateTimeFormatter;
    try {
      dateTimeFormatter = new Intl.DateTimeFormat('en-US', options);
    } catch (err) {
      options.timeZone = fallBackTimeZone;
      dateTimeFormatter = new Intl.DateTimeFormat('en-US', options);
    }

    function updateDateTime() {
      const dateTime = {};
      dateTimeFormatter.formatToParts(new Date()).forEach(({type, value}) => {
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

      setTimeout(updateDateTime, (60 - new Date().getSeconds()) * 1000);
      // according to the following link, "setTime" circumvents system sleeps:
      // https://stackoverflow.com/a/38408581
    }

    updateDateTime();
  }
}

atom.packages.onDidActivatePackage(activatedPackage => {
  if (activatedPackage === atom.packages.getActivePackage('status-bar')) {

    require('https').get(
      {
        hostname: 'freegeoip.app',
        port: null,
        path: '/json/',
        headers: { 'accept': 'application/json', 'content-type': 'application/json' }
      },
      response => {
        const chunks = [];
        response.on('data', chunk => chunks.push(chunk));
        response.on('end', () => {
          const body = Buffer.concat(chunks);
          try {
            const timeZone = JSON.parse(body.toString())['time_zone'];
            showDateTime(activatedPackage, timeZone);
          }
          catch (err) {
            console.error(err);
            showDateTime(activatedPackage, fallBackTimeZone);
          };
        });
      }
    ).on('error', err => {
      console.error(err);
      showDateTime(activatedPackage, fallBackTimeZone);
    });
  }
});

// https://github.com/sebhildebrandt/systeminformation
