const projectViews = document.createElement('div');
module.exports = projectViews;

projectViews.views = {};

projectViews.initialize = (userDataDir) => {
  loadSession(userDataDir);

  // set a timer to save session

  nw.Window.get().on('close', async () => {
    await saveSession(userDataDir);
    win.close(true);
  });
}

projectViews.addView = (view) => {}

function loadSession (userDataDir) {
  // if there is a saved session file for the project
}

async function saveSession(userDataDir) => {}
