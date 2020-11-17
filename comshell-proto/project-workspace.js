const { Stack } = require("widgets");

class ProjectWorkspace extends HTMLElement {
  projectFiles;
  projectViews = new Stack();

  constructor(projectPath) {
    super();
    this.attachShadow({mode: 'open'});

    const wrapper = document.createElement('div');
    wrapper.setAttribute('class','wrapper');

    const style = document.createElement('style');
    style.textContent = `.wrapper {
      display: flex;
    }`

    this.shadowRoot.append(style, wrapper);

    this.projectFiles = new ProjectFiles();
    this.appendChild(this.projectFiles);

    this.appendChild(this.projectViews);

    this.loadSession(projectPath);

    // set a timer to save session

    nw.Window.get().on('close', async () => {
      await this.saveSession(projectPath);
      nw.App.quit();
    });
  }

  loadSession (projectPath) {
    // if there is a saved session file for the project
  }

  async saveSession(projectPath) => {
    // save paths relatively; otherwise renaming/moving project breaks paths;
  }
}

window.customElements.define('project-workspace', ProjectWorkspace);
