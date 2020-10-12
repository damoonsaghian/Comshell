class ProjectWorkspace extends HTMLElement {
  constructor(projectPath) {
    super();
    this.style.display = "flex";
  }
}

window.customElements.define('project-workspace', ProjectWorkspace);
