export { Track };

class Track {
  constructor(settings) {
    this.update(settings);
  }

  csvName() {
    if (this.name == '#' || this.name == 'Master') {
      return this.name;
    } else {
      let prefix = '';

      if (this.mute) {
        prefix = 'M';
      }

      if (this.solo) {
        prefix = 'S'
      }

      return `${prefix}I${this.instrumentNumber} ${this.name}`;
    }
  }

  update(settings) {
    this.instrumentNumber = settings.instrumentNumber || this.instrumentNumber;
    this.name = settings.name || this.name;
    this.mute = settings.mute || this.mute;
    this.solo = settings.solo || this.solo;

    return this;
  }
}
