export { Instrument };

class Instrument {
  constructor(settings) {
    this.update(settings);
  }

  get availableMidiChannels() {
    return _.range(0, 17);
  }

  update(settings) {
    this.number = settings.number || this.number;
    this.name = settings.name || this.name;
    this.instrument = settings.instrument || this.instrument;
    this.soundFontPath = settings.soundFontPath || this.soundFontPath;
    this.parameters = settings.parameters || this.parameters;
    this.midiChannel = settings.midiChannel || this.midiChannel;

    return this;
  }
}
