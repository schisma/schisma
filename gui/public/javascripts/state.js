export { State };

import { Instrument } from './instrument.js';

import {
  createTrackFromName,
  scale
} from './utilities.js'

class State {
  constructor(synthParameters) {
    this.instruments = []
    this.selectedInstrumentNumber = null;
    this.tracks = []
    this.defaultSynthParameters = synthParameters;
  }

  addInstrument(settings) {
    const instrument = new Instrument(settings);
    this.instruments.push(instrument);
    return instrument;
  }

  changeInstrumentForTracks(trackIndices, instrument) {
    this.tracks = _.map(this.tracks, (track, index) => {
      if (_.includes(trackIndices, index)) {
        return track.update({
          instrumentNumber: instrument.number,
          name: instrument.name
        });
      } else {
        return track;
      }
    });
  }

  clearInstruments() {
    this.instruments = [];
  }

  createTracksFromNames(names) {
    this.tracks = _.map(names, name =>
      createTrackFromName(name, this.instruments)
    );
  }

  selectedInstrument() {
    return _.find(this.instruments, { number: this.selectedInstrumentNumber });
  }

  track(trackIndex) {
    return this.tracks[trackIndex];
  }

  trackNames() {
    return _.map(this.tracks, 'name');
  }

  updateInstrumentTrackNames(instrumentNumber, trackName) {
    this.tracks = _.map(this.tracks, track => {
      if (track.instrumentNumber == instrumentNumber) {
        return track.update({ name: trackName });
      } else {
        return track;
      }
    });
  }

  updateParameterOfSelectedInstrument(parameterName, parameterValue) {
    let instrument = this.selectedInstrument();
    instrument.parameters = _.map(instrument.parameters, parameter => {
      if (parameter.name != parameterName) {
        return parameter;
      }

      const rescaledValue = scale(
        parameterValue, 0, 100, parameter.minimum, parameter.maximum
      );
      return _.merge({}, parameter, { value: rescaledValue });
    });

    return instrument;
  }

  updateSelectedInstrumentNumber(number) {
    this.selectedInstrumentNumber = number;
  }
}
