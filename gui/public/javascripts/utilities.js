export {
  createMidiControlChangeMessage,
  createTrackFromName,
  generateInstrumentsJson,
  generateTrackerCsv,
  htmlToElement,
  toMidiMessage,
  scale
};

import { Track } from './track.js';

const createMidiControlChangeMessage = function(instrument, parameterName) {
  const parameter = _.find(instrument.parameters, { name: parameterName });
  const value = scale(
    parameter.value, parameter.minimum, parameter.maximum, 0, 127
  );

  const midiMessage = toMidiMessage(
    176, parameter.midiCcNumber, value, instrument.midiChannel
  );

  return midiMessage;
}

const createTrackFromName = function(trackName, instruments) {
  const settings = parseTrackName(trackName, instruments);
  return new Track(settings);
}

const generateInstrumentsJson = function(state) {
  const instruments = _.map(state.instruments, instrument => (
    {
      number: instrument.number,
      name: instrument.name,
      instrument: instrument.instrument,
      soundFontPath: instrument.soundFontPath,
      // TODO: What about amplitude, frequency?
      parameters: _.reduce(instrument.parameters, function(object, parameter) {
        object[parameter.name] = parameter.value;
        return object;
      }, {}),
      midiChannel: instrument.midiChannel,
    }
  ));

  return JSON.stringify({
    instruments: instruments,
    // TODO: Expose these
    parameterRenamings: {
      aa: 'amplifierAttack',
      rel: 'amplifierRelease',
      amp: 'amplitude'
    }
  })
};

const generateTrackerCsv = function(state, sheet) {
  const header = _.map(state.tracks, (track) => track.csvName());
  const csvHeader = Papa.unparse([header]);
  const rows = sheet.exportAsCsv().replace(/\n.*$/, '');
  const csv = `${csvHeader}\n${rows}`;
  return csv;
}

const htmlToElement = function(html) {
  const template = document.createElement('template');
  html = html.trim();
  template.innerHTML = html;
  return template.content.firstChild;
}

const instrumentName = function(instrumentNumber, instruments) {
  const instrument = _.find(instruments, { number: instrumentNumber });
  return instrument.name;
}

const toMidiMessage = function(command, noteNumber, value, channel) {
  let data = new Uint8Array([
    (command + channel - 1),
    noteNumber,
    value
  ]);

  return { data: data, timestamp: 0 }
}

const parseTrackName = function(trackName, instruments) {
  let template = {
    instrumentNumber: null,
    name: trackName,
    mute: false,
    solo: false
  }

  if (trackName == '#' || trackName == 'Master') {
    return template;
  }

  if (_.startsWith(trackName, 'M')) {
    return _.merge(
      parseTrackName(trackName.substring(1), instruments),
      { mute: true }
    );
  } else if (_.startsWith(trackName, 'S')) {
    return _.merge(
      parseTrackName(trackName.substring(1), instruments),
      { solo: true }
    );
  } else if (_.startsWith(trackName, 'I')) {
    let withoutPrefix = trackName.substring(1);
    let instrumentNumber = _.parseInt(withoutPrefix.split(' ')[0]);
    return _.merge(template, {
      instrumentNumber: instrumentNumber,
      name: instrumentName(instrumentNumber, instruments)
    });
  } else {
    return template;
  }
}

const scale = function(value, lowerBound, upperBound, scaledMin, scaledMax) {
  return (value - lowerBound)
    * (scaledMax - scaledMin)
    / (upperBound - lowerBound)
    + scaledMin;
}
