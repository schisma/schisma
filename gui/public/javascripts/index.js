import { State } from './state.js'
import { Socket } from './socket.js';
import { Chart } from './chart.js';
import { Spreadsheet } from './spreadsheet.js';
import { MidiKeyboard } from './midi_keyboard.js';

import {
  index as loadIndexBindings,
  chart as loadChartBindings,
  midiInstrument as loadMidiInstrumentBindings,
  spreadsheet as loadSpreadsheetBindings
} from './bindings.js';


const state = new State(window.defaultSynthParameters);
const socket = new Socket(8888);


const chartBindings = loadChartBindings(state, socket);
const synthControlsChart = new Chart(
  'synth-controls-chart', chartBindings.updateSynthParameter
);


const tracker = new Spreadsheet(
  document.getElementById('spreadsheet'),
  [0],
  '----------'
);

const spreadsheetBindings = loadSpreadsheetBindings(
  state, socket, tracker, synthControlsChart
);

tracker.updateCallbacks(spreadsheetBindings)


const midiInstrumentBindings = loadMidiInstrumentBindings(state, socket);


const midiKeyboard = new MidiKeyboard(
  document.getElementById('midi-keyboard'),
  midiInstrumentBindings.forwardMessage
);


loadIndexBindings(state, tracker);
feather.replace();
