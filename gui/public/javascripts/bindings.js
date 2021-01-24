export { chart, index, midiInstrument, spreadsheet };

import {
  createMidiControlChangeMessage,
  generateInstrumentsJson,
  generateTrackerCsv,
  htmlToElement,
  toMidiMessage
} from './utilities.js'

// TODO: Mute/solo instruments

const updateInstrumentsJson = function(state) {
  return function() {
    const contents = generateInstrumentsJson(state);
    fetch('/instruments', {
      method: 'put',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        instrumentsFile: document.getElementById('instruments-file').value,
        contents: contents
      })
    }).then(() => {
      // TODO: Some indication of success or failure
    });
  };
};

const updateTrackerCsv = function(state, sheet) {
  return function() {
    const contents = generateTrackerCsv(state, sheet);
    fetch('/tracker', {
      method: 'put',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        trackerFile: document.getElementById('tracker-file').value,
        contents: contents
      })
    }).then(() => {
      // TODO: Some indication of success or failure
    });
  };
};


const chart = function(state, socket) {
  const updateInstrumentsFile = _.debounce(updateInstrumentsJson(state), 1000);

  const updateSynthParameter = function(parameterName, parameterValue) {

    const instrument = state.updateParameterOfSelectedInstrument(
      parameterName, parameterValue
    );

    if (instrument.midiChannel > 0) {
      const midiMessage = createMidiControlChangeMessage(
        instrument, parameterName
      );
      socket.sendMidiMessage(midiMessage);
    }

    updateInstrumentsFile();
  };

  return {
    updateSynthParameter: updateSynthParameter
  }
};

const index = function(state, tracker) {
  const instrumentTemplate = _.template(
    document.getElementById('instrument-template').innerHTML
  );

  const instrumentsNode = document.getElementById('instruments');

  const updateInstrumentsFile = _.debounce(updateInstrumentsJson(state), 1000);


  const activateNavLink = function(event) {
    document.querySelectorAll('.nav-link').forEach(function(navLink) {
      navLink.classList.remove('active');
    });

    event.target.classList.add('active');
  };

  const activatePanel = function(event) {
    document.querySelectorAll('.panel').forEach(function(panel) {
      panel.classList.add('hidden');
    });

    const panelId = event.target.getAttribute('data-target');
    const targetPanel = document.getElementById(panelId);
    targetPanel.classList.remove('hidden')
  };

  const addInstrument = function(settings) {
    const instrument = state.addInstrument(settings);
    const html = instrumentTemplate({
      instrument: instrument,
      synths: _.keys(state.defaultSynthParameters)
    });

    const element = htmlToElement(html);
    instrumentsNode.appendChild(element);

    const instrumentName = element.querySelector('.instrument-name');
    instrumentName.addEventListener('change', function(event) {
      const name = event.target.value

      instrument.update({ name: name });
      updateInstrumentsFile();

      state.updateInstrumentTrackNames(instrument.number, name);
      tracker.updateSpreadsheetHeaders(state.trackNames());
      tracker.updateSpreadsheetContextMenu(spreadsheetContextMenuItems());
    });

    const synthSelection = element.querySelector('.synth-selection');
    synthSelection.addEventListener('change', function(event) {
      instrument.update({ instrument: event.target.value });
      toggleSoundFontPath(event.target);
      updateInstrumentsFile();
    });

    const soundFontPath = element.querySelector('.soundfont-path');
    soundFontPath.addEventListener('change', function(event) {
      instrument.update({ soundFontPath: event.target.value });
      updateInstrumentsFile();
    });

    const midiChannel = element.querySelector('.midi-channel');
    midiChannel.addEventListener('change', function(event) {
      instrument.update({ midiChannel: _.parseInt(event.target.value) });
      updateInstrumentsFile();
    });

    toggleSoundFontPath(synthSelection);
  };

  const clearInstruments = function() {
    while (instrumentsNode.firstChild) {
      instrumentsNode.removeChild(instrumentsNode.firstChild);
    }

    state.clearInstruments();
  };

  const spreadsheetContextMenuItems = function() {
    const selectionCallback = (key, selections, event) => {
      const instrumentNumber = _.parseInt(_.replace(key, 'instrument:', ''));
      const instrument = _.find(state.instruments, { number: instrumentNumber });

      const trackIndices = _.flatMap(selections, selection =>
        _.range(selection.start.col, selection.end.col + 1)
      );
      _.pull(trackIndices, 0, 1);

      state.changeInstrumentForTracks(trackIndices, instrument);

      // setTimeout is needed to prevent a Handsontable error.
      // See https://forum.handsontable.com/t/gh-5727-contextmenu-callback-the-runhooks-method-cannot-be-called/4134/10
      setTimeout(() => tracker.updateSpreadsheetHeaders(state.trackNames()), 1);
    }

    const submenuItems = _.map(state.instruments, instrument => (
      {
        key: `instrument:${instrument.number}`,
        name: instrument.name,
        callback: selectionCallback
      }
    ));

    return [
      '---------',
      {
        key: 'instrument',
        name: 'Instrument',
        submenu: {
          items: submenuItems
        }
      }
    ];
  };

  const toggleSoundFontPath = function(target) {
    const parent = target.closest('.synth-selection-container');
    const soundFontPathContainer = parent.nextElementSibling;
    if (target.value == 'SoundFont') {
      soundFontPathContainer.classList.remove('hidden');
    } else {
      soundFontPathContainer.classList.add('hidden');
    }
  };

  document.addEventListener('click', function(event) {
    const selectors =
      'synth-controls-chart, a.nav-link[data-target="synth-controls"]';
    if (event.target.matches(selectors)) {
      document.getElementById('midi-keyboard').focus();
    }
  });

  document.getElementById('add-instrument').addEventListener('click', function(event) {
    event.preventDefault();

    const highestNumberInstrument = _.maxBy(state.instruments, 'number');
    const number = highestNumberInstrument ? highestNumberInstrument.number : 0;
    const defaultSynthParameters = state.defaultSynthParameters;
    const synth = _.keys(defaultSynthParameters)[0];
    const parameters = _.map(defaultSynthParameters[synth], parameter =>
      _.merge({ value: parameter.defaultValue }, parameter)
    );

    addInstrument({
      number: number + 1,
      name: 'New Instrument',
      instrument: synth,
      soundFontPath: '',
      parameters: parameters,
      midiChannel: 0
    });
  });

  document.getElementById('load-instruments-file').addEventListener('click', function(event) {
    event.preventDefault();

    const queryString = new URLSearchParams({
      file: document.getElementById('instruments-file').value
    });

    fetch('/instruments?' + queryString, {
      method: 'get',
      headers: { 'Content-Type': 'application/json' },
    }).then(
      response => response.json()
    ).then(result => {
      clearInstruments();

      _.each(result, settings => {
        const synthParameters = state.defaultSynthParameters[settings.instrument];
        const newParameters = _.map(synthParameters, parameter =>
          _.merge({
            value: settings.parameters[parameter.name]
          }, parameter)
        );
        settings.parameters = newParameters;
        addInstrument(settings);
      });
    });
  });

  document.getElementById('load-tracker-file').addEventListener('click', function(event) {
    event.preventDefault();

    const queryString = new URLSearchParams({
      file: document.getElementById('tracker-file').value
    });

    fetch('/tracker?' + queryString, {
      method: 'get',
      headers: { 'Content-Type': 'application/json' },
    }).then(
      response => response.json()
    ).then(result => {
      const [trackNames, ...rows] = result;

      state.createTracksFromNames(trackNames);
      tracker.updateSpreadsheetHeaders(state.trackNames())
      tracker.updateSpreadsheetData(rows);
      tracker.updateSpreadsheetContextMenu(spreadsheetContextMenuItems());
    });
  });

  document.querySelectorAll('.nav-link').forEach(function(navLink) {
    navLink.addEventListener('click', function(event) {
      event.preventDefault();
      activateNavLink(event);
      activatePanel(event);
    });
  });
};

const midiInstrument = function(state, socket) {
  const forwardMessage = function(message) {
    const allowedCommands = [
      128, // note off
      144, // note on
      176  // control change
    ]

    const instrument = state.selectedInstrument();

    if (!_.includes(allowedCommands, message[0]) || _.isNil(instrument)) {
      return;
    }

    if (instrument.midiChannel > 0) {
      const midiMessage = toMidiMessage(
        message[0], message[1], message[2], instrument.midiChannel
      );
      socket.sendMidiMessage(midiMessage);
    }
  }

  return {
    forwardMessage: forwardMessage
  }
};

const spreadsheet = function(state, socket, sheet, synthControlsChart) {
  let isEditing = false;
  let updateTrackerFile = _.debounce(updateTrackerCsv(state, sheet), 1000);

  const sendInstrumentParametersAsMidiControlChangeMessages = function() {
    const instrument = state.selectedInstrument();
    if (instrument.midiChannel > 0) {
      _.each(instrument.parameters, parameter => {
        const midiMessage = createMidiControlChangeMessage(
          instrument, parameter.name
        );
        socket.sendMidiMessage(midiMessage);
      });
    }
  };

  const sendMidiChannel = function() {
    const instrument = state.selectedInstrument();
    if (instrument.midiChannel > 0) {
      socket.sendMidiChannel(instrument.midiChannel);
    }
  }


  const afterBeginEditing = function() {
    isEditing = true;
  };

  const afterChange = function(changes, source) {
    if (source === 'loadData') {
      return;
    }

    if (source != 'ignore') {
      const cell = changes[0];
      const col = cell[1];
      const rowIndex = parseInt(cell[0], 10);
      const text = cell[3];

      if (col == '1' && text == '--') {
        sheet.addSeparator(rowIndex);
      }

      sheet.renumberRows(rowIndex);
      updateTrackerFile();
    }

    if (source == 'edit') {
      isEditing = false;
    }
  };

  const afterCreateRow = function(rowIndex, amount, source) {
    if (source == 'ContextMenu.rowAbove') {
      sheet.renumberRows(rowIndex - 1);
      updateTrackerFile();
    } else if (source == 'ContextMenu.rowBelow') {
      sheet.renumberRows(rowIndex);
      updateTrackerFile();
    }
  };

  const afterRemoveRow = function(rowIndex, amount, removedRows, source) {
    if (source == 'ContextMenu.removeRow') {
      sheet.renumberRows(rowIndex);
      updateTrackerFile();
    }
  };

  const afterSelection = function(row, column, row2, column2) {
    if (column != column2 || column == 0 || column == 1) {
      return false;
    }

    const track = state.track(column);
    const instrument = _.find(state.instruments, { number: track.instrumentNumber });
    state.updateSelectedInstrumentNumber(track.instrumentNumber);
    synthControlsChart.updateChart(instrument.name, instrument.parameters);

    sendMidiChannel();
    sendInstrumentParametersAsMidiControlChangeMessages();
  };

  const play = function(startLine, endLine) {
    fetch('/play', {
      method: 'put',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: new URLSearchParams({
        trackerFile: document.getElementById('tracker-file').value,
        instrumentsFile: document.getElementById('instruments-file').value,
        start: startLine,
        end: endLine
      })
    }).then(() => {
      sendInstrumentParametersAsMidiControlChangeMessages();
    });
  };

  const onKeyDown = function(event) {
    if (event.code == 'F5') {
      event.preventDefault();

      const rowNumbers = sheet.startAndEndRows();
      play(rowNumbers.start, rowNumbers.end);
    }

    if (event.code == 'F6') {
      event.preventDefault();

      fetch('/stop', { method: 'put' });
    }

    if (event.code == 'F7') {
      event.preventDefault();

      play(-1, -1);
    }

    if (event.key == 'Escape') {
      if (!isEditing) {
        sheet.blur();
        document.getElementById('midi-keyboard').focus();
      }

      if (isEditing) {
        isEditing = false;
      }
    }

    if (event.altKey == true) {
      if (event.key == 'M') {
        // TODO: Mute
      }
      if (event.key == 'S') {
        // TODO: Solo
      }
    }

  };

  return {
    afterBeginEditing: afterBeginEditing,
    afterChange: afterChange,
    afterCreateRow: afterCreateRow,
    afterRemoveRow: afterRemoveRow,
    afterSelection: afterSelection,
    onKeyDown: onKeyDown
  };
};
