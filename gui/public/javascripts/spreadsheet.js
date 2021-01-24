export { Spreadsheet };

class Spreadsheet {
  constructor(spreadsheetElement, readOnlyColumnIndices, rowSeparator) {
    this.rowSeparator = rowSeparator;
    this.spreadsheet = this._createSpreadsheet(
      spreadsheetElement, readOnlyColumnIndices
    );

    this.exportPlugin = this.spreadsheet.getPlugin('exportFile');

    this.renumberRows(0);
  }

  addSeparator(row) {
    const cols = this.spreadsheet.countCols();
    for (let i = 0; i <= cols - 1; i++) {
      this.spreadsheet.setDataAtCell(row, i, this.rowSeparator, 'ignore')
    }
  }

  blur() {
    this.spreadsheet.deselectCell();
    this.spreadsheet.unlisten();
  }

  _createSpreadsheet(spreadsheetElement, readOnlyColumnIndices) {
    const spreadsheet = new Handsontable(spreadsheetElement, {
      licenseKey: 'non-commercial-and-evaluation',
      data: [],
      colHeaders: [],
      cells: function (row, col) {
        const cellProperties = {};

        if (_.includes(readOnlyColumnIndices, col)) {
          cellProperties.readOnly = true;
        }

        return cellProperties;
      },
      contextMenu: true,
      hiddenColumns: {
        indicators: true
      },
      minSpareRows: 1,
      height: '47vh',
      width: '100%'
    });

    return spreadsheet;
  }

  exportAsCsv() {
    return this.exportPlugin.exportAsString('csv', {
      bom: false,
      columnDelimiter: ',',
      columnHeaders: false,
      exportHiddenColumns: false,
      exportHiddenRows: false,
      rowDelimiter: "\n",
      rowHeaders: false
    });
  }

  _lastRowNumber(lastRowIndex) {
    const rowNumbers = _.flatten(
      this.spreadsheet.getData(0, 0, lastRowIndex - 1, 0).reverse()
    );
    const finalNumber = _.parseInt(_.find(rowNumbers, function(number) {
      return _.isInteger(_.parseInt(number));
    }));

    if (finalNumber === 0) {
      return 0;
    } else {
      return finalNumber || -1;
    }
  }

  renumberRows(startIndex) {
    let rowNumber = 0;

    if (startIndex > 0) {
      rowNumber = this._lastRowNumber(startIndex) + 1;
    }

    const endIndex = this.spreadsheet.countRows() - 2;
    const cells = [];
    for (let i = startIndex; i <= endIndex; i++) {
      const data = this.spreadsheet.getDataAtCell(i, 1);
      if (data != this.rowSeparator) {
        cells.push([i, 0, rowNumber]);
        rowNumber = rowNumber + 1;
      }
    }

    this.spreadsheet.setDataAtCell(cells, 'ignore');
  }

  startAndEndRows() {
    const selected = this.spreadsheet.getSelected();
    let start = 0;
    if (selected) {
      start = _.parseInt(this.spreadsheet.getDataAtCell(selected[0][0], 0));
    }

    const end = this._lastRowNumber(this.spreadsheet.countRows() - 1);

    return { start: start, end: end };
  }

  updateCallbacks(callbacks) {
    this.spreadsheet.updateSettings({
      afterBeginEditing: callbacks.afterBeginEditing,
      afterChange: callbacks.afterChange,
      afterCreateRow: callbacks.afterCreateRow,
      afterRemoveRow: callbacks.afterRemoveRow,
      afterSelection: callbacks.afterSelection
    });

    Handsontable.hooks.add('afterDocumentKeyDown', callbacks.onKeyDown);
  }

  updateSpreadsheetContextMenu(contextMenuItems) {
    this.spreadsheet.updateSettings({
      contextMenu: {
        items: Handsontable.plugins.ContextMenu.DEFAULT_ITEMS.concat(
          contextMenuItems
        )
      }
    });
  }

  updateSpreadsheetData(rows) {
    this.spreadsheet.loadData(rows);
  }

  updateSpreadsheetHeaders(headers) {
    this.spreadsheet.updateSettings({ colHeaders: headers });
  }
}
