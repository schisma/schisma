export { Chart };

import { scale } from './utilities.js'

class Chart {
  constructor(selector, dragCallback) {
    this.chart = this._createChart(selector, dragCallback);
  }

  _createChart(selector, dragCallback) {
    return Highcharts.chart(selector, {
      chart: {
        type: 'spline',
      },
      credits: {
        enabled: false
      },
      legend: {
        enabled: false
      },
      title: {
        text: ''
      },
      plotOptions: {
        spline: {
          cursor: 'ns-resize'
        },
        series: {
          dragDrop: {
            draggableY: true
          },
          point: {
            events: {
              drag: function (event) {
                dragCallback(this.name, event.newPoint.y);
              }
            }
          }
        }
      },
      xAxis: {
        categories: []
      },
      yAxis: {
        endOnTick: false,
        min: 0,
        max: 100
      },
      series: [{}]
    });
  }

  updateChart(name, parameters) {
    const yAxisMin = 0;
    const yAxisMax = 100;

    const data = _.map(parameters, (parameter) => {
      return {
        y: scale(
          parameter.value,
          parameter.minimum,
          parameter.maximum,
          yAxisMin,
          yAxisMax
        ),
        dragDrop: {
          dragMinY: scale(
            parameter.minimum,
            parameter.minimum,
            parameter.maximum,
            yAxisMin,
            yAxisMax
          ),
          dragMaxY: scale(
            parameter.maximum,
            parameter.minimum,
            parameter.maximum,
            yAxisMin,
            yAxisMax
          ),
          dragPrecisionY: scale(
            parameter.step,
            0,
            Math.abs(parameter.minimum) + Math.abs(parameter.maximum),
            0,
            Math.abs(yAxisMin) + Math.abs(yAxisMax)
          )
        },
        name: parameter.name
      };
    });

    this.chart.update({
      title: {
        text: `${name} Parameters`
      },
      xAxis: {
        categories: _.map(parameters, 'displayName')
      },
      series: [
        {
          data: data,
          name: 'Settings'
        }
      ]
    });
  }
}
