import covidData from '../static/covid19_us.csv'    // import dataset
"use strict";     // the code should be executed in "strict mode".
                  // With strict mode, you can not, for example, use undeclared variables

var line_svg;    // used for svg later
var colorSet;    // used for color scheme later
var confirmedArray = [];   // used to store data later

// preparation for our svg
var margin = { top: 50, right: 35, bottom: 50, left: 50};
var w = 1200 - (margin.left + margin.right);
var h = 520 - (margin.top + margin.bottom);
var legendSpace = 130;
console.log(margin);

// preparation for our x/y axis
var states = [];
// change the numerical x axis to names of states
var formatStates = function(d) {
    return states[d % 56];
}

var y = d3.scaleLinear()
          .domain([0, d3.max(covidData, d => parseInt(d.Confirmed))])
          .range([h, 0]).nice();
var x = d3.scaleLinear()
          .domain([0, 59])
          .range([0, w]).nice();
var yAxis = d3.axisLeft(y);
var xAxis = d3.axisBottom(x).tickFormat(formatStates);
  //.tickFormat(d3.timeFormat("%b"));   // %b: abbreviated Month format (Mon, Jun..)

var convidArray = [];

console.log(d3.max(convidArray, d => +d["Confirmed"]));

// once finish processing data, make a graph!
d3.csv(covidData).then(function(data) {
  data.forEach(function(d){
    convidArray.push(d);
    if (!states.includes(d.Province_State)) {
      states.push(d.Province_State);
    }
  })

  //print out all states in the console, check with inspect
  console.log(states);
  drawLineD3();
});

function drawLineD3() {
  colorSet = d3.scaleOrdinal()
               .domain(states)
               .range(d3.schemeSet2);
  //x.domain(d3.extent(confirmedArray, d => d.Province_State));
  //x.domain([1,56]);
  //y.domain(d3.extent(confirmedArray, d => parseFloat(d.Confirmed)));

  // create our svg
  line_svg = d3.select('#d3-demo')
              .append('svg')
                .attr("id", "line-chart")
                .attr("width", w + margin.left + margin.right + legendSpace)
                .attr("height", h + margin.top + margin.bottom)
              .append('g')
                .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

  // append x axis to svg
  line_svg.append("g")
            .attr("transform", "translate(0," + h + ")")
            .attr("class","myXaxis")
            .call(xAxis);

  // append y axis to svg
  line_svg.append("g")
            .attr("class","myYaxis")
            .call(yAxis);

  // create a group to store lines for our line chart
  var path = line_svg.append('g')
                        .attr("id","paths-group");

  var line = d3.line()
              .x(d => x(d.Province_State))
              .y(d => y(parseFloat(d.Confirmed)));

  // make a line for each city
  states.forEach(function(d) {
      var currentState = confirmedArray.filter(e => e.Province_State === d);
      path.append("path")
            .datum(currentState)
            .attr("class", "lines")
            .attr('d', line)
            .style("stroke-width", 2.5)
            .style("fill", "none")
            .attr("stroke", colorSet(d))
  });

  // add legend
  /*var legend = line_svg.append('g')
                        .attr("id","legend-group");

  legend.selectAll("rect").data(citySet)
      .join("rect")
        .attr("class","legends")
        .attr("x",600)
        .attr("y", d => 25+30*(citySet.indexOf(d)))
        .attr("width", 10)
        .attr("height", 10)
        .style("fill", d => colorSet(d));

  legend.selectAll("text").data(citySet)
      .join("text")
        .attr("class","legends")
        .attr("x", 620)
        .attr("y", d => 30+30*(citySet.indexOf(d)))
        .text(d => d)
        .style("font-size", "15px")
        .attr("alignment-baseline","middle");*/
}
