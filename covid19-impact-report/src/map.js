import data from '../static/covid.json'

var covidData = data.filter(function(p){return p.Month == 2});

const width = 900;
const height = 600;
const svg = d3.select("div").append("svg")
    .attr("width", width)
    .attr("height", height);

const projection = d3.geoAlbersUsa()
    .translate([width / 2, height / 2]) // translate to center of screen
    .scale([1000]); // scale things down so see entire US

const path = d3.geoPath().projection(projection);

const colorScale = d3.scaleLinear()
    .domain([0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000])
    //.domain([0,166666.7,333333.3,500000,666666.7,833333.5,1000000])
    .range(["#ffffff", "#fcffa1", "#fbf544", "#fbb844", "#ff4d00", "#ff0000", "#c20404", "#941010"]);

console.log(colorScale.domain().slice());
Number.prototype.round = function (decimals) {
    return Number((Math.round(this + "e" + decimals) + "e-" + decimals));
};

const tooltip = d3.select(".map").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

function range(start, count) {
    return Array.apply(0, Array(count + 1))
        		    .map(function (element, index) {
          		        return index + start;
    });
}

d3.json("https://gist.githubusercontent.com/Bradleykingz/3aa5206b6819a3c38b5d73cb814ed470/raw/a476b9098ba0244718b496697c5b350460d32f99/us-states.json", function(error, uState) {
    if (error) throw error;
    _(uState.features)
        .keyBy('properties.name')
        .merge(_.keyBy(covidData, 'Province_State'))
        .values()
        .value();

    svg.selectAll('path')
        .data(uState.features)
        .enter()
        .append('path')
        .attr("d", path)
        .attr('class', 'state')
        .style('fill', function (d, i) {
            let uConfirmed = d.Confirmed;
            //console.log(uConfirmed);
            return uConfirmed ? colorScale(uConfirmed) : "#ccc";
        })
        .on('mousemove', function (d) {
            tooltip.transition()
                    .duration(200)
                    .style("opacity", .9);
            //Any time the mouse moves, the tooltip should be at the same position
            tooltip.style("left", (d3.event.pageX) + "px")
                    .style("top", (d3.event.pageY) + "px")
                    //The text inside should be State: rate%
                    .text(()=> `${d.Province_State}: ${(d.Confirmed).round(0)}`)
        })
});

//create a new SVG in the body
const legend = d3.select("body").append('svg')
    .attr('class', 'legend')
    .attr('width', 180)
    .attr('height', 180)
    .selectAll('g')
    .data(colorScale.domain().slice().reverse())
    .enter().append('g')
    .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });
//Inside every 'legend', insert a rect
legend.append("rect")
    .attr("width", 18)
    .attr("height", 18)
    .style("fill", colorScale);
legend.append("text")
    .attr("x", 24)
    .attr("y", 9)
    .attr("dy", ".35em")
    .text(function(d) { return `${(d).round(0)}`});
