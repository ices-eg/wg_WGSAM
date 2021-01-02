var binding = new Shiny.OutputBinding();

binding.find = function (scope) {
  return $(scope).find(".Dragable");
};

binding.renderValue = function (el, data) {
  const width = data.width ?
    parseInt(data.width) :
    parseInt(getComputedStyle(document.querySelector(".Dragable")).width);
  const height = width * 1.2;
  const margin = { top: 20, right: 10, bottom: 130, left: 60 },
    boxWidth = width - margin.left - margin.right,
    boxHeight = height - margin.top - margin.bottom;

  // Remove previous svg
  d3.select(".Dragable > svg")
    .remove()
    .html("");
  // Make new svg
  var box = d3.select(el)
    .append('svg')
    .attr('class', 'box')
    //.attr("preserveAspectRatio", "xMinYMin meet")
    //.attr("viewBox", "0 0 850 800")
    .attr('width', width)
    .attr('height', height);

  // Scaling functions (from data to pixels)
  var y = d3.scaleLinear()
    .domain([0, 2])
    .range([boxHeight - margin.bottom - margin.top, margin.top]);
  var x = d3.scaleBand()
    .domain(data.data.map(i => i.x))
    .paddingInner(0)
    .range([margin.left, boxWidth - margin.right])
    .align(0.5)

  // Axes
  var y_axis = d3.axisLeft(y).tickFormat(d3.format('.2')).ticks(4);
  box.append('g')
    .attr("transform", "translate(" + (margin.left + 1) + ",0)")
    .call(y_axis)
    .attr("font-size", "1em")
    .style('color', 'black')
    .attr('class', 'y-axis');

  const ticks = x.domain();
  const x_axis = d3.axisBottom(x).tickValues(ticks);
  const hh = boxHeight - margin.bottom - margin.top + 5;
  box.append('g')
    .attr("transform", "translate(0," + hh + ")")
    .call(x_axis)
    .attr("font-size", "1em")
    .attr('class', 'x-axis')
    .style('color', 'black')
    .selectAll('text')
    .attr("transform", "rotate(90)")
    .attr("y", 0)
    .attr("x", 9)
    .attr("dy", "0.35em")
    .style("text-anchor", "start");

  var brushY = d3.brushY()
    .extent(function (d, i) {
      return [[x(d.x) + x.step() / 4, 0],
      [x(d.x) + 3 * x.step() / 4, boxHeight]];

    })
    .on("brush", brushmoveY)
    .on("end", brushendY);


  // Update function
  function update() {
  svgbrushY
    .select('text').remove().html("")
  svgbrushY
    .append('text')
    .attr('y', function (d) { return y(d.y) + 25; })
    .attr('x', function (d, i) { return x(d.x) + x.step() / 2; })
    .attr('dx', '-.35em')
    .attr('dy', -5)
    .style('fill', '#123456')
    .text(function (d) { return d3.format('.2')(d.y); });

    svgbrushY
      .call(brushY.move, function (d) {
        return [d.y, 0].map(y);
      })
      .selectAll('text')
      .attr('y', function (d) { return Math.max(Math.min(y(d.y) + 25, boxHeight - margin.bottom - 25), 25) })
      .text(function (d) { return d3.format('.2')(d.y); });
    // Add labels

  }

  // Bars - using brush to make them resizable
  var svgbrushY = box
    .selectAll('.brush')
    .data(data.data)
    .enter()
    .append('g')
    .attr('class', 'brush')
    .append('g')
    .call(brushY)
    .call(brushY.move, function (d) { return [d.y, 0].map(y); });

  // Baseline line
  box.append('line')
    .attr('x1', margin.left).attr('x2', boxWidth - margin.right)
    .attr('y1', y(1)).attr('y2', y(1))
    .attr('stroke-opacity', "0.5")
    .style("stroke", "rgb(189, 189, 189)");

  // Baseline text
  box.append('text')
    .attr('x', boxWidth)
    .attr('y', y(1))
    .attr('dx', '-4em')
    .attr('dy', '-.1em')
    .style('fill', 'lightgrey')
    .text('Baseline')
    .attr("transform", "rotate(90)");

  // Y-axis label
  box.append("text")
    .attr("transform", "rotate(-90)")
    .attr("y", margin.right)
    .attr("x", 0 - ((boxHeight - margin.top - margin.bottom) / 2))
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .text("Effort multiplier");

  function brushendY(event) {
    if (!event.sourceEvent) return;
    if (event.sourceEvent.type === "brush") return;
    if (!event.selection) { // just in case of click with no move
      svgbrushY
        .call(brushY.move, function (d) {
          return [d.y, 0].map(y);
        });
      svgbrushY.select('text').remove().html("")
    }
    Shiny.onInputChange("rv", data.data.map(v => v.y));
  }

  function brushmoveY(event) {
    if (!event.sourceEvent) return;
    if (event.sourceEvent.type === "brush") return;
    if (!event.selection) return;

    var d0 = event.selection.map(y.invert);
    var d = d3.select(this).select('.selection');

    if (d0[0] > 2) d0[0] = 2;
    if (d0[0] < 0.2) d0[0] = 0.2;

    d.datum().y = d0[0]; // Change the value of the original data
    if (data.sameval) d3.selectAll('.brush').each((d, i) => d.y = d0[0])

    update();
  }
};

// Regsiter new Shiny binding
Shiny.outputBindings.register(binding, "shiny.Dragable");