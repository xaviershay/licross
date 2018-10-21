const gridSize = 2

data = []
for (let x = 0; x < gridSize; x++) {
  for (let y = 0; y < gridSize; y++) {
    let content = {"score": 0, "bonus": "none"}

    if (x == 0 && y == 0) {
      content = {"bonus": "TW"}
    } else if (x == 1 && y == 1) {
      content = {"bonus": "DW"}
    } else if (x == 3 && y == 3) {
      content = {"bonus": "TL"}
    } else if (x == 4 && y == 0) {
      content = {"bonus": "DL"}
    } else if (x == 7 && y == 7) {
      content = {"bonus": "★"}
    } else if (x == 1 && y == 0) {
      content = {"letter": "X", "score": 8}
    } else if (x == 3 && y == 6) {
      content = {"letter": "A", "score": 1}
    } else if (x == 4 && y == 6) {
      content = {"letter": "W", "score": 4, "bonus": "TW"}
    } else if (x == 3 && y == 7) {
      content = {"letter": "Q", "score": 10}
    } else if (x == 3 && y == 8) {
      content = {"letter": "Z", "score": 10}
    }
    data.push(
      deepmerge({"key": x + "," + y, "xPos": x, "yPos": y, "bonus": "none"}, content)
    )
  }
}
function indexFor(x, y) {
  return x * gridSize + y
}

// gold #d59120
// green #759900
// red #ac524f
// another green #92B166
// background #DEE3E6
const BONUS_COLORS = {
  "none": "#738896",
  "TW": "#ac524f",
  "DW": "#ef928f",
  "TL": "#759900",
  "DL": "#69D2E7",
  "★": "#cc12bf",
  "letter": "#d59120"
}

// https://www.colourlovers.com/palette/92095/Giant_Goldfish
const BONUS_COLORS_OLD = {
  "none": "#fff9e5",
  "TW": "#FA6900",
  "TL": "#F38630",
  "DW": "#69D2E7",
  "DL": "#A7DBD8",
  "★": "#69D2E7",
  "letter": "tomato"
}

const DEFAULT_KERNING = {
  "letter": {"x": 0.50, "y": 0.55},
  "score": {"x": 0.86, "y": 0.86}
}
const KERNING = {
  /*
  "Z": {
    "letter": {"x": 0.42},
    "score": {"x": 0.82}
  },
  "Q": {
    "letter": {"x": 0.42},
    "score": {"x": 0.82}
  }*/
}

const getKerning = letter => {
  var kerning = KERNING[letter] || DEFAULT_KERNING

  return deepmerge(DEFAULT_KERNING, kerning)
}

// 1. Fill out the grid.
// 2. Experiment with colors
// 3. Drag and drop

    //kerning[d.letter]
var board = d3.select("#board")
  .append("svg")
  .attr("viewBox", "0 0 15 15")

var deltaX, deltaY;

window.data = data

window.doUpdate = function(data) {
  let cell = board.selectAll(".cell").data(data, d => d.key)

  let cellEnter = cell.enter().append("g")
    .attr("class", "cell")

  let spaceEnter = cellEnter.append("svg")
    .attr("class", "space")
    .attr("viewBox", "0 0 1 1")
    .attr("width", 1)
    .attr("height", 1)
  //cell.merge(cellEnter)
    .attr("x", d => d.xPos)
    .attr("y", d => d.yPos)

// BASE SPACE
// Always render this
spaceEnter.append("rect")
  .attr("width", 1)
  .attr("height", 1)
  .attr("fill", d => BONUS_COLORS[d.bonus])
  .attr("shape-rendering", "crispEdges")
  .attr("stroke-width", 0.02)
  .attr("stroke", "#1a1a1a")
  .attr("opacity", d => d.bonus == "none" ? "0.5" : "1.0")

spaceEnter.filter(d => d.bonus && d.bonus != "none").append("text")
  .html(d => d.bonus)
  .attr("x", d => 0.50)
  .attr("y", d => 0.53)
  .attr("font-size", 0.45)
  .attr("fill", "white")
  .attr("text-anchor", "middle")
  .attr("alignment-baseline", "middle")

// LETTER
  let letter = cell.select(".letter")
  let letterEnter = cellEnter.append("svg")
    .attr("class", "letter")
    .attr("viewBox", "0 0 1 1")
    .attr("width", 1)
    .attr("height", 1)
    .attr("x", d => d.xPos)
    .attr("y", d => d.yPos)
    .call(d3.drag()
      .on("start", function(d) {
        current = d3.select(this).raise().raise()
        deltaX = current.attr("x") - d3.event.x
        deltaY = current.attr("y") - d3.event.y
      })
      .on("drag", function(d) {
        d3.select(this)
          .attr("x", d3.event.x + deltaX)
          .attr("y", d3.event.y + deltaY)
      })
      .on("end", function(d) {
        let x = Math.floor(d3.event.x);
        let y = Math.floor(d3.event.y);
        console.log(x, y)
        let target = data[indexFor(x, y)]
        console.log(target)
 
        oldx = d.xPos
        oldy = d.yPos

        // We aren't actually moving the element we were dragging, instead
        // we're changing the value of a target one. As such, snap the dragged
        // element back to where it came from.
        d3.select(this)
          .attr("x", oldx)
          .attr("y", oldy)

        if (target.letter) {
        } else {
          target.letter = d.letter
          target.score = d.score
          d.score = 0
          d.letter = undefined
        }
        target = data[indexFor(x, y)]
        doUpdate(data)
      })
    )
  letter.merge(letterEnter)
    .attr("visibility", d => d.letter ? "visible" : "hidden") // TODO: Update

letterEnter.append("rect")
  .attr("width", 1)
  .attr("height", 1)
  .attr("fill", d => BONUS_COLORS["letter"])
  .attr("shape-rendering", "crispEdges")
  .attr("stroke-width", 0.02)
  .attr("stroke", "#1a1a1a")

let contentEnter = letterEnter.append("text")
  .attr("class", "content")
  .attr("x", d => getKerning(d.letter)["letter"]["x"])
  .attr("y", d => getKerning(d.letter)["letter"]["y"])
  .attr("font-size", 0.80)
  .attr("fill", "white")
  .attr("text-anchor", "middle")
  .attr("alignment-baseline", "middle")

letter.select("text.content").merge(contentEnter)
  .html(d => d.letter)

// SCORE
let scoreEnter = letterEnter.append("text")
  .attr("class", "score")
  .attr("x", d => getKerning(d.letter)["score"]["x"])
  .attr("y", d => getKerning(d.letter)["score"]["y"])
  .attr("fill", "white")
  .attr("text-anchor", "middle")
  .attr("alignment-baseline", "middle")
  .attr("font-size", 0.20)
  .attr("letter-spacing", "-0.02")

letter.select("text.score").merge(scoreEnter)
  .html(d => d.score)
}
window.doUpdate(data)
