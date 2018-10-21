const gridSize = 15
const BONUS_COLORS = {
  "none": "#738896",
  "TW": "#ac524f",
  "DW": "#ef928f",
  "TL": "#759900",
  "DL": "#69D2E7",
  "★": "#cc12bf",
  "letter": "#d59120"
}

boardData = []
for (let x = 0; x < gridSize; x++) {
  for (let y = 0; y < gridSize; y++) {
    let content = {"bonus": "none"}

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
    }
    boardData.push(
      deepmerge({"key": x + "," + y, "xPos": x, "yPos": y, "bonus": "none"}, content)
    )
  }
}

tileData = [
  {"id": 1, "letter": "Z", "score": 10, "location": ["rack", 0], "moveable": true},
  {"id": 2, "letter": "Q", "score": 10, "location": ["board", [5, 0]], "moveable": false},
  {"id": 3, "letter": "Q", "score": 10, "location": ["board", [5, 0]], "moveable": false},
  {"id": 4, "letter": "X", "score": 8, "location": ["board", [5, 1]], "moveable": false},
]
   // } else if (x == 1 && y == 0) {
   //   content = {"letter": "X", "score": 8}
   // } else if (x == 3 && y == 6) {
   //   content = {"letter": "A", "score": 1}
   // } else if (x == 4 && y == 6) {
   //   content = {"letter": "W", "score": 4, "bonus": "TW"}
   // } else if (x == 3 && y == 7) {
   //   content = {"letter": "Q", "score": 10}
   // } else if (x == 3 && y == 8) {
   //   content = {"letter": "Z", "score": 10}
   // }

var tileSize = 0;
var borderWidth = 2;

const gridWidth = 15

function tilesToPixels(n) {
  return tileSize * n + borderWidth * n
}
function redraw() {
  // TODO: Remove any existing nodes
  d3.select("svg").remove()

  boardSize = tilesToPixels(gridWidth)
  gutter = tileSize
  rackHeight = tilesToPixels(1) + borderWidth
  rackWidth = tilesToPixels(7) + borderWidth

  containerWidth = boardSize + borderWidth
  containerHeight = boardSize + gutter + rackHeight
  containerColor = '#333'

  let container = d3.select("#board").append('svg')
    .attr('width', containerWidth)
    .attr('height', containerHeight)

  container.append('rect')
    .attr('fill', containerColor)
    .attr('width', containerWidth)
    .attr('height', containerHeight)

  /*
  let rack = container.append('rect')
      .attr('fill', BONUS_COLORS["none"])
      .attr('x', (containerWidth - rackWidth) / 2)
      .attr('y', boardSize + gutter)
      .attr('width', rackWidth)
      .attr('height', rackHeight)
      */

  let cell = container.selectAll('.space').data(boardData).enter()
    .append('g')
      .attr('class', 'space')
      .attr('transform', d => "translate(" + tilesToPixels(d.xPos) + "," + tilesToPixels(d.yPos) + ")")

  cell.append('rect')
    .attr('fill', d => BONUS_COLORS[d.bonus])
    .attr('width', tileSize)
    .attr('height', tileSize)
    .attr('x', borderWidth)
    .attr('y', borderWidth)

  cell.filter(d => d.bonus != "none").append('text')
    .attr("x", borderWidth + tileSize * 0.5)
    .attr("y", borderWidth + tileSize * 0.53)
    .attr("font-size", tileSize * 0.45)
    .attr("fill", "white")
    .attr("text-anchor", "middle")
    .attr("alignment-baseline", "middle")
    .html(d => d.bonus)

  update(tileData)
}

function update(tileData) {
  let container = d3.select("#board").select('svg')

  let tiles = container.selectAll('.tile').data(tileData, d => d.id)

  function tileLocation(loc) {
    switch (loc[0]) {
      case "board":
        return [tilesToPixels(loc[1][0]), tilesToPixels(loc[1][1])]
      case "rack":
        return [tilesToPixels(loc[1]) + (containerWidth - rackWidth) / 2, boardSize + gutter + borderWidth]
      default: console.log("Unknown location: " + loc)
    }
  }

  let deltaX, deltaY;
  let transformF = d => {
    const x = tileLocation(d.location)[0]
    const y = tileLocation(d.location)[1]
    return "translate(" + x + "," + y + ")"
  }
  let tilesEnter = tiles.enter()
    .append('g')
      .attr('class', 'tile')
      .call(d3.drag()
        .on("start", function(d) {
          current = d3.select(this).raise()
          deltaX = tileLocation(d.location)[0] - d3.event.x
          deltaY = tileLocation(d.location)[1] - d3.event.y
        })
        .on("drag", function(d) {
          x = d3.event.x + deltaX
          y = d3.event.y + deltaY
          xPos = Math.floor(x / (tileSize + 1))
          yPos = Math.floor(y / (tileSize + 1))
          d3.select(this)
            .attr('transform', d => "translate(" + x + "," + y + ")")
        })
        .on("end", function(d) {
          // If on board and placeable, put it there, otherwise back to the
          // rack.
          // TODO: Allow rack re-arranging
          x = d3.event.x;
          y = d3.event.y;

          if (x <= boardSize && y <= boardSize) {
            xPos = Math.floor(x / (tileSize + 1))
            yPos = Math.floor(y / (tileSize + 1))

            // If not, move tile to that space by updating data.
            if (xPos >= 0 && xPos < gridSize && yPos >= 0 && yPos < gridSize) {
              // Check if space is occupied
              let existing = tileData.find(d => {
                const l = d.location
                return l[0] == "board" && l[1][0] == xPos && l[1][1] == yPos
              })
              if (!existing) {
                d.location = ["board", [xPos, yPos]]
              }
            }
          }
          update(tileData)
        })
      )
      .attr('transform', transformF)

  tiles.merge(tilesEnter)
      .transition()
      // This transform is duplicated in enter selection, so that the initial
      // placement doesn't animate, only subsequent updates.
      .attr('transform', transformF)

  tilesEnter.append('rect')
      .attr('fill', containerColor)
      .attr('width', tileSize + borderWidth * 2)
      .attr('height', tileSize + borderWidth * 2)

  tilesEnter.append('rect')
      .attr('fill', BONUS_COLORS["letter"])
      .attr('x', borderWidth)
      .attr('y', borderWidth)
      .attr('width', tileSize)
      .attr('height', tileSize)

  let contentEnter = tilesEnter.append('text')
    .attr("class", "content")
    .attr("x", borderWidth + tileSize * 0.5)
    .attr("y", borderWidth + tileSize * 0.55)
    .attr("font-size", tileSize * 0.8)
    .attr("fill", "white")
    .attr("text-anchor", "middle")
    .attr("alignment-baseline", "middle")

  tiles.select('.content').merge(contentEnter)
    .html(d => d.letter)

  let scoreEnter = tilesEnter.append('text')
    .attr("class", "score")
    .attr("x", borderWidth + tileSize * 0.86)
    .attr("y", borderWidth + tileSize * 0.86)
    .attr("fill", "white")
    .attr("text-anchor", "middle")
    .attr("alignment-baseline", "middle")
    .attr("font-size", tileSize * 0.20)
    .attr("letter-spacing", "-0.02")

  tiles.select('.score').merge(scoreEnter)
    .html(d => d.score)

}
window.doUpdate = update
window.data = tileData

function responsify(svg) {
  var container = d3.select(svg.node().parentNode);

  d3.select(window).on("resize." + container.attr('id'), resize);
  resize()

  function resize() {
    let targetWidth = parseInt(container.style('width'))
    oldTileWidth = tileSize;
    if (targetWidth <= 450) {
      console.log("targeting mobile")
      tileSize = 25;
    } else {
      console.log("targeting desktop")
      tileSize = 60;
    }
    if (oldTileWidth != tileSize) {
      redraw()
    }
    console.log(targetWidth)

  }
}

d3.select('#board').append('svg')
  .call(responsify)
