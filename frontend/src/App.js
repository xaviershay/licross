// @flow

import React, { useState } from "react";
import { BrowserRouter as Router, Route, Link } from "react-router-dom";
import * as d3 from "d3";

import "./App.css";

const gridSize = 15;
const BONUS_COLORS = {
  none: "#738896",
  tw: "#ac524f",
  tl: "#759900",
  dl: "#699be7",
  dw: "#c62dbc",
  anchor: "#c62dbc",
  letter: "#d59120"
};
const tileSize = 32;
const borderWidth = 2;
const gridWidth = 15;

function tilesToPixels(n) {
  return tileSize * n + borderWidth * n;
}

type Props = {
  uri: string,
  gameId: string,
  showRack: boolean
};

type State = {
  tiles: Array<any>
};

class Board extends React.Component<Props, State> {
  node: any;
  source: any;
  tileData: any;

  static defaultProps = {
    showRack: false
  };

  constructor(props) {
    super();
    this.node = React.createRef();
    this.source = null;
    this.state = {
      tiles: []
    };
  }

  componentWillUnmount() {
    if (this.source) {
      this.source.close();
      this.source = null;
    }
  }

  componentDidMount() {
    let inited = false;

    this.source = new EventSource(this.props.uri);
    this.source.addEventListener("finished", (e: any) => {
      this.source.close();
    });
    this.source.addEventListener("snapshot", e => {
      const data = JSON.parse(e.data);

      let boardData = [];
      let tileData = [];

      Object.keys(data.board).forEach(position => {
        const [x, y] = position.split("-");
        boardData.push({ x, y, bonus: data.board[position] });
      });

      // TODO: Index rack by player
      let rackIndex = 0;
      data.tiles.forEach(tile => {
        if (tile.location.type === "rack") {
          tile.location.index = rackIndex;
          tile.moveable = true;
          rackIndex += 1;
        }
      });

      tileData = data.tiles;
      if (!inited) {
        this.renderBoard(boardData, tileData);
        inited = true;
      } else {
        const node = this.node.current;
        this.updateBoard(d3.select(node).select("svg"), tileData);
      }
    });
  }

  renderBoard(boardData, tileData) {
    const node = this.node.current;

    if (node == null) {
      console.log("Error: <div> node is not available!");
      return;
    }

    const showRack = this.props.showRack;
    const boardSize = tilesToPixels(gridWidth);
    const gutter = tileSize;
    const rackHeight = tilesToPixels(1) + borderWidth;
    // const rackWidth = tilesToPixels(7) + borderWidth;

    const containerWidth = boardSize + borderWidth;
    const containerHeight =
      boardSize + (showRack ? gutter + rackHeight : 0) + borderWidth;
    const containerColor = "#333";

    let container = d3
      .select(node)
      .append("svg")
      .attr("width", containerWidth)
      .attr("height", containerHeight);

    container
      .append("rect")
      .attr("fill", containerColor)
      .attr("width", containerWidth)
      .attr("height", containerHeight);

    // CELL BACKGROUNDS
    let cell = container
      .selectAll(".space")
      .data(boardData)
      .enter()
      .append("g")
      .attr("class", "space")
      .attr(
        "transform",
        d => "translate(" + tilesToPixels(d.x) + "," + tilesToPixels(d.y) + ")"
      );

    cell
      .append("rect")
      .attr("fill", d => BONUS_COLORS[d.bonus])
      .attr("width", tileSize)
      .attr("height", tileSize)
      .attr("x", borderWidth)
      .attr("y", borderWidth);

    cell
      .filter(d => d.bonus !== "none")
      .append("text")
      .attr("x", borderWidth + tileSize * 0.5)
      .attr("y", borderWidth + tileSize * 0.53)
      .attr("font-size", tileSize * 0.45)
      .attr("fill", "white")
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle")
      .html(d => (d.bonus === "anchor" ? "★" : d.bonus.toUpperCase()));

    this.updateBoard(container, tileData);
  }

  updateBoard(container, tileData) {
    this.tileData = tileData;
    let tiles = container.selectAll(".tile").data(tileData, d => d.id);

    const boardSize = tilesToPixels(gridWidth);
    const gutter = tileSize;
    //const rackHeight = tilesToPixels(1) + borderWidth
    const rackWidth = tilesToPixels(7) + borderWidth;

    const containerWidth = boardSize + borderWidth;
    //const containerHeight = boardSize + gutter + rackHeight
    const containerColor = "#333";

    function tileLocation(loc): Array<number> {
      switch (loc.type) {
        case "bag":
          return [0, 0];
        case "board":
          return [
            tilesToPixels(loc.position[0]),
            tilesToPixels(loc.position[1])
          ];
        case "rack":
          return [
            tilesToPixels(loc.index) + (containerWidth - rackWidth) / 2,
            boardSize + gutter + borderWidth
          ];
        default:
          console.log("Unknown location: " + loc);
      }
      return [];
    }

    let self = this;
    let deltaX, deltaY;
    let transformF = d => {
      const x = tileLocation(d.location)[0];
      const y = tileLocation(d.location)[1];
      if (!(x >= 0)) {
        console.log(d);
      }
      return "translate(" + x + "," + y + ")";
    };
    let tilesEnter = tiles
      .enter()
      .append("g")
      .attr("class", "tile")
      .attr("cursor", "grab")
      .call(
        d3
          .drag()
          .on("start", function(d) {
            //const current = d3.select(this).raise()
            deltaX = tileLocation(d.location)[0] - d3.event.x;
            deltaY = tileLocation(d.location)[1] - d3.event.y;
          })
          .on("drag", function(d) {
            const x = d3.event.x + deltaX;
            const y = d3.event.y + deltaY;

            // TODO: Highlight cell being hovered over
            d3.select(this).attr(
              "transform",
              d => "translate(" + x + "," + y + ")"
            );
          })
          .on("end", function(d) {
            // If on board and placeable, put it there, otherwise back to the
            // rack.
            // TODO: Allow rack re-arranging
            const x = d3.event.x + deltaX;
            const y = d3.event.y + deltaY;

            if (x <= boardSize && y <= boardSize) {
              const dropWidth = borderWidth + tileSize;
              const xPos = Math.floor((x + borderWidth / 2) / dropWidth + 0.5);
              const yPos = Math.floor((y + borderWidth / 2) / dropWidth + 0.5);

              // If not, move tile to that space by updating data.
              if (
                xPos >= 0 &&
                xPos < gridSize &&
                yPos >= 0 &&
                yPos < gridSize
              ) {
                // Check if space is occupied
                let existing = tileData.find(d => {
                  const l = d.location;
                  return l.type === "board" && l.x === xPos && l.y === yPos;
                });
                if (!existing) {
                  d.location = { type: "board", position: [xPos, yPos] };
                }
              }
            }
            self.updateBoard(container, tileData);
          })
      )
      .attr("transform", transformF);

    tiles
      .merge(tilesEnter)
      .transition()
      // This transform is duplicated in enter selection, so that the initial
      // placement doesn't animate, only subsequent updates.
      .attr("transform", transformF);

    tilesEnter
      .append("rect")
      .attr("fill", containerColor)
      .attr("width", tileSize + borderWidth * 2)
      .attr("height", tileSize + borderWidth * 2);

    tilesEnter
      .append("rect")
      .attr("fill", BONUS_COLORS["letter"])
      .attr("x", borderWidth)
      .attr("y", borderWidth)
      .attr("width", tileSize)
      .attr("height", tileSize);

    let contentEnter = tilesEnter
      .append("text")
      .attr("class", "content")
      .attr("x", borderWidth + tileSize * 0.5)
      .attr("y", borderWidth + tileSize * 0.55)
      .attr("font-size", tileSize * 0.8)
      .attr("fill", "white")
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle");

    tiles
      .select(".content")
      .merge(contentEnter)
      .html(d => d.letter);

    let scoreEnter = tilesEnter
      .append("text")
      .attr("class", "score")
      .attr("x", borderWidth + tileSize * 0.86)
      .attr("y", borderWidth + tileSize * 0.86)
      .attr("fill", "white")
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle")
      .attr("font-size", tileSize * 0.2)
      .attr("letter-spacing", "-0.02");

    tiles
      .select(".score")
      .merge(scoreEnter)
      .html(d => d.score);
  }

  submitHandler = async () => {
    const moveTiles = this.tileData.filter(
      tile => tile.moveable && tile.location.type === "board"
    );

    //console.log(moveTiles)
    const move = {
      type: "PlayTiles",
      tiles: moveTiles
    };

    const uri = `http://localhost:8080/game/${this.props.gameId}/move`;
    try {
      await fetch(uri, {
        method: "POST",
        body: JSON.stringify(move),
        headers: {
          "Content-Type": "application/json"
        }
      });
    } catch (e) {
      alert(e); // TODO
    } finally {
      //setLoading(false)
    }
  };

  render() {
    return (
      <div>
        <button onClick={this.submitHandler}>Submit Move</button>
        <div ref={this.node} />
      </div>
    );
  }
}

function Example() {
  return <Board gameId="example" uri="http://localhost:8080/example" />;
}

function Home() {
  return <h1>Licross</h1>;
}

function NewGame() {
  const [loading, setLoading] = useState(false);
  const clickHandler = async () => {
    setLoading(true);

    const uri = "http://localhost:8080/game";
    try {
      const response = await fetch(uri, {
        method: "POST",
        body: JSON.stringify({}),
        headers: {
          "Content-Type": "application/json"
        }
      });

      const gameId = await response.json();
      window.location = `/game/${gameId}`;
    } catch (e) {
      alert(e); // TODO
    } finally {
      setLoading(false);
    }
  };

  return (
    <button onClick={clickHandler} disabled={loading}>
      Make New Game
    </button>
  );
}

function Game({ match }) {
  //const [_joining, setJoining] = useState(false);
  const gameId = match.params.id;
  const playerId = "1234";

  const joinHandler = async () => {
    const uri = `http://localhost:8080/game/${gameId}/join?playerId=${playerId}`;
    try {
      await fetch(uri, {
        method: "POST",
        body: JSON.stringify({}),
        headers: {
          "Content-Type": "application/json"
        }
      });

      //const gameId = await response.json()
    } catch (e) {
      alert(e); // TODO
    } finally {
      //setJoining(false);
    }
  };

  const startHandler = async () => {
    const uri = `http://localhost:8080/game/${gameId}/start`;
    try {
      await fetch(uri, {
        method: "POST",
        body: JSON.stringify({}),
        headers: {
          "Content-Type": "application/json"
        }
      });
    } catch (e) {
      alert(e); // TODO
    } finally {
    }
  };

  return (
    <div>
      <button onClick={joinHandler}>Join Game</button>
      <button onClick={startHandler}>Start Game</button>
      <Board
        showRack
        gameId={gameId}
        uri={`http://localhost:8080/game/${gameId}/player/${playerId}/subscribe`}
      />
    </div>
  );
}

function App() {
  return (
    <Router>
      <ul>
        <li>
          <Link to="/">Home</Link>
        </li>
        <li>
          <Link to="/new-game">New Game</Link>
        </li>
        <li>
          <Link to="/example">Example</Link>
        </li>
      </ul>
      <Route path="/" exact component={Home} />
      <Route path="/new-game" exact component={NewGame} />
      <Route path="/game/:id" exact component={Game} />
      <Route path="/example" component={Example} />
    </Router>
  );
}

export default App;
