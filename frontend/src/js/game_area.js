class GameArea extends HTMLElement {
  connectedCallback() {
    let container = d3.select(this)
      .append('div')
        .attr('id', 'board')
        .attr('class', 'board')

    container.append('svg')
      .call(responsify)
  }

  get source() {
    console.log("get source")
    return this._source;
  }


  set source(x) {
    console.log("set source", x)
    if (this._source) {
      this._source.close()
    }

    this._source = new EventSource("http://localhost:8080" + x)
    this._source.addEventListener('snapshot', function(message) {
      let data = JSON.parse(message.data)
      boardData = []
      tileData = []
      let tileId = 0
      data.board.forEach(space => {
        if (space.letter) {
          tileId += 1;
          tileData.push(
            {
              "id": tileId,
              "letter": space.letter,
              "score": space.score,
              "location": ["board", [space.x, space.y]], "moveable": false
            }
          )
        }

        const {x, y, bonus, ...partial} = space;

        boardData.push({ x, y, bonus})
      })
      redraw(boardData)
    })
  }

  get gameId() {
    console.log("get game id")
    return this._gameId;
  }

  set gameId(id) {
    console.log("set game id", id)
    this._gameId = id
  }
}

