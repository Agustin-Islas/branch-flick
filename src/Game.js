import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "red";
    case "v": return "violet";
    case "p": return "pink";
    case "g": return "green";
    case "b": return "blue";
    case "y": return "yellow";
    default:;
  }
  return color;
}
class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      grid: null,
      complete: false,  // true if game is complete, false otherwise
      waiting: false,
      captured: 1
    };
    this.handleClickInit = this.handleClickInit.bind(this);
    this.handleClick     = this.handleClick.bind(this);
    this.findAdjacents   = this.findAdjacents.bind(this);
    this.capturedCells   = this.capturedCells.bind(this);
    this.checkEnd        = this.checkEnd.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }
  
  handlePengineCreate() {
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });
      this.handleClickInit("0.0");
      }
    });
  }

  handleClickInit(index) {
    if (this.state.turns === 0) {
      //parsear y setear initCell
      let cell = this.parsearIndex(index);
      const queryInit = "setInit(" +  Number(cell[0]) + "," + Number(cell[1]) + ")";
    
      this.pengine.query(queryInit, (success, response) => {
        if(success) {
          // cambiar borde de init cell
          console.log("init"+ cell);
          this.pengine.query("setAdjacent(" +  Number(cell[0]) + "," + Number(cell[1]) + ")", (success, response) => {
            if(success) {
              // cambiar borde de init cell
              console.log("adj ");
              } else {
                console.log("falló adj");
              }
          });
        }
      });
    }
  }

  parsearIndex(index) {
    let i=0;
    let row="";
    let column="";
    while (index[i] !== ".") {
      row += index[i];
      i++;
    }
    i++;
    while (i < index.length) {
      column += index[i];
      i++;
    }
    return [row, column];
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const queryS = `flickAdjacents(${gridS},${color},Grid)`;

    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });

        this.findAdjacents(color);

        this.setState({
          turns: this.state.turns + 1,
          waiting: false,
        })
        
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
        console.log("falló flick");
      }
    });
  }
  
  findAdjacents(color) {
    let gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    let queryFind = "findAdjacentC(" + gridS + "," + color + ")";
    this.pengine.query(queryFind, (success, response) => {
      if (success) {
        console.log("findAdj success");
        //actualized captured
        this.capturedCells();
      } else {
          console.log("falló findAdj");
        }
    });
  }

  capturedCells() {
    let queryC = "capturedCells(Captured)";
    this.pengine.query(queryC, (success, response) => {
      if (success) {
        this.setState({
          captured: response['Captured']
        });
        this.checkEnd();
      } else {
        console.log("falló Captured");
      }
    });
  }

  checkEnd() {
    let queryEnd = "checkEnd(End)";
    this.pengine.query(queryEnd, (success, response) => {
      if (success) {
        this.setState({
          complete: response['End']
        });
      } else {
        console.log("Falló CheckEnd");
      }
    })
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel">
            {colors.map(color =>
              <button
                className="colorBtn"
                style={{ backgroundColor: colorToCss(color) }}
                onClick={() => this.handleClick(color)}
                key={color}
              />)}
          </div>
          <div className="turnsPanel">
            <div className="turnsLab">Turns</div>
            <div className="turnsNum">{this.state.turns}</div>

            <div className="capturedCells">CAPTURED CELLS</div>
            <div className="capturedNumber">{this.state.captured}</div>

            <div className="End">{this.state.complete}</div>
          </div>
        </div>
        <Board grid={this.state.grid}
               onClick={(index) => this.handleClickInit(index)}
        />
      </div>
    );
  }
}

export default Game;