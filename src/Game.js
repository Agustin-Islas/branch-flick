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
      turns: 0,         // turno actual de la partida
      grid: null,       // grilla de colores
      complete: false,  // true if game is complete, false otherwise
      waiting: false,   // true si el juego esta procesando información, false en caso contrario
      captured: 1,      // total de celdas capturadas hasta el momento
      initCell: [0,0],  // celda inicial, por defecto es la celda [0, 0]
      plays: [],        // pila de jugadas realizadas hasta el momento.
      bestPlay: [],
      capturedHelp: 0
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
        
        // resetea el estado plays a vacío.
        this.pengine.query("resetStackPlays", (success, response) => {
          // setea la celda [0, 0] como inicial.
            this.handleClickInit("0.0");
        });
      }
    });
  }

  /**
   * Si el estado turns es 0, setea index como nueva initCell y la agrega a la lista de adjacencias.
   * @param index string de la forma "num1.num2"
   */
  handleClickInit(index) {
    if (this.state.turns === 0) {
      let cell = this.parsearIndex(index);
      const queryInit = "setInit(" +  Number(cell[0]) + "," + Number(cell[1]) + ")";
    
      this.pengine.query(queryInit, (success, response) => {
        if(success) {
          this.pengine.query("setAdjacent(" +  Number(cell[0]) + "," + Number(cell[1]) + ")");          
          this.setState({
            initCell: cell
          });
        }
      });
    }
  }

  /**
   * Recibe un string y lo tranforma en un array.
   * @param index string "num1.num2"
   * @returns array [num1, num2]
   */
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

  /**
   * Si el juego no termino o no esta en espera, cambia el color de initCell y de sus adjacentesC* a el color
   * recibido por parametro.
   * @param color char que representa el color selecionado.
   */
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
        // Realiza el flick de initCell y sus adjacentC*. Actuliza el estado grid.
        this.setState({
          grid: response['Grid']
        });

        this.pushStackPlays(color);

        this.setState({
          turns: this.state.turns + 1,
          waiting: false,
        })
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
      }
    });
  }

  /**
   *  agrega color a la pila de jugadas realizadas. 
   * @param color char que representa el color selecionado.
   */
  pushStackPlays(color) {
    this.pengine.query("pushStackPlays(" + color + ", Plays)", (success, response) => {
      if (success) {
        // Actualiza el estado plays con el nuevo color agregado.
        this.setState({
          plays: response['Plays']
        })
      }
      this.findAdjacents(color);
    });
  }
  
  /**
   * busca nuevas celdas adjacentC* a la lista de adjacencias actual y las incorpora a la lista.
   * @param color char que representa el color selecionado.
   */
  findAdjacents(color) {
    let gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    let queryFind = "findAdjacentC(" + gridS + "," + color + ")";
    this.pengine.query(queryFind, (success, response) => {
      if (success) {
        this.capturedCells();
      }
    });
  }

  /**
   * Calcula y actualiza el estado Captured.
   */
  capturedCells() {
    let queryC = "capturedCells(Captured)";
    this.pengine.query(queryC, (success, response) => {
      if (success) {
        this.setState({
          captured: response['Captured']
        });
        this.checkEnd();
      }
    });
  }

  /**
   * Checkea si el juego finalizo actualizando el estado complete.
   */
  checkEnd() {
    let queryEnd = "checkEnd(End)";
    this.pengine.query(queryEnd, (success, response) => {
      if (success) {
        this.setState({
          complete: response['End']
        });
      }
    })
  }

  handleHelp() {
    console.log("OK help!");
    //consulta help a prolog

    this.setState({
      waiting: true
    });

    let queryResetHelp = "resetHelp";
    let gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    let queryHelp = "searchCombinations(" + 3 + "," + gridS + ")";

    this.pengine.query(queryResetHelp, (success, response) => {
      if(success) {
        console.log("reset");
        this.pengine.query(queryHelp, (success, response) => {
          if (!success) {
            this.pengine.query("bestPlay(Stack)", (success, response) => {
              if (success) {
                this.setState({
                  bestPlay: response['Stack']
                });
                this.pengine.query("maxCaptureds(Captureds)", (success, response) => {
                  if (success) {
                    this.setState({
                      capturedHelp: response['Captureds'] - this.state.captured
                    });
                  } else {
                    console.log("fail captured");
                  }
                });
              } else {
                console.log("fail sequenceColor");
              }
            });
          } else {
            console.log("fail help");
          }
        })
      } else {
        console.log("fail reset");
      }
    })
    

    this.setState({
      waiting: false
    });
    //cargar state.bestPlay
    //cargar capturesBest
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      this.state.complete === false?
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
            <div className="panelGeneral">
              <div className="turnsLab">Turns</div>
              <div className="turnsNum">{this.state.turns}</div>

              <div className="capturedLab">CAPTURED CELLS</div>
              <div className="capturedNum">{this.state.captured}</div>

              <div  className="playsLab">PLAYS</div>
              <div className="playsStack"> 
                {
                  this.state.plays.map((cell, i) =>
                    <button
                    className="playBtn"
                    style={{ backgroundColor: colorToCss(cell)}}
                    key={i}
                  />)}
              </div>

              <div className="End">{this.state.complete}</div>
            </div>
          </div>
            <Board grid={this.state.grid}
                onClick={(index) => this.handleClickInit(index)}
                initCell={this.state.initCell}
            />
        <div>
          <div className="panelHelp">
            <button className="btnHelp"
                    onClick={() => this.handleHelp()}>
              HELP
            </button>
            <div  className="playsBest">BEST PLAY</div>
              <div className="bestPlayStack"> 
                {
                  this.state.bestPlay.map((cell, i) =>
                    <button
                    className="playBtn"
                    style={{ backgroundColor: colorToCss(cell)}}
                    key={i}
                  />)}
              </div>
              <div className='capturedsBestPlay'>
                  Captureds {this.state.capturedHelp}
              </div>
          </div>
        </div>
        </div>
        : 
        <div className='endGame'>
          <div className='endPanel'>
            <h1  className='titule'> ¡You won in {this.state.turns} turns!</h1>
            <button onClick={() => window.location.reload()} className='playAgain'>PLAY AGAIN</button>
          </div>
        </div>
    );
  }
}

export default Game;