import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            index={i + "." + j}
                            onClick={(index) =>  this.props.onClick(index)}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;