import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {
    render() {
        if (this.props.borde === true) {
            return (
                <div 
                    onClick={() => this.props.onClick(this.props.index)} 
                    style={{ 
                        backgroundColor: colorToCss(this.props.value),
                        border: "2px solid #000"
                    }} 
                />
            )
        }else {
            return (
                <div 
                    onClick={() => this.props.onClick(this.props.index)} 
                    style={{ backgroundColor: colorToCss(this.props.value) }} 
                />
            );
        }
    }
}

export default Square;