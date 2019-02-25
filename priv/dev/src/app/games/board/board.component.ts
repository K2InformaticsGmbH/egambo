import { Component, OnInit, Input } from '@angular/core';
import { Board, BoardCell } from 'app/games/board/board.model';
import { Cell } from 'app/games/board/cell/cell.model';
import { DataStorageService } from 'app/data-storage.service';

@Component({
    selector: 'app-board',
    templateUrl: './board.component.html',
    styleUrls: ['./board.component.css']
})
export class BoardComponent implements OnInit {
    readonly padding: number = 10
    readonly boardSize: number = 1000

    @Input() board: Board
    @Input() id: string

    boardCells: BoardCell[]
    cellSize: number

    constructor(private dataStorage: DataStorageService) {
        this.boardCells = new Array();
    }

    ngOnInit() {
        const colors = {
            ' ': 'snow',
            'X': 'red',
            'O': 'blue'
        }
        let longestSide = this.board.width;
        if(this.board.height > longestSide) {
            longestSide = this.board.height;
        }
        this.cellSize = Math.trunc(this.boardSize / longestSide);

        const radius = Math.trunc((this.cellSize - this.padding) / 2);
        const offset = this.cellSize / 2; // Shift the cells as first one starts at [0,0].
        let i = 0;
        for(const token of this.board.representation) {
            const occupied = token !== ' ';
            const col = i % this.board.width;
            const row = Math.floor(i / this.board.width);
            this.boardCells.push({
                x: offset + col * this.cellSize,
                y: offset + row * this.cellSize,
                cell: new Cell(occupied, colors[token], radius)
            });
            i++;
        }
    }

    handleClick(index: number) {
        console.log('the click', index);
        const cell: Cell = this.boardCells[index].cell
        // If the cell is occupied we do nothing.
        if(cell.occupied) {
            return;
        }

        // Otherwise we play.
        this.dataStorage.play(this.id, index).subscribe((response) => {
            console.log('The response from playing :O', response);
            cell.occupied = true;
            cell.color = 'green';
        });
    }
}
