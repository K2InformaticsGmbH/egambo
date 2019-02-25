import { Component, OnInit, Input } from '@angular/core';
import { Cell } from 'app/games/board/cell/cell.model';

@Component({
    // Disable component selector check as svg doens't support custom components.
    // tslint:disable-next-line:component-selector
    selector: '[app-cell]',
    templateUrl: './cell.component.html',
    styleUrls: ['./cell.component.css']
})
export class CellComponent implements OnInit {
    @Input() cell: Cell

    constructor() { }

    ngOnInit() {
    }
}
