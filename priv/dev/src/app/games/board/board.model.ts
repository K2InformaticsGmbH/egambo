import { Cell } from "app/games/board/cell/cell.model";

export class Board {
    public width: number;
    public height: number;
    public moves: number[];
    public representation: string;
}

export class BoardCell {
    public x: number
    public y: number
    public cell: Cell
}
