import { Board } from "app/games/board/board.model";

export class Game {
    public id: string;
    // The list order defines who is next
    public players: string[];
    public status: string;
    public board: Board;
}
