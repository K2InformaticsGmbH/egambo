import { Resolve } from "@angular/router/src/interfaces";
import { Game } from "app/games/game/game.model";
import { ActivatedRouteSnapshot, RouterStateSnapshot } from "@angular/router";
import { Observable } from "rxjs/Observable";
import { Injectable } from "@angular/core";
import { DataStorageService } from "app/data-storage.service";
import { Board } from "app/games/board/board.model";

@Injectable()
export class GameResolve implements Resolve<Game> {

    constructor(private dataService: DataStorageService) {}

    resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Game | Observable<Game> | Promise<Game> {
        return this.dataService.loadGame(route.params.id)
            .map((respObj) => {
                return {
                    id: respObj.id,
                    players: respObj.players,
                    status: respObj.status,
                    board: {
                        width: respObj.width,
                        height: respObj.height,
                        moves: respObj.moves,
                        representation: respObj.board
                    }
                };
            });
    }

}