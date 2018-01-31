import { Injectable } from "@angular/core";
import { Http, Response } from "@angular/http";
import 'rxjs/Rx';

const urlPrefix = 'api/';

@Injectable()
export class DataStorageService {
    constructor(private http: Http) {}

    getGameTypes() {
        return this.http.post(urlPrefix + 'get_game_types', {})
            .map((res) => {
                console.log(res);
                return res.json().resp;
            });
    }

    listGames(type: string) {
        return this.http.post(urlPrefix + 'list_games', {type: type})
            .map((res) => {
                console.log(res);
                return res.json().resp; // Here we probably should map to game-list-item
            });
    }

    loadGame(id: string) {
        return this.http.post(urlPrefix + 'load_game', {id: id})
        .map((res) => {
            console.log(res);
            return res.json().resp;
        });
    }
}
