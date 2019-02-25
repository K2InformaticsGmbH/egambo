import { Injectable } from '@angular/core';
import { Http } from '@angular/http';
import 'rxjs/Rx';

const urlPrefix = 'api/';

@Injectable()
export class DataStorageService {
    constructor(private http: Http) {}

    getGameTypes() { return this.post('get_game_types', {}); }
    listGames(type: string) { return this.post('list_games', {type: type}); }
    loadGame(id: string) { return this.post('load_game', {id: id}); }
    play(id: string, pos: number) { return this.post('play', {game_id: id, position: pos}); }

    private post(url, args) {
        return this.http.post(urlPrefix + url, args).map((res) => {
            console.log(res);
            return res.json().resp;
        });
    }
}
