import { Component, OnInit } from '@angular/core';
import { GameListItem } from 'app/games/game-list/game-list-item.model';
import { ActivatedRoute } from '@angular/router';

@Component({
    selector: 'app-game-list',
    templateUrl: './game-list.component.html',
    styleUrls: ['./game-list.component.css']
})
export class GameListComponent implements OnInit {
    games: GameListItem[];

    constructor(private route: ActivatedRoute) { }

    ngOnInit() {
        this.games = this.route.snapshot.data.games;
    }

}
