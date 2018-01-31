import { Component, OnInit } from '@angular/core';
import { Game } from 'app/games/game/game.model';
import { ActivatedRoute } from '@angular/router';

@Component({
    selector: 'app-game',
    templateUrl: './game.component.html',
    styleUrls: ['./game.component.css']
})
export class GameComponent implements OnInit {
    game: Game;

    constructor(private route: ActivatedRoute) { }

    ngOnInit() {
        this.game = this.route.snapshot.data.game;
    }

}
