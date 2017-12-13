import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { GameType } from 'app/games/type-list/game-type.model';

@Component({
    selector: 'app-type-list',
    templateUrl: './type-list.component.html',
    styleUrls: ['./type-list.component.css']
})
export class TypeListComponent implements OnInit {
    gameTypes: GameType[];

    constructor(private route: ActivatedRoute) { }

    ngOnInit() {
        this.gameTypes = this.route.snapshot.data.types;
    }

}
