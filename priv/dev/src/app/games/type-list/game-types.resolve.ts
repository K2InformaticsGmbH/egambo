import { Resolve, ActivatedRouteSnapshot, RouterStateSnapshot } from "@angular/router";
import { Observable } from "rxjs/Observable";
import { GameType } from "app/games/type-list/game-type.model";
import { DataStorageService } from "app/data-storage.service";
import { Injectable } from "@angular/core";

@Injectable()
export class GameTypesResolve implements Resolve<GameType[]> {

    constructor(private dataService: DataStorageService) {}

    resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): any[] | Observable<any[]> | Promise<any[]> {
        return this.dataService.getGameTypes();
    }
}
