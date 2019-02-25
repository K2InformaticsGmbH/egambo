import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { LoginComponent } from 'app/auth/login/login.component';
import { RegisterComponent } from 'app/auth/register/register.component';
import { WelcomeComponent } from 'app/welcome/welcome.component';
import { TypeListComponent } from 'app/games/type-list/type-list.component';
import { GameTypesResolve } from 'app/games/type-list/game-types.resolve';
import { GameListComponent } from 'app/games/game-list/game-list.component';
import { WrapperComponent } from 'app/games/wrapper/wrapper.component';
import { GameComponent } from 'app/games/game/game.component';
import { GameListResolve } from 'app/games/game-list/game-list.resolve';
import { GameResolve } from 'app/games/game/game.resolve';

const appRoutes: Routes = [
    {path: '', redirectTo: 'welcome', pathMatch: 'full'},
    {path: 'welcome', component: WelcomeComponent},
    {path: 'register', component: RegisterComponent},
    {path: 'login', component: LoginComponent},
    {path: 'type-list', component: WrapperComponent, children: [
        {path: '', component: TypeListComponent, resolve: {types: GameTypesResolve}},
        {path: ':type', component: WrapperComponent, children: [
            {path: '', component: GameListComponent, resolve: {games: GameListResolve}},
            {path: ':id', component: GameComponent, resolve: {game: GameResolve}}
        ]}
    ]}
];

@NgModule({
    imports: [RouterModule.forRoot(appRoutes, {useHash: true})],
    exports: [RouterModule]
})
export class AppRoutingModule {}
