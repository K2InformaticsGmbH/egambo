import { NgModule } from "@angular/core";
import { Routes, RouterModule } from "@angular/router";
import { LoginComponent } from "app/auth/login/login.component";
import { RegisterComponent } from "app/auth/register/register.component";
import { WelcomeComponent } from "app/welcome/welcome.component";
import { TypeListComponent } from "app/games/type-list/type-list.component";
import { GameTypesResolve } from "app/games/type-list/game-types.resolve";

const appRoutes: Routes = [
    {path: '', redirectTo: 'welcome', pathMatch: 'full'},
    {path: 'welcome', component: WelcomeComponent},
    {path: 'register', component: RegisterComponent},
    {path: 'login', component: LoginComponent},
    {path: 'type-list', component: TypeListComponent, resolve: {types: GameTypesResolve}}
];

@NgModule({
    imports: [RouterModule.forRoot(appRoutes, {useHash: true})],
    exports: [RouterModule]
})
export class AppRoutingModule {}