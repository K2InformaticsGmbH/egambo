import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';

import { AppComponent } from './app.component';
import { BoardComponent } from './games/board/board.component';
import { MovesComponent } from './moves/moves.component';
import { WelcomeComponent } from './welcome/welcome.component';
import { HeaderComponent } from './header/header.component';
import { LoginComponent } from './auth/login/login.component';
import { RegisterComponent } from './auth/register/register.component';
import { AuthService } from 'app/auth/auth.service';
import { AppRoutingModule } from 'app/app-routing.module';
import { TypeListComponent } from './games/type-list/type-list.component';
import { GameListComponent } from './games/game-list/game-list.component';
import { GameTypesResolve } from 'app/games/type-list/game-types.resolve';
import { DataStorageService } from 'app/data-storage.service';

@NgModule({
    declarations: [
        AppComponent,
        BoardComponent,
        MovesComponent,
        WelcomeComponent,
        HeaderComponent,
        LoginComponent,
        RegisterComponent,
        TypeListComponent,
        GameListComponent
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        AppRoutingModule
    ],
    providers: [
        AuthService,
        DataStorageService,
        GameTypesResolve
    ],
    bootstrap: [AppComponent]
})
export class AppModule { }
