import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';

import { AppComponent } from './app.component';
import { BoardComponent } from './board/board.component';
import { MovesComponent } from './moves/moves.component';
import { WelcomeComponent } from './welcome/welcome.component';
import { HeaderComponent } from './header/header.component';
import { LoginComponent } from './auth/login/login.component';
import { RegisterComponent } from './auth/register/register.component';
import { AuthService } from 'app/auth/auth.service';
import { AppRoutingModule } from 'app/app-routing.module';

@NgModule({
    declarations: [
        AppComponent,
        BoardComponent,
        MovesComponent,
        WelcomeComponent,
        HeaderComponent,
        LoginComponent,
        RegisterComponent
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        AppRoutingModule
    ],
    providers: [AuthService],
    bootstrap: [AppComponent]
})
export class AppModule { }
