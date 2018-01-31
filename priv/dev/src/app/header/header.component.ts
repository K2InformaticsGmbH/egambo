import { Component, OnInit } from '@angular/core';
import { AuthService } from 'app/auth/auth.service';

@Component({
    selector: 'app-header',
    templateUrl: './header.component.html',
    styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {

    constructor(private authService: AuthService) { }

    ngOnInit() {
    }

    logout() {
        this.authService.logout().subscribe((resp) => {
            console.log("resp");
        });
    }

}
