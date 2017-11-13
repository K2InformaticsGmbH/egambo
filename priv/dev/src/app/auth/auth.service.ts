import { Injectable } from "@angular/core";
import { Router } from "@angular/router";

@Injectable()
export class AuthService {
    // TODO: For now a dummy service
    private authenticated = false;

    constructor(private router: Router) {} 

    login(email: string, password: string) {
        console.log("Loging in user:", email);
        console.log(password);
        this.authenticated = true;
        this.router.navigate(['/type-list']);
    }

    logout() {
        this.authenticated = false;
        this.router.navigate(['/']);
    }

    isAuthenticated() {
        return this.authenticated;
    }
}