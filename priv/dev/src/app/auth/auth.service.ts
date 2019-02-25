import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { Http } from '@angular/http';
import { Observable } from 'rxjs/Observable';

const urlPrefix = 'api/'

@Injectable()
export class AuthService {
    private authenticated = false;
    private user = '';

    constructor(private router: Router, private http: Http) { }

    login(email: string, password: string): Observable<string> {
        console.log('Loging in user:', email);
        return this.http.post(urlPrefix + 'login', { user: email, password: password })
            .map((res) => {
                if(res.json().resp === 'ok') {
                    this.user = email;
                    this.authenticated = true;
                    this.router.navigate(['/type-list']);
                }
                return res.json().resp;
            });
    }

    logout() {
        console.log('Log out user:', this.user);
        return this.http.post(urlPrefix + 'logout', {})
            .map((res) => {
                if(res.json().resp === 'ok') {
                    this.authenticated = false;
                    this.router.navigate(['/login']);
                }
                return res.json().resp;
            });

    }

    isAuthenticated() {
        return this.authenticated;
    }
}
