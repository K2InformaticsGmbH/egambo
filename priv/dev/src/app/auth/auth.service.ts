export class AuthService {
    // TODO: For now a dummy service
    private authenticated = false;

    login() {
        this.authenticated = true;
    }

    logout() {
        this.authenticated = false;
    }

    isAuthenticated() {
        return this.authenticated;
    }
}