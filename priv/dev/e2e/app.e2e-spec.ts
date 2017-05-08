import { EgamboPage } from './app.po';

describe('egambo App', () => {
  let page: EgamboPage;

  beforeEach(() => {
    page = new EgamboPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
