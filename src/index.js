import { Elm } from './Main.elm'

const app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: { storedDuel: JSON.parse(localStorage.getItem("lastDuel")) }
});

app.ports.storeDuelState.subscribe(duel => {
    const stringRepr = JSON.stringify(duel);
    console.info(`Storing duel state: ${stringRepr}`);
    localStorage.setItem("lastDuel", stringRepr);
});

app.ports.info.subscribe(msg => {
    console.info(msg)
});

app.ports.error.subscribe(msg => {
    console.error(msg)
});
