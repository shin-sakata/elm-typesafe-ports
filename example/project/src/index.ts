import { Elm } from "./Main.elm"

const app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: 114514
});

app.ports.alertString.subscribe(function (msg: string) {
    alert(msg);
});

app.ports.alertInt.subscribe(function (int: number) {
    alert(int);
});

app.ports.alertBool.subscribe(function (bool: boolean) {
    alert(bool);
});
