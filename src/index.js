import { Elm } from "./Main.elm";
import { Howl, Howler } from "howler";

const app = Elm.Main.init({
  node: document.querySelector("main")
});

Howler.volume(0.1);

app.ports.emitSound.subscribe((filename) => {
  const sound = new Howl({
    src: [filename]
  })
  sound.play();
})