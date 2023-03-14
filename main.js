import "./style.css"
import { Elm } from "./src/Main.elm"

if (process.env.NODE_ENV === "development") {
    const ElmDebugTransform = await import("elm-debug-transformer")

    ElmDebugTransform.register({
        simple_mode: true
    })
}

const flags = {
    hiraganaOptions: localStorage.hiraganaOptions ? localStorage.hiraganaOptions : null,
    katakanaOptions: localStorage.katakanaOptions ? localStorage.katakanaOptions : null,
    windowWidth: window.innerWidth,
    windowHeight: window.innerHeight,
}
const root = document.querySelector("#app div")
const app = Elm.Main.init({ node: root, flags })

app.ports.saveHiragana.subscribe(options => {
    localStorage.hiraganaOptions = JSON.stringify(options)
})

// app.ports.saveKatakana.subscribe(options => {
//     localStorage.katakanaOptions = JSON.stringify(options)
// })
