import "./style.css"
import { Elm } from "./src/Main.elm"

if (process.env.NODE_ENV === "development") {
    const ElmDebugTransform = await import("elm-debug-transformer")

    ElmDebugTransform.register({
        simple_mode: true
    })
}

const flags = {
    options: localStorage.options ? localStorage.options : null
}
const root = document.querySelector("#app div")
const app = Elm.Main.init({ node: root, flags })

app.ports.save.subscribe(options => {
    localStorage.options = JSON.stringify(options)
})
