const { Elm } = require('./Main.elm')


const initWebSocket = app => {
    const socket = new WebSocket('ws://localhost:3000/ws')
    socket.onopen = event => {
	console.log('websocket connected')
    }
    socket.onmessage = event => {
	app.ports.recieveMessage.send(event.data)
	console.log('websocket recieved message')
    }
    socket.onerror = err => {
	console.log(`websocket error: ${err}`)
    }
    socket.onclose = event => {
	console.log('websocket close')
    }
}


const app = Elm.Main.init({
    node: document.querySelector('#app'),
})
initWebSocket(app)

