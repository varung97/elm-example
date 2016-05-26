var io = require('socket.io')(8001);

io.on('connection', function (socket) {
  console.log("Client connected.");
  socket.on('echo', function (data) {
    console.log('Received:', data);
    socket.emit('echo', data);
  });
  socket.on('disconnect', function (socket) {
    console.log("Client disconnected.");
  });
});
