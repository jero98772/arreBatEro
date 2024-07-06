const https = require('https');
const fs = require('fs');
const path = require('path');

const options = {
  key: fs.readFileSync('key.pem'),
  cert: fs.readFileSync('cert.pem')
};

const server = https.createServer(options, function (req, res) {
  if (req.url === '/') {
    res.writeHead(200, { 'Content-Type': 'text/html' });
    fs.createReadStream('index.html').pipe(res);
  } else if (req.url === '/app.js') {
    res.writeHead(200, { 'Content-Type': 'application/javascript' });
    fs.createReadStream('app.js').pipe(res);
  } else {
    res.writeHead(404);
    res.end();
  }
});

server.listen(8000, () => {
  console.log('Server running at https://localhost:8000/');
});