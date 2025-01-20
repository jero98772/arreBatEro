const express = require('express');
const app = express();
const port = 3000;

// Middleware for serving static files
app.use(express.static('public'));

// Set the template engine (if using EJS)
app.set('view engine', 'ejs');

// Define routes
app.get('/', (req, res) => {
  res.send('Welcome to my Node.js website!');
});
app.get('/about', (req, res) => {
  res.send('This is the About page.');
});


// Start the server
app.listen(port, () => {
  console.log(`Server is running at http://localhost:${port}`);
});
