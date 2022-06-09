
// Import the express in typescript file
import express from 'express';
import process from 'process';
 
// Initialize the express engine
const app: express.Application = express();

 
// Handling '/' Request
app.get('/', (_req, _res) => {
    _res.send("TypeScript With Expresss");
});
 
// Server setup
const port: string = process.env.PORT || '3000';

app.listen(port, () => {
    console.log(`TypeScript with Express
         http://localhost:${port}/`);
});