const join = require('path').join
const spawn = require('child_process').spawn

const proc = spawn(
  'pike',
  ['-M', join(__dirname, '..', 'pike-modules'), join(__dirname, 'main.pike')],
  {
    stdio: 'inherit',
  }
)

proc
  .on('message', (message) => {
    console.log(`Got data:`, message)
  })
  .on('error', (err) => {
    console.log(`Got error:`, err)
  })
  .on('close', (code) => {
    console.log(`Closed:`, code)
  })
  .on('disconnect', () => {
    console.log(`Diconnect`)
  })
  .on('exit', (code) => {
    console.log(`Exit:`, code)
  })
