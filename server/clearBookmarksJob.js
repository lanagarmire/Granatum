const fs = require('fs')
const BOOKMARK_TIMEOUT = 60 * 60 * 24 * 30 // in sec
const BOOKMARK_TIMEOUT_CHECK_INTERVAL = 60 * 1000 // in milli-sec

module.exports = winston => {
  setInterval(() => {
    const basedir = '../shiny_bookmarks'
    winston.info('---- Bookmark Folder Check ----')
    fs.readdir(basedir, (readdirErr, files) => {
      for (let i = 0; i < files.length; i += 1) {
        const filePath = files[i]
        const disconnectedFilename = `${basedir}/${filePath}/disconnected`
        fs.readFile(disconnectedFilename, (readFileErr, data) => {
          if (readFileErr === null) {
            const lastLogoutTime = parseInt(data)
            const currentTime = (Date.now() / 1000) | 0
            const secondsElapsed = currentTime - lastLogoutTime
            winston.info(`${filePath}: secondsElapsed: ${secondsElapsed}`)

            if (secondsElapsed > BOOKMARK_TIMEOUT) {
              const dirname = `${basedir}/${filePath}`
              winston.info(`${dirname} has timed out and is going to be deleted`)
              rimraf(dirname, () => winston.info(`${dirname} is successfully deleted`))
            }
          } else {
            winston.info(`${filePath}: no "disconnected" file`)
          }
        })
      }
    })
  }, BOOKMARK_TIMEOUT_CHECK_INTERVAL)
}
