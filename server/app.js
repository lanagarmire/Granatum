/* eslint no-bitwise: ["error", { "int32Hint": true }] */

const PORT_START_POINT = 8101
const NUM_PORTS = 20
const BOOKMARK_TIMEOUT = 60 * 60 * 24 * 30 // in sec
const BOOKMARK_TIMEOUT_CHECK_INTERVAL = 60 * 1000 // in milli-sec
const SERVER_LOAD_CHECK_INTERVAL = 60 * 1000 // in milli-sec
// const INSTANCE_UP_CHECK_INTERVAL = 60 * 1000; // in milli-sec

const express = require('express')
const child_process = require('child_process')
const async = require('async')
const fs = require('fs')
const winston = require('winston')
const rimraf = require('rimraf')

const app = express()

const loggingDir = process.env.LOGGING_DIR || 'logs/'
const domainName = process.env.DOMAIN_NAME || 'granatum.garmiregroup.org'

winston.configure({})

winston.add(winston.transports.File, {
  json: false,
  filename: `${loggingDir}/main-process.log`
})
winston.add(winston.transports.Console, {
  colorize: true,
  timestamp: () => new Date()
})

winston.info(`your domain name is: ${domainName}`)
winston.info(`logging directory is: ${loggingDir}`)

// a helper function for generating 0..n
const range = function(a, b) {
  const out = []
  for (; a < b; a += 1) {
    out.push(a)
  }

  return out
}

const indexOfMin = arr => arr.indexOf(Math.min(...arr))

// initialize all instances

const instances = Array(NUM_PORTS)

// TODO: make sure all instances are successfully initialized
// (not bumped out because of port occupation)

const startInstance = instanceIndex => {
  const port = PORT_START_POINT + instanceIndex
  const rProcess = child_process.spawn('Rscript', [
    '-e',
    `setwd(".."); source("load_dependencies.R"); enableBookmarking("server"); runApp(".", host="0.0.0.0", port=${port})`
  ])
  const pid = rProcess.pid
  const logger = new winston.Logger()
  logger.configure({})
  logger.add(winston.transports.File, {
    json: false,
    filename: `${loggingDir}/instance-${instanceIndex}.log`,
    formatter: opt => opt.message
  })
  rProcess.stdout.on('data', data => {
    logger.info(`${data}`, { instance: instanceIndex, type: 'stdout' })
  })
  rProcess.stderr.on('data', data => {
    console.log(data.toString())
    logger.warn(`${data}`, { instance: instanceIndex, type: 'stderr' })
  })
  rProcess.on('exit', code => {
    const instanceObject = instances.find(instance => instance.pid === rProcess.pid)
    winston.warn(
      `The process with pid=${instanceObject.pid} port=${
        instanceObject.port
      } has ended, the exit code=${code}.`
    )
    instances[instanceIndex] = startInstance(instanceIndex)
  })

  winston.info(`pid=${pid} port=${port} has been started.`)

  return { rProcess, port, pid, logger }
}

for (let i = 0; i < NUM_PORTS; i += 1) {
  instances[i] = startInstance(i)
}

app.get('/', (req, res) => {
  async.times(
    NUM_PORTS,
    (offset, cb) =>
      child_process.exec(
        `netstat -anp | grep :${PORT_START_POINT + offset} | grep ESTABLISHED | wc -l`,
        (e, out) => cb(null, parseInt(out))
      ),
    (asyncTimesErr, timesReults) =>
      res.redirect(
        `http://${domainName}:${PORT_START_POINT + indexOfMin(timesReults)}${req.originalUrl}`
      )
  )
})

setInterval(() => {
  const basedir = '../shiny_bookmarks'
  winston.info('---- Bookmark Folder Check ----')
  fs.readdir(basedir, (readdirErr, files) => {
    for (let i = 0; i < files.length; i += 1) {
      const filePath = files[i]
      const disconnectedFilename = `${basedir}/${filePath}/disconnected`
      fs.readFile(disconnectedFilename, (readFileErr, data) => {
        if (err === null) {
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

async.forever(next => {
  async.map(
    range(PORT_START_POINT, PORT_START_POINT + NUM_PORTS),
    (port, cb) =>
      child_process.exec(
        `netstat -anp | grep :${port} | grep ESTABLISHED | wc -l`,
        (execErr, out) => cb(null, parseInt(out))
      ),
    (asyncFinishErr, mapResults) => {
      winston.info(`${mapResults}`, { type: 'server-load' })
      setTimeout(() => next(), SERVER_LOAD_CHECK_INTERVAL)
    }
  )
})

app.listen(8100)
