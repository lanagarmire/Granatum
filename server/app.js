/* eslint no-bitwise: ["error", { "int32Hint": true }] */

const express = require('express')
const child_process = require('child_process')
const async = require('async')
const fs = require('fs')
const winston = require('winston')
const rimraf = require('rimraf')
const argv = require('minimist')(process.argv.slice(2))

const PORT_START_POINT = 8101
const NUM_PORTS = argv.ports ? parseInt(argv.ports) : 4
const SERVER_LOAD_CHECK_INTERVAL = 60 * 1000 // in milli-sec
// const INSTANCE_UP_CHECK_INTERVAL = 60 * 1000; // in milli-sec

const app = express()

const loggingDir = process.env.LOGGING_DIR || 'logs/'
const domainName = process.env.DOMAIN_NAME || 'granatum1.garmiregroup.org'
const hostIp = process.env.HOST_IP || '0.0.0.0'

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
const range = (a, b) => {
  const out = []
  for (; a < b; a += 1) {
    out.push(a)
  }

  return out
}

const indexOfMin = arr => arr.indexOf(Math.min(...arr))

// initialize all instances
const instances = Array(NUM_PORTS)

// Set process title to make it easy to check running status of this app from other apps
// Note: process.title is limited in length to length of initial command
process.title = process.title.length < 5 ? 'grn1' : 'gran1node'

// TODO: make sure all instances are successfully initialized
// (not bumped out because of port occupation)

const startInstance = instanceIndex => {
  const port = PORT_START_POINT + instanceIndex
  const rProcess = child_process.spawn('Rscript', [
    '-e',
    `setwd(".."); source("load_dependencies.R"); enableBookmarking("server"); runApp(".", host="${hostIp}", port=${port})`
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

require('./clearBookmarksJob.js')(winston)

app.listen(80)
