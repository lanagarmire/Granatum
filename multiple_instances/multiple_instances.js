/* eslint no-bitwise: ["error", { "int32Hint": true }] */


const PORT_START_POINT = 8101;
const NUM_PORTS = 20;
const BOOKMARK_TIMEOUT = 60 * 60 * 24 * 30; // in sec
const BOOKMARK_TIMEOUT_CHECK_INTERVAL = 60 * 1000; // in milli-sec
const SERVER_LOAD_CHECK_INTERVAL = 60 * 1000; // in milli-sec

const express = require('express');
const cp = require('child_process');
const async = require('async');
const fs = require('fs');
const winston = require('winston');
const rimraf = require('rimraf');

const app = express();

const loggingDir = process.env.LOGGING_DIR;

fs.mkdirSync(loggingDir);

winston.configure({});

winston.add(winston.transports.File, {
  json: false,
  filename: `${loggingDir}/main-process.log`,
});
winston.add(winston.transports.Console, {
  colorize: true,
  timestamp: () => new Date(),
});


winston.info(`your domain name is: ${process.env.DOMAIN_NAME}`);
winston.info(`logging directory is: ${loggingDir}`);


// a helper function for generating 0..n
function range(a, b) {
  const out = [];
  for (let i = a; i < b; i += 1) {
    out.push(i);
  }

  return out;
}

function indexOfMin(arr) {
  if (arr.length === 0) {
    return -1;
  }

  let min = arr[0];
  let minIndex = 0;

  for (let i = 1; i < arr.length; i += 1) {
    if (arr[i] < min) {
      minIndex = i;
      min = arr[i];
    }
  }

  return minIndex;
}


// initialize all instances

const instances = [];

// TODO: make sure all instances are successfully initialized
// (not bumped out because of port occupation)

for (let i = 0; i < NUM_PORTS; i += 1) {
  const port = PORT_START_POINT + i;
  const p = cp.spawn('Rscript', ['-e', `setwd(".."); source("load_dependencies.R"); enableBookmarking("server"); runApp(".", host="0.0.0.0", port=${port})`]);
  const pid = p.pid;
  const logger = new winston.Logger();
  logger.configure({});
  logger.add(winston.transports.File, {
    json: false,
    filename: `${loggingDir}/instance-${i}.log`,
    formatter: opt => opt.message,
  });
  p.stdout.on('data', (data) => {
    logger.info(`${data}`, { instance: i, type: 'stdout' });
  });
  p.stderr.on('data', (data) => {
    logger.warn(`${data}`, { instance: i, type: 'stderr' });
  });
  p.on('exit', (code) => {
    const pp = instances.find(ii => ii.pid === p.pid);
    winston.warn(`The process with pid = ${pp.pid} and port = ${pp.port} has ended, the exit code = ${code}.`);
  });

  winston.info(`pid: ${pid}; port: ${port}`);
  instances.push({ p, port, pid, logger });
}

process.on('message', (m) => {
  if (m === 'prepare_termination') {
    winston.info('Received a message to prepare termination.');
    winston.info('Terminating child processes ...');
    async.eachOf(
      instances,
      (instance, key, callback) => {
        instance.p.kill();
        instance.p.on('exit', () => {
          winston.info(`The process pid = ${instances[key].pid} and port = ${instances[key].port} has been killed.`);
          callback();
        });
      },
      () => {
        winston.info('All child processes have been terminated ...');
        process.send('prepare_termination_done');
      },
    );
  }
});

app.get('/', (req, res) => {
  async.times(
    NUM_PORTS,
    (offset, cb) => cp.exec(`netstat -anp | grep :${PORT_START_POINT + offset} | grep ESTABLISHED | wc -l`, (e, out) => { cb(null, +out); }),
    (e, rs) => {
      res.redirect(`http://${process.env.DOMAIN_NAME}:${PORT_START_POINT + indexOfMin(rs)}${req.originalUrl}`);
    },
  );
});


setInterval(
  () => {
    const basedir = '../shiny_bookmarks';
    winston.info('---- Bookmark Folder Check ----');
    fs.readdir(basedir, (e, files) => {
      for (let i = 0; i < files.length; i += 1) {
        const f = files[i];
        const disconnectedFilename = `${basedir}/${f}/disconnected`;
        fs.readFile(disconnectedFilename, (ee, data) => {
          if (ee === null) {
            const lastLogoutTime = +data;
            const currentTime = Date.now() / 1000 | 0;
            const secondsElapsed = currentTime - lastLogoutTime;
            winston.info(`${f}: secondsElapsed: ${secondsElapsed}`);

            if (secondsElapsed > BOOKMARK_TIMEOUT) {
              const dirname = `${basedir}/${f}`;
              winston.info(`${dirname} has timed out and is going to be deleted`);
              rimraf(dirname, () => {
                winston.info(`${dirname} is successfully deleted`);
              });
            }
          } else {
            winston.info(`${f}: no "disconnected" file`);
          }
        });
      }
    });
  },
  BOOKMARK_TIMEOUT_CHECK_INTERVAL,
);

setInterval(
  () => {
    async.map(
      range(PORT_START_POINT, PORT_START_POINT + NUM_PORTS),
      (port, cb) => cp.exec(`netstat -anp | grep :${port} | grep ESTABLISHED | wc -l`, (e, out) => { cb(null, +out); }),
      (e, rs) => {
        winston.info(`${rs}`, { type: 'server-load' });
      },
    );
  },
  SERVER_LOAD_CHECK_INTERVAL,
);

app.listen(8100);
