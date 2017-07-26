/* eslint no-console: ["error", { allow: ["log"] }] */

if (typeof process.env.DOMAIN_NAME === 'undefined') {
  process.stderr.write('Environment variable DOMAIN_NAME is undefined!\n');
  process.exit(1);
}

const SERVER_RESTART_CHECK_INTERVAL = 1000 * 5;
const LOGGING_BASEDIR = 'logs';

const fs = require('fs');
const cp = require('child_process');
const async = require('async');

let granatum = null;
let granatumLoggingFolder = null;

async.forever((cb) => {
  granatumLoggingFolder = `${LOGGING_BASEDIR}/${process.env.DOMAIN_NAME}-${Date.now()}`;
  granatum = cp.fork('./multiple_instances.js', [], {
    env: Object.assign(process.env, { LOGGING_DIR: granatumLoggingFolder }),
    silent: true,
  });
  granatum.stdout.on('data', (data) => {
    process.stdout.write(data);
  });
  granatum.stderr.on('data', (data) => {
    process.stderr.write(data);
  });
  granatum.on('exit', () => {
    console.log('Run Forever: Granatum has exited!');
    console.log('Run Forever: Granatum is being restarted!');
    cb(null);
  });
});

// this function kills the multiple_instances process asynchronously
// the messages are to ensure the restart happens after the demise of all child processes
const killGranatum = () => {
  granatum.send('prepare_termination');
  granatum.on('message', (msg) => {
    if (msg === 'prepare_termination_done') {
      granatum.kill('SIGINT');
    }
  });
};


// Check file flags to restart the server
setInterval(
  () => {
    const forceStartPath = `${granatumLoggingFolder}/force_restart`;
    const restartNeededPath = `${granatumLoggingFolder}/restart_needed`;
    const plzNoRestartPath = `${granatumLoggingFolder}/plz_no_restart`;

    fs.stat(forceStartPath, (err) => {
      if (err === null) {
        killGranatum();
        fs.unlink(forceStartPath, () => {
          console.log(`${forceStartPath} has been deleted`);
        });
      }
    });

    fs.stat(restartNeededPath, (err) => {
      if (err === null) {
        fs.stat(plzNoRestartPath, (errr) => {
          if (errr !== null) {
            killGranatum();
            fs.unlink(restartNeededPath, () => {
              console.log(`${restartNeededPath} has been deleted`);
            });
          } else {
            console.log('restart_needed is found, by plz_no_restart is present: no restart');
          }
        });
      }
    });
  },
  SERVER_RESTART_CHECK_INTERVAL,
);
