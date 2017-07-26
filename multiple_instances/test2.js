const cp = require('child_process');
const async = require('async');
const fs = require('fs');

console.log('HAHAH');

setInterval(
  () => {
    async.seq(
      (cb) => { console.log('AA'); cb(null); },
      (cb) => {
        console.log('HAHAH');
        const foldersInOrder = cp.execSync('ls -ct tmp/ | xargs').toString().trim().split(' ');
        while (+cp.execSync('du -bs tmp | cut -f1').toString() > 10000) {
          const folderToDelete = foldersInOrder.pop();
          const folderCtime = fs.statSync(folderToDelete).toString().ctime;
          cp.execSync(`rm -rf ${folderToDelete}`);
          console.log(`${folderToDelete} (ctime = ${folderCtime}) has been deleted due to size limit`);
        }
        cb(null);
      },
    )();
  },
  5000,
);
