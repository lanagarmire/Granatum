const async = require('async');

async.timesSeries(10, (n, next) => {
  console.log(n);
  setTimeout(() => next(), 500);
});
