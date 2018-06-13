# Setting up a Granatum1 Server

This **should** work on Google Cloud.

1.  Create Ubuntu 18 VM.
2.  SSH to VM.
3.  Run `machine_setup.sh` (or run the commands individually)
4.  Run `Rscript installDependencies.r`
5.  Verify that worked with `Rscript load_dependencies.r`
6.  Put a private key in ~/.ssh/id_rsa that has permission to read from granatum1 gitlab
7.  Do a git clone granatum1
8.  cd granatum1/server
9.  `npm install .`
10. node app.js
