# Setting up a Granatum1 Server

This **should** work on Google Cloud.

1.  Create Ubuntu 18 VM
2.  Add firewall rule to VM opening up TCP to ports above 8000
3.  SSH to VM.
4.  Run `machine_setup.sh` (or run the commands individually)
5.  Run `Rscript installDependencies.r`
6.  Verify that worked with `Rscript load_dependencies.r`
7.  Put a private key in ~/.ssh/id_rsa that has permission to read from granatum1 gitlab
8.  Do a git clone granatum1
9.  cd granatum1/server
10. `npm install .`
11. node app.js
