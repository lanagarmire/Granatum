# Granatum

Granatum is a graphical single-cell RNA-seq (scRNA-seq) analysis pipeline for genomics scientists.

Please Cite: Zhu, Xun et al. [Granatum: A Graphical Single-Cell RNA-Seq Analysis Pipeline for Genomics Scientists](https://genomemedicine.biomedcentral.com/articles/10.1186/s13073-017-0492-3). Genome Medicine 9.1 (2017)

### Try Granatum

You can use Granatum on our cloud instance running at [http://granatum.dcmb.med.umich.edu/](http://granatum.dcmb.med.umich.edu) or clone this repository and setup a server of your own locally or in the cloud.

Besides, we also have an updated version named GranatumX. You can also try GranatumX at [http://granatumx.garmiregroup.org](http://garmiregroup.org/granatumx/app).

### Setting up a Granatum1 Server on Google Cloud

This should also mostly work for other targets

This **should** work on Google Cloud.

1.  Create Ubuntu 18 VM
2.  SSH to VM.
3.  Run `machine_setup.sh` (or run the commands individually)
4.  Run `Rscript installDependencies.r`
5.  Verify that worked with `Rscript load_dependencies.r`
6.  Do a `git clone https://github.com/lanagarmire/granatum`
7.  cd granatum1/server
8.  `npm install .`
9.  `node app.js`
10. Make sure both Google Firewall and Ubuntu Firewall are open for Granatum ports
11. To run on port 80 you need to allow node to run on privileged ports:
12. Ubuntu: `sudo apt-get install libcap2-bin; sudo setcap cap_net_bind_service=+ep /usr/local/bin/node`
13. Fedora: `sudo yum install libcap-devel; sudo setcap cap_net_bind_service=+ep /usr/local/bin/node`
14. Increase R's default R_MAX_NUM_DLLS `echo "R_MAX_NUM_DLLS=500" > ~/.Renviron`
