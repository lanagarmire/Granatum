# Ubuntu
sudo apt update

sudo apt install nodejs
sudo apt install r-base
sudo chmod o+w /usr/local/lib/R/site-library

sudo apt install libssl-dev
sudo apt install libcurl4-openssl-dev
sudo apt install libxml2-dev
sudo apt install libcairo2-dev
sudo apt install libxt-dev

git clone git@gitlab.com:breckuh/granatum1.git
cd granatum1
mkdir shiny_bookmarks # todo: move this to code


cd ~
git clone https://github.com/tj/n
cd n
make install
cd ~
