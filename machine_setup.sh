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

git clone git@github.com:lanagarmire/Granatum.git
cd granatum1
mkdir shiny_bookmarks # todo: move this to code

# Allow node on port 80:
sudo apt-get install libcap2-bin
sudo setcap cap_net_bind_service=+ep /usr/local/bin/node


# Increase R's max DLLs
echo "R_MAX_NUM_DLLS=500" > ~/.Renviron


cd ~
git clone https://github.com/tj/n
cd n
make install
cd ~
