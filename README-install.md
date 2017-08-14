
## Linux
- Following instructions [here for installing R](https://cloud.r-project.org/bin/linux/ubuntu/) (assuming Ubuntu or Linux Mint). Make sure to install `r-base-dev` too.
- RStudio have installer for Linux.
- Some of the packages needed for the app need extra steps to install properly:
  + `devtools` need `libcurl`, `ssl` if it's not already installed in your system. You can watch error messages to check what is required. I install them in Linux Mint 18 with
  
        sudo apt-get install libcurl4-openssl-dev
        sudo apt-get install libssl-dev
  
  + `ctmm` need `rgdal`, which need another two libraries in linux:
  
        sudo apt-get update && sudo apt-get install libgdal-dev libproj-dev
  
