## Package installation

If you met problem in the one line script, you can run these lines one by one and locate the specific problem.

**You may need to restart R session and run them again when met error in package installation.**

```r
# restart R session first
remove.packages("ctmm")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/shiny")
devtools::install_github("ctmm-initiative/ctmm")
```

## Run specific version of app

```r
# stable version
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.6")
# or latest version
shiny::runGitHub('ctmm-initiative/ctmm-webapp')
```

## Run app in local machine without Internet

The method above always download the app before run. You can also download the app and run without Internet. 

With R and dependency packages installed:

1. Download the stable version from [releases page](https://github.com/ctmm-initiative/ctmm-webapp/releases), currently `v0.0.6`, or the [latest development version](https://github.com/ctmm-initiative/ctmm-webapp/archive/master.zip).

2. Unzip the zip to a folder, then run it with

    ```r
    if (!require("devtools"))
      install.packages("devtools")
    devtools::install_github("rstudio/shiny")
    devtools::install_github("ctmm-initiative/ctmm")
    devtools::install_github("r-lib/crayon")
    shiny::runApp("<absolute path to your app folder>")
    ```


## Linux
- Following instructions [here for installing R](https://cloud.r-project.org/bin/linux/ubuntu/) (assuming Ubuntu or Linux Mint). Make sure to install `r-base-dev` too.
- RStudio have installer for Linux.
- Some of the packages needed for the app need extra steps to install properly:
  + `devtools` need `libcurl`, `ssl` if it's not already installed in your system. You can watch error messages to check what is required. I installed them in Linux Mint 18 with
  
        sudo apt-get install libcurl4-openssl-dev
        sudo apt-get install libssl-dev
  
  + `ctmm` need `rgdal`, which need another two libraries in linux:
  
        sudo apt-get update && sudo apt-get install libgdal-dev libproj-dev
  
## Known bugs
- When you start the app in RStudio, it was opened with embeded browser in RStudio, which has these known limitations:
  - Windows: log scale slider not shown properly; download dialog don't prompt proper file name by default.
  - Mac: download file will open a blank page, and the dowload function was executed twice so there will be two entries in report.
  - Linux: download dialog do not respond.
- There is a [known bug](https://github.com/Rdatatable/data.table/issues/2137) caused by `data.table`, `openMP` and `mcapply` in R 3.4. So it's possible to see this error when fitting models in parallel under Mac/Linux:

    ```r
    Assertion failure at kmp_runtime.cpp(6480): __kmp_thread_pool == __null.
    OMP: Error #13: Assertion failure at kmp_runtime.cpp(6480).
    ```

The error is pretty random and hard to reproduce, but usually you can restart R session and it should dissappear.  
