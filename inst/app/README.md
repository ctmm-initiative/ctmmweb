# Introduction

This is a Shiny web application for [ctmm](https://github.com/ctmm-initiative/ctmm) [R package](https://cran.r-project.org/web/packages/ctmm/index.html). 

Check [the videos here](README-demo.md) for feature demonstrations. The `help` buttons in each page also have detailed documentations for the specific feature.

# Running the app

## Hosted app
Just open [the hosted app](https://ctmm.shinyapps.io/ctmmweb/) with browser (Chrome recommended). 

This is more intended for demonstration purposes because

- R is single threaded, so any computation heavy operations like loading large datasets, fitting models could block other users in same thread.
- The active hours per month is limited. 

## Run app in local machine with Internet

You can run app in your local machine with these steps:

1. Install [the latest R](https://www.r-project.org/). 

    Note R 3.4 bring some big changes, so it's better to upgrade all the existing packages if you have earlier version installed.
    
    [RStudio](https://www.rstudio.com/products/rstudio/download/) is not required but recommended.

2. Start R or RStudio, run this line in console.

    ```r
    source("https://raw.githubusercontent.com/ctmm-initiative/ctmm-webapp/master/run.R")
    ```

    The script above will install dependency packages and run app. Sometimes you may need to restart R in installing packages. 
    Running in RStudio will use RStudio's embeded browser by default. In windows/Linux this has known compatibility problems(slider label not in log scale, download file name not formated automatically, download dialog lost response). You can click the `open in browser` button in the browser top bar to use system browser instead. Chrome is recommended.
  
  More details about installation, running specific version of app, or running app without Internet can be [found here.](README-install.md) 
