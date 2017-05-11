# Shiny web app for ctmm

## Introduction

This is a Shiny web application for [ctmm](https://cran.r-project.org/web/packages/ctmm/index.html) [R package](https://github.com/ctmm-initiative/ctmm). 

Check [the videos here](README-demo.md) for feature demonstrations.

## Running the app

There are several methods to run the app:

### Hosted app
With app [hosted in a website](https://ctmm.shinyapps.io/dashboardalpha/), you can use the app with just a browser. This is more intended for demonstration use because

- R is single threaded, so any computation heavy operations like loading large datasets, fitting models could block other users in same thread.
- The active hours per month is limited. 

### Download and run locally
You can run current repo locally:

1. Install [the latest R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/)
2. Install dependency packages in R. You need to install `ctmm` and `shiny` development version instead of CRAN version. 

    **You may need to restart R session and run same line again when met error.**

```r
# restart R session first
remove.packages("ctmm")
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shiny")
devtools::install_github("ctmm-initiative/ctmm", ref = "8332cb3")
```

3. Download the stable version from [releases page](https://github.com/ctmm-initiative/ctmm-webapp/releases), currently `v0.0.5`. You can also try the [latest development version](https://github.com/ctmm-initiative/ctmm-webapp/archive/master.zip).

4. Unzip the zip to a folder, then run it with

```r
shiny::runApp("<absolute path to your app folder>")
```

Other dependency packages will be automatically installed in the first run of app. You may also need to restart R session in this procedure.

### Download on demand then run locally

You can run the remote files directly. After same preparation steps 1 and 2, run

```r
# stable version
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.5")
# or latest version
shiny::runGitHub('ctmm-initiative/ctmm-webapp')
```

## Usage

The web page have a side bar at left for each stage of analysis. Click the `help` buttons in each page for detailed documentations.
