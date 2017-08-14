# Introduction

This is a Shiny web application for [ctmm](https://github.com/ctmm-initiative/ctmm) [R package](https://cran.r-project.org/web/packages/ctmm/index.html). 

Check [the videos here](README-demo.md) for feature demonstrations.

# Running the app

There are several methods to run the app:

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

2. Install some dependency packages in R. 

    **You may need to restart R session and run them again when met error in package installation.**

```r
# restart R session first
remove.packages("ctmm")
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shiny")
devtools::install_github("ctmm-initiative/ctmm")
```

  [Some tips for Linux installation can be found here](README-install.md).

3. Pick stable version or latest version to run:

```r
# stable version
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.5")
# or latest version
shiny::runGitHub('ctmm-initiative/ctmm-webapp')
```

  Other dependency packages will be automatically installed in the first run of app. You may also need to restart R session in this procedure.

## Run app in local machine without Internet

The method above always download the app before run. It's a simple method to keep updated, but you can also download the app and run without Internet. 

After same steps of 1, 2:

3. Download the stable version from [releases page](https://github.com/ctmm-initiative/ctmm-webapp/releases), currently `v0.0.5`, or the [latest development version](https://github.com/ctmm-initiative/ctmm-webapp/archive/master.zip).

4. Unzip the zip to a folder, then run it with

```r
shiny::runApp("<absolute path to your app folder>")
```

# Usage

The web page have a side bar at left for each stage of analysis. Click the `help` buttons in each page for detailed documentations.
