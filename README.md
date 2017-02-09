# Prototype of Shiny web application for ctmm package

## Introduction

This is a prototype Shiny app for [ctmm](https://cran.r-project.org/web/packages/ctmm/index.html) R package. This repo is used for beta testing of the development version. The app can be run from github directly so it will be easy to distribute the test.

## Installation

[The first beta version is hosted in shinyapps.io](https://ctmm.shinyapps.io/dashboard1/) so you can open it with browser.

[The current alpha version is here](https://ctmm.shinyapps.io/dashboardalpha/).

You can also run it locally:

1. Install [the latest R](https://www.r-project.org/), currently 3.3.2. Lower version may not work with ctmm.
2. Install dependency packages in R. To use the plot zoom function with fixed axes scales correctly, you will need the development version of Shiny.
	
```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shiny")
```

3. Run the Shiny app with

	`shiny::runGitHub('ctmm-shiny-prototype', 'xhdong-umd')`
	
	
## Usage

**Note: every plot in plot 3 itself have x y axes in same scale, but scales across plots need to be fixed in next version.**

The web page have a side bar at left for each stage of analysis. The top right corner have some links for project background and help (the "messages" format is not perfect for this purpose, I will polist it later).

Intro page serve as help for the analysis. (Current page is just a placeholder)

Upload page can upload movebank format data or use ctmm package internal data. Right now the app always take the first animal if there are multiple animals in data. Once data is loaded, the summary and plots (basic plot and ggplot2) will be generated.

Time-lag page create variogram plots with different parameters and user selected zoom.

Model page select the best model according to guessed parameters. In next step user selected parameters will be added. Note the model fitting process could take 1.5 mins for ctmm internal data. There is a progress bar indicating the process but it didn't reflect the real progress yet.

Home range page estimate home range based on best model selected. More plot can be added to this page in nest stage.

Report page will generate a work report include all the information and result generated in the analysis. Right now this page is just a placeholder.
