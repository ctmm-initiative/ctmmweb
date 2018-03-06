## current
- Update outlier page to use latest ctmm functions which take errors into account for both distance and speed.
- Use home range plot for overlap location plot tab.
- `app()` now open in system default browser instead of the RStudio embedded browser to avoid compatibility problems.
- added package installation date in message menu, console log and work report. This can help to identify which version was installed. I didn't find an automatic way to save build date in package building.
- package can be installed through base R command now

    install.packages("ctmmweb", type = "source", 
                     repos = "https://ctmm-initiative.github.io/drat/")

## version 0.1.0, 2018/02/08
- Moved package website to separate repo to reduce package download size and time.
- Overlap page finished.
- Changed cores parameter for `par_lapply`

## version 0.0.9.9, 2017/12/27
- Changed repo name to match package name
- [export most features of app as package functions](https://github.com/ctmm-initiative/ctmmweb/issues/41) 
- Established workflow with package functions.
- Finished vignette of `package usage`. 
- Renamed `Save Cache` to `Save Data` to make it easier to save cleaned data.

## version 0.0.9, 2017/12/04, Package
- The app has grown to a certain degree, when there are some features can be useful independently for advanced R users. Packing app into a package make reusing these features easier, and the app distribution will also be familiar for experienced R users.
- Reference website is built
- This is a time consuming process which involved:
    + changing app structure to make it work both as a function and an individual shiny app for hosting server
    + untangling a lot of dependencies
    + using full qualified function names everywhere
    + minimizing modifications to user environment
    + following other package development requirements
    + building a website
- For the version after this change, you cannot launch specific version using previous method. You can install certain version of package with

```r
devtools::install_github("ctmm-initiative/ctmmweb", ref = "v0.0.9")
ctmmweb::app()
```

## version 0.0.8, 2017/11/09, Map
- Interactive online map for animal locations, home ranges are added.
- For bigger dataset, heatmap is generated first since it's faster to move around in a heatmap. After you switched to the point map, the bounds of heatmap will be applied to point map so you will see roughly same area.

## version 0.0.7, 2017/10/24, Work Report, save/load cache 
- Every meaningful action is logged in a html report. Report and plot pictures can be saved into a zip.
- Time consuming calculations are cached so they can rerun instantly. The cache can be save/load for next run.

## version 0.0.6, 2017/08/25, Models
- Model Selection
- Home Range
- Occurrence Distribution

## version 0.0.5, 2017/05/11, Variograms
- Variograms for models

## version 0.0.4, 2017/04/21, Movebank import, Filter Outlier 
- Movebank Import
- Plot individuals by selected rows or current page
- Filter outliers by distance to median center or speed

[Demo of usage](http://www.youtube.com/watch?v=nyUe6PIVfyU)

Run this version with

```r
shiny::runGitHub('ctmm-initiative/ctmmweb', ref = "v0.0.4")
```

## version 0.0.2, 2017/02/28, Data exploration, plots, time subsetting

This version have much more finished features in data exploration, plots, and time subsetting.

[Demo of features](http://www.youtube.com/watch?v=7vRktLa76Ho)

Though the latter stages of modeling is not connected yet, because the modeling work flow may need some reconsideration to incorporate a flexible workflow into a linear page structure.

Run this version with

```r
shiny::runGitHub('ctmm-initiative/ctmmweb', ref = "v0.0.2")
```

## version 0.0.1, 2017/01/27, First rudimental version 

This version is a proof of concept. Every page is very simple but the app provided a complete workflow starting from import data to the final home range estimation.

Later versions will have much more sophisticated features but only work with selected pages.

To run this version you need to install shinyjs (which is not required in later versions).

```r
install.packages("shinyjs")
shiny::runGitHub('ctmm-initiative/ctmmweb', ref = "v0.0.1")
```
