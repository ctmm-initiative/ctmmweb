## version 0.2.3, 2019/02/07, Misc Updates
- Adding all `ctmm` internal datasets to app. 
- Some of the datasets are anonymized, i.e. no referencee information available for location and time, only `x`, `y` and `t` which are relative values. A Pseudonymize process is added to simulate data with certain origin in location and time, so that the data can be analysized with app properly.
- `Speed/Distance` page is added, which will estimate animals' speed and distance traveled.
- Using unicode symbols in model summary, following changes in `ctmm`
- The resolution of saved plot in `Save Progress` can be customized in sidebar.
- Fitted model `ctmm` objects will be included in `Save Progress` zip, so user can load them to a R session and continue the analysis.
- Update on Movebank part:
    - All attributes of data can be downloaded now, previously only several columns can be get from API. Thanks Movebank for implementing this suggestion!
    - Some attributes are obsoleted and no longer used, like number of deployments.
    - Better error handling when there is no data available or need license terms. Users will no longer see irrelevant warning messages.
- Rewrite installaion instructions to make it more clear.
- This is a official release with more tests done against `ctmm` 0.5.3 CRAN release. Release version can be installed from our own repo using like this:

    ```r
    install.packages("ctmmweb", 
                     repos = c(getOption("repos"),
                               "https://ctmm-initiative.github.io/ctmm_repo/"))
    ```

## version 0.2.0, 2018/09/07, Telemetry Errors
- Dealing with bugs, conflicts caused by `devtools::install_github`, dependency packages, alternative installation methods.
- Units in tables were moved to column header so the columns can be sorted properly.
- Capture warning/error in importing data, notify user when needed.
- Take a location subset by cropping.

#### Telemetry Errors
- Report Error information
- Calibrate data with calibration data
- Outlier page will use Error information

#### Model Selection Page
- The design, workflow and reactive logic have been improved
- [new features added](https://github.com/ctmm-initiative/ctmmweb/issues/54) on home range weights, multiple sampling schedules and pooling variograms.
- Refit based on existing models, fine-tune model results as initial condition of next fit, showing multiple results in variograms.
- Model result page improved and condensed.

#### Home Range/Occurrence plots
- Added much more plot control to home range, occurrence and overlap plot in a consistent way.

## version 0.1.0, 2018/02/08, adjust package, Overlap
- [export most features of app as package functions](https://github.com/ctmm-initiative/ctmmweb/issues/41), so user can use them in their own analysis with more flexibility 
- Established workflow with package functions.
- Finished vignettes for package usage. 
- Changed repo name to match package name
- Moved package website to separate repo to reduce package download size and time.
- Overlap page finished.

## version 0.0.9, 2017/12/04, convert to Package
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
