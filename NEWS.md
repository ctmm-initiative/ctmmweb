# 2017/11/28, v0.0.9 pack into package - in process
- The app has grown to a certain degree, when there are some features can be useful independently for advanced R users. Packing app into a package make reusing these features easier, and the app distribution will also be familiar for experienced R users.
- This is a time consuming process which involved:
    + changing app structure to make it work both as a function and an individual shiny app for hosting server
    + untangling a lot of dependencies
    + using full qualified function names everywhere
    + minimizing modifications to user environment
    + following other package development requirements
- For the version after this change, you cannot launch specific version using previous method. You can install certain verion of package with

```r
devtools::install_github("ctmm-initiative/ctmm-webapp", ref = "v0.0.9")
ctmmweb::app()
```

# 2017/11/09, v0.0.8 Map
- Interactive online map for animal locations, home ranges are added.
- For bigger dataset, heatmap is generated first since it's faster to move around in a heatmap. After you switched to the point map, the bounds of heatmap will be applied to point map so you will see roughly same area.

# 2017/10/24, v0.0.7 Work Report, save/load cache
- Every meaningful action is logged in a html report. Report and plot pictures can be saved into a zip.
- Time consuming calculations are cached so they can rerun instantly. The cache can be save/load for next run.

# 2017/08/25, v0.0.6 Model Selection, Home Range, Occurrence Distribution

- Model selection
- Home Range
- Occurrence Distribution

# 2017/05/11, v0.0.5 Variograms

- Variograms for models

# 2017/04/21, v0.0.4 Movebank import, Filter Outlier

- Movebank Import
- Plot individuals by selected rows or current page
- Filter outliers by distance to median center or speed

[Demo of usage](http://www.youtube.com/watch?v=nyUe6PIVfyU)

Run this version with

```r
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.4")
```

# 2017/02/28, v0.0.2 data exploration, plots, time subsetting

This version have much more finished features in data exploration, plots, and time subsetting.

[Demo of features](http://www.youtube.com/watch?v=7vRktLa76Ho)

Though the latter stages of modeling is not connected yet, because the modeling work flow may need some reconsideration to incorporate a flexible workflow into a linear page structure.

Run this version with

```r
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.2")
```

# 2017/01/27, v0.0.1 First rudimental version

This version is a proof of concept. Every page is very simple but the app provided a complete workflow starting from import data to the final home range estimation.

Later versions will have much more sophisticated features but only work with selected pages.

To run this version you need to install shinyjs (which is not required in later versions).

```r
install.packages("shinyjs")
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.1")
```