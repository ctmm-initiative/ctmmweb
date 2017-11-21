# pack into package

# Map

# Work Report, save/load session

# v0.0.6 Model Selection, Home Range, Occurrence Distribution

- Model selection
- Home Range
- Occurrence Distribution

# v0.0.5 Variograms

- Variograms for models

# v0.0.4 Movebank import, Filter Outlier

- Movebank Import
- Plot individuals by selected rows or current page
- Filter outliers by distance to median center or speed

[Demo of usage](http://www.youtube.com/watch?v=nyUe6PIVfyU)

Run this version with

```r
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.4")
```

# v0.0.2 data exploration, plots, time subsetting

This version have much more finished features in data exploration, plots, and time subsetting.

[Demo of features](http://www.youtube.com/watch?v=7vRktLa76Ho)

Though the latter stages of modeling is not connected yet, because the modeling work flow may need some reconsideration to incorporate a flexible workflow into a linear page structure.

Run this version with

```r
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.2")
```

# v0.0.1 First rudimental version

This version is a proof of concept. Every page is very simple but the app provided a complete workflow starting from import data to the final home range estimation.

Later versions will have much more sophisticated features but only work with selected pages.

To run this version you need to install shinyjs (which is not required in later versions).

```r
install.packages("shinyjs")
shiny::runGitHub('ctmm-initiative/ctmm-webapp', ref = "v0.0.1")
```