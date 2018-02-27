## Introduction

This is a web app package for analyzing animal tracking data as a continuous-time stochastic processes.

It's a shiny app built upon [ctmm R package](https://github.com/ctmm-initiative/ctmm). This is also a R package so you can reuse some functionalities in your own R analysis. 

Suggested citation for the app/package:

```
Dong, X., C.H. Fleming, and J.M. Calabrese. 2018. ctmmweb: A Shiny web app for the ctmm movement analysis package.
https://github.com/ctmm-initiative/ctmmweb
```

- Check [the videos](https://ctmm-initiative.github.io/ctmmwebdoc/articles/demo.html) for feature demonstrations. 
- The `help` buttons in each page also have detailed documentations for the specific feature.
- [The package reference website](https://ctmm-initiative.github.io/ctmmwebdoc/) provided some tips on installation and documentations for package functions.
- [Release History](https://ctmm-initiative.github.io/ctmmwebdoc/news/index.html)

## Running the app

### Bundled with package

1. Install [the latest R](https://www.r-project.org/). 

    Note R 3.4 bring some big changes, so it's better to upgrade all the existing packages if you have earlier version installed.
    
    [RStudio](https://www.rstudio.com/products/rstudio/download/) is not required but recommended.

2. Start R or RStudio, run these in console to install dependency packages. Sometimes you may need to restart R in the process.

    ```r
    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("ctmm-initiative/ctmmweb")
    ```

3. Run the app in one of following methods:

    ```r
    library(ctmmweb)
    # run app
    app()
    # run app with path to movebank format csv file
    app("path_to_movebank_format_csv")
    # run app with existing telemetry object or list in R environment
    library(ctmm)
    data(buffalo)
    app(buffalo)
    ```
  
    This will launch the app locally with system default browser. Chrome is recommended for compatibility reasons. You can copy the url into Chrome if it was not launched by Chrome. After the browser windows was closed, you may need to stop the R session in R console manually (`Esc` or `Ctrl+C`) to quit the app.
  
  More details about installation and compatibility problems can be [found here.](https://ctmm-initiative.github.io/ctmmwebdoc/articles/installation.html) 

### Hosted app
Just open [the hosted app](https://ctmm.shinyapps.io/ctmmweb/) with browser (Chrome recommended). 

This is more intended for demonstration purposes because

- R is single threaded, so any computation heavy operations like loading large datasets, fitting models could block other users in same thread. We do have mutiple workers avaialble in the host but the resources are still limited.
- The active hours per month is limited. 
