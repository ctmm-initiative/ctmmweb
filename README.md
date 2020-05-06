## Introduction

This is a web app for analyzing animal tracking data, built upon [ctmm R package](https://github.com/ctmm-initiative/ctmm). It's also an R package so you can use some features in your code directly.

## Install and run app

- Just run this in R console ([More detailed instructions](https://ctmm-initiative.github.io/ctmmwebdoc/articles/installation.html)):

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("ctmm-initiative/ctmmweb")

ctmmweb::app()  
```

- We also built [a windows installer](https://github.com/ctmm-initiative/ctmmweb/releases/download/v0.2.10/ctmmwebsetup.exe), which will download R installer if needed, install R and package, create shortcut for the app.


## Run app from our website

Just open [https://tiny.cc/ctmmweb](https://tiny.cc/ctmmweb) (Chrome recommended). This will have some resource limitations compare to run the app locally.

## References

- Check the [videos](https://ctmm-initiative.github.io/ctmmwebdoc/articles/demo.html), release [History](https://ctmm-initiative.github.io/ctmmwebdoc/news/index.html), package reference [website](https://ctmm-initiative.github.io/ctmmwebdoc)
- Suggested citation for the app/package:

```
X. Dong, C.H. Fleming, M.J. Noonan, and J.M. Calabrese. 2018. ctmmweb: A Shiny web app for the ctmm movement analysis package.
https://github.com/ctmm-initiative/ctmmweb
```
