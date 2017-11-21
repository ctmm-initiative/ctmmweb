# script that install packages, run app
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shiny")
devtools::install_github("rstudio/leaflet")
devtools::install_github("ctmm-initiative/ctmm")
devtools::install_github("r-lib/crayon")
# run app
shiny::runGitHub('ctmm-initiative/ctmm-webapp')
