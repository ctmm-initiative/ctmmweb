# script that install packages, run app
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shiny")
devtools::install_github("ctmm-initiative/ctmm")
# run app
shiny::runGitHub('ctmm-initiative/ctmm-webapp')
