# initialize renv
renv::init()
#
# enable the INBO universe
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
#
# install checklist
install.packages("checklist")
#
# set-up project
checklist::setup_project(".")
#
# install packages
pkgs <- c(
  "tidyverse",
  "cyclocomp",
  "quarto",
  "INBOtheme"
  )
pkgs_installed <- pkgs %in% rownames(installed.packages())
if(any(pkgs_installed == FALSE)){
  install.packages(pkgs[!pkgs_installed])
}
#
#
# write renv lockfile
renv::snapshot()
#
# author information can be adapted manually via
# C:\Users\janne_adolf\AppData\Roaming\R\data\R\checklist
