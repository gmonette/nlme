.onAttach <- function(libname,pkgname) {
  packageStartupMessage('This is a modified version of nlme that includes a pdInd class. See ?pdInd')
  packageStartupMessage('It can be installed with: remotes::install_github(\'gmonette/nlme\')')
  packageStartupMessage('To suppress this message load nlme with \'suppressPackageStartupMessages(library(nlme))\'')
}
  
  
