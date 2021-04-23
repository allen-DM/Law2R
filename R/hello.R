# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

checkLibrary <- function() {
  libraryList=c("DBI","RMariaDB","jsonlite","httr","config","quantmod","dplyr","stringr","jiebaR")
  for(i in 1:length(libraryList[i])){
    if(require(libraryList[i])==FALSE){
      install.packages(libraryList[i],dependencies=T)
    }
  }
}
