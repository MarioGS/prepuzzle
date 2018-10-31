#' Converting dm and vs into covariates properly formated for the puzzle function
#'
#' @authors Mario Gonzalez Sales
#'
#' @param dm dataframe with the demographic information
#' @param vs dataframe with the vital sign information 
#' @return a dataframe
#' @export
#' @examples
#'
#'  cov = prepuzzle_cov(dm=dm,
#'                   vs=vs)

prepuzzle_cov = function(dm=NULL,
                         vs=NULL,
                         lower_case = F){
  
  packages = c("magrittr","Hmisc","sas7bdat","readr")
  if (length(setdiff(packages, rownames(installed.packages()))) >
      0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  suppressPackageStartupMessages(library("tidyverse"))
  suppressPackageStartupMessages(library("magrittr"))
  suppressPackageStartupMessages(library("Hmisc"))
  suppressPackageStartupMessages(library("sas7bdat"))
  suppressPackageStartupMessages(library("readr"))
  
  
  if(is.null(dm) & is.null(vs)){
    stop("Please define at least the dm or the vs arguments")
  }
  
  if(!is.null(dm) & !is.null(vs)){
    if(length(dm)!=length(vs)){
      stop("dm and vs has to have the same length. If you have used prepuzzle_dm and prepuzzle_vs, please make sure time_dependent_cov argument has been consistently used")
    }
  }
  
  
  if(!is.null(dm)){
    dm = dm
    if(lower_case){
      names(dm) = tolower(names(dm))
    }
  }
  
  if(!is.null(vs)){
    vs = vs
    if(lower_case){
      names(vs) = tolower(names(vs))
    }
  }
  
  if(!is.null(dm) & !is.null(vs)){
    df = rbind(dm,vs)
    df = arrange(df,ID,DATETIME)
  }
  
  if(!is.null(dm) & is.null(vs)){
    df = dm
    df = arrange(df,ID,DATETIME)
  }
  
  if(is.null(dm) & !is.null(vs)){
    df = vs
    df = arrange(df,ID,DATETIME)
  }
  
  return(df)
}
