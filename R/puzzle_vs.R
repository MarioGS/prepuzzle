#' Converting vs SDTM into covariates input for the puzzle function
#'
#' @authors Mario Gonzalez Sales
#'
#' @param directory path to your external file
#' @param xpt Has your file a .xpt extension?
#' @param sas7bdat Has your file a .sas7bdat extension?
#' @param csv Has your file a .csv extension?
#' @param df R object type dataframe
#' @param lower_case if TRUE convert the names of df from upper to lower case
#' @param time_dependent_cov Do you have time dependent covariates to be included in the dataset? 
#' @return a dataframe
#' @export
#' @examples
#'
#'  vs = as.data.frame(puzzle_vs(df = df_vs1,time_dependent_cov = F))

puzzle_vs = function(directory=NULL,
                     xpt=FALSE,
                     sas7bdat=FALSE,
                     csv=FALSE,
                     df,
                     lower_case = F,
                     time_dependent_cov=F){
  
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
  
  
  if(!is.null(directory) & is.null(df)){
    stop("You do not need to define the arguments directory and df at the same time! Please use one of them and set the other to NULL")
  }
  
  if(!is.null(directory) & xpt & is.null(df)){
    df = Hmisc::sasxport.get(directory)
  }
  
  if(!is.null(directory) & sas7bdat & is.null(df)){
    df = sas7bdat::read.sas7bdat(directory)
  }
  
  if(!is.null(directory) & csv & is.null(df)){
    df = readr::read_csv(directory)
  }
  
  if(is.null(directory) & !is.null(df)){
    df = df
  }
  
  df = df
  if(lower_case){
    names(df) = tolower(names(df))
  }
  
  df$ID = df$usubjid
  df$DATETIME = df$vsdtc
  df$VARIABLE = df$vstestcd
  df$VALUE = df$vsstresn
  
  df_id = dplyr::select(df,ID,DATETIME,VARIABLE,VALUE)
  
  if(time_dependent_cov==FALSE){
    df_id = dplyr::select(df,ID,VARIABLE,VALUE)
  }
  
  df = df_id
  return(df)
}
