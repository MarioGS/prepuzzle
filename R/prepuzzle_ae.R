#' Converting ae SDTM into covariates input for the puzzle function
#'
#' @authors Mario Gonzalez Sales
#'
#' @param directory path to your external file
#' @param xpt Has your file a .xpt extension?
#' @param sas7bdat Has your file a .sas7bdat extension?
#' @param csv Has your file a .csv extension?
#' @param df R object type dataframe
#' @param lower_case if TRUE convert the names of df from upper to lower case
#' @param include_time Would yu like to include time in the dataset? 
#' @return a dataframe
#' @export
#' @examples
#'
#'  ae = as.data.frame(prepuzzle_su(df = df_ae1))

prepuzzle_ae = function(directory=NULL,
                        xpt=FALSE,
                        sas7bdat=FALSE,
                        csv=FALSE,
                        df,
                        lower_case = F,
                        include_time=F){
  
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
  
  options(warn = -1)
  
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
  "%!in%" <- function(x, y) !(x %in% y)
  required = c("usubjid", "aedecod", "aestdtc")
  if (required %!in% names(df) & lower_case == F) {
    stop("Have you forgotten to set lower_case = T?")
  }
  if (lower_case) {
    names(df) = tolower(names(df))
  }
  if (required %!in% names(df)) {
    stop("You need to provide at least the following items: usubjid, aedecod, aestdtc")
  }
  
  df$ID = df$usubjid
  df$DATETIME = df$aestdtc
  df$VARIABLE = df$aedecod
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  #'%ni%' <- Negate('%in%') alternative
  
  if("aesev" %in% names(df)){
    df$VALUE = df$aesev
  }
  
  if("aetoxgr" %in% names(df) & "aesev" %!in% names(df)){
    df$VALUE = df$aetoxgr
  }
  
  if("aetoxgr" %!in% names(df) & "aesev" %!in% names(df)){
    stop("AESEV or AETOXGR must be in defined in AE dataset")
  }
  
  #Proper format for puzzle()
  if(include_time){
    dplyr::select(df,ID,DATETIME,VARIABLE,VALUE)
  }  
  
  if(include_time==FALSE){
    dplyr::select(df,ID,VARIABLE,VALUE)
  }
  return(df)
}

