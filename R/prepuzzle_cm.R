#' Converting cm SDTM into covariates input for the puzzle function
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
#' @param abbreviated Does your CM file have an abbreviated format? 
#' @return a dataframe
#' @export
#' @examples
#'
#'  cm = as.data.frame(prepuzzle_cm(df = df_cm1))

prepuzzle_cm = function(directory=NULL,
                        xpt=FALSE,
                        sas7bdat=FALSE,
                        csv=FALSE,
                        df,
                        lower_case=FALSE,
                        include_time=FALSE,
                        abbreviated=FALSE){
  
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
  "%!in%" <- function(x, y) !(x %in% y)
  required = c("usubjid", "cmtrt", "cmdose")
  if (required %!in% names(df) & lower_case == F) {
    stop("Have you forgotten to set lower_case = T?")
  }
  if (lower_case) {
    names(df) = tolower(names(df))
  }
  if (required %!in% names(df)) {
    stop("You need to provide at least the following items: usubjid, cmtrt and cmdose")
  } 
  
  if(abbreviated==FALSE & include_time){
    df$ID = df$usubjid
    df$DATETIME = df$cmstdtc 
    df$VARIABLE = df$cmtrt
    df$VALUE = df$cmdose
    df_id = dplyr::select(df,ID,DATETIME,VARIABLE,VALUE)
  }
  
  if(abbreviated==FALSE & include_time==FALSE){
    df$ID = df$usubjid
    df$VARIABLE = df$cmtrt
    df$VALUE = df$cmdose
    df_id = dplyr::select(df,ID,VARIABLE,VALUE)
  }
  
  if(abbreviated & include_time){
    stop("Abbreviated cannot include time. Please set abbreviated ot include_time to FALSE")
  }
  
  if(abbreviated & include_time==FALSE){
    df$ID = df$usubjid
    df$VARIABLE = df$cmtrt
    df$VALUE = df$cmoccur
    df_id = dplyr::select(df,ID,VARIABLE,VALUE)
  }
  
  df = df_id
  return(df)
}

#abbreviated==T implies there is not time information. Sponsors often are interested in whether subjects are exposed to specific concomitant medications, and collect this information using a checklist. 
#The important information is if the subject takes the co-medication or not.
#This version only takes into account if the co-medication has been administrated or not. It is not smart enough to compute the times of administration
