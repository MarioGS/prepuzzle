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
#' @param formulary_format Does your CM file have a formulary format? 
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
                        lower_case = F,
                        formulary_format=F){
  
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
  
  if(formulation_format==FALSE){
    df$ID = df$usubjid
#    df$DATETIME = df$cmstdtc 
    df$VARIABLE = df$cmtrt
    df$VALUE = df$cmdose
  #  df_id = dplyr::select(df,ID,DATETIME,VARIABLE,VALUE)
    df_id = dplyr::select(df,ID,VARIABLE,VALUE)
  }
  
  if(formulation_format==TRUE){
    df$ID = df$usubjid
    df$VARIABLE = df$cmtrt
    df$VALUE = df$cmoccur
    df_id = dplyr::select(df,ID,VARIABLE,VALUE)
  }
  
  df = df_id
  return(df)
}

#Formulation_format==T implies there is not time information. Sponsors often are interested in whether subjects are exposed to specific concomitant medications, and collect this information using a checklist. 
#The important information is here if the subject takes the co-medication or not.
#This version only takes into account if the co-medication has been administrated or not. It is not smart enough to compute the times of administration
