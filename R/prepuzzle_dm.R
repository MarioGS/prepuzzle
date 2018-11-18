#' Converting dm SDTM into covariates input for the puzzle function
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
#' @param covariates vector with the name of the covariates to be included in the dataset 
#' @return a dataframe
#' @export
#' @examples
#'
#'  dm = as.data.frame(prepuzzle_dm(df = df_dm1, covariates = c("age","sex","race")))


prepuzzle_dm = function(directory=NULL,
                        xpt=FALSE,
                        sas7bdat=FALSE,
                        csv=FALSE,
                        df,
                        lower_case = F,
                        include_time=F,
                        covariates = NULL){
  
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
  required = c("usubjid", "rfstdtc")
  if (required %!in% names(df) & lower_case == F) {
    stop("Have you forgotten to set lower_case = T?")
  }
  if (lower_case) {
    names(df) = tolower(names(df))
  }
  if (required %!in% names(df)) {
    stop("You need to provide at least the following items: usubjid and rfstdtc")
  }
  
  df$ID = df$usubjid
  df$DATETIME = df$rfstdtc
  
  df_id = dplyr::select(df,ID,DATETIME)
  
  if(include_time==FALSE){
    df_id = dplyr::select(df,ID)
  }
  
  #Select covariates
  if(!is.null(covariates)){
    df_covariates = dplyr::select(df,covariates)
    df = cbind(df_id,df_covariates)
  }
  
  #Proper format for puzzle()
  if(include_time){
    df = reshape2::melt(df,id.vars=c("ID","DATETIME"))
    names(df) = c("ID","DATETIME","VARIABLE","VALUE")
    df$VARIABLE = toupper(df$VARIABLE)
  }  
  
  if(include_time==FALSE){
    df = reshape2::melt(df,id.vars=c("ID"))
    names(df) = c("ID","VARIABLE","VALUE")
    df$VARIABLE = toupper(df$VARIABLE)
  }
  return(df)
}
