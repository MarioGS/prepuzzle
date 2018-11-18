#' Converting ex SDTM into dose input for the puzzle function
#'
#' @authors Mario Gonzalez Sales
#'
#' @param directory path to your external file
#' @param xpt Has your file a .xpt extension?
#' @param sas7bdat Has your file a .sas7bdat extension?
#' @param csv Has your file a .csv extension?
#' @param df R object type dataframe
#' @param lower_case if TRUE convert the names of df from upper to lower case
#' @return a dataframe
#' @export
#' @examples
#'
#'  dose = as.data.frame(prepuzzle_dose(df = EX, lower_case = T))


prepuzzle_dose = function(directory=NULL,
                          xpt=FALSE,
                          sas7bdat=FALSE,
                          csv=FALSE,
                          df=df,
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
  required = c("usubjid", "studyid", "visitnum", "visit", "exstdtc", "extrt")
  if (required %!in% names(df) & lower_case == F) {
    stop("Have you forgotten to set lower_case = T?")
  }
  if (lower_case) {
    names(df) = tolower(names(df))
  }
  if (required %!in% names(df)) {
    stop("You need to provide at least the following items: usubjid, studyid, visitnum, visit, exstdtc and extrt")
  }
  
  df$ID = df$usubjid
  df$STUDY = df$studyid
  df$VISIT = df$visitnum
  df$PERIOD = df$visit
  df$DATETIME = df$exstdtc
  df$TRT = df$extrt
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  #'%ni%' <- Negate('%in%') alternative
  
  if("exdose" %in% names(df)){
    df$AMT = df$exdose
  }
  
  if("exdostot" %in% names(df) & "exdose" %!in% names(df)){
    df$AMT = df$exdostot
  }
  
  if("exroute" %in% names(df)){
    df$ROUTE = df$exroute
  }
  
  if("exfast" %in% names(df)){
    df$FAST = df$exfast
  }
  
  if("exfast" %!in% names(df) & "exroute" %!in% names(df)){
  df = dplyr::select(df,ID,STUDY,DATETIME,PERIOD,AMT,VISIT,TRT)
  }
  
  if("exfast" %in% names(df) & "exroute" %!in% names(df)){
    df = dplyr::select(df,ID,STUDY,DATETIME,PERIOD,AMT,VISIT,TRT,FAST)
  }
  
  if("exfast" %!in% names(df) & "exroute" %in% names(df)){
    df = dplyr::select(df,ID,STUDY,DATETIME,PERIOD,AMT,VISIT,TRT,ROUTE)
  }

  if("exfast" %in% names(df) & "exroute" %in% names(df)){
    df = dplyr::select(df,ID,STUDY,DATETIME,PERIOD,AMT,VISIT,TRT,FAST,ROUTE)
  }
  
    df = dplyr::mutate_all(df, as.character)
  return(df)
}

#Does not take in to account EXENDTC yet