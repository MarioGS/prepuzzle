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
  if(lower_case){
    names(df) = tolower(names(df))
  }
  
  df$ID = df$usubjid
  df$STUDY = df$studyid
  df$VISIT = df$visitnum
  df$PERIOD = df$visit
  df$DATETIME = df$exstdtc
  if("exdostot" %in% names(df)){
    df$AMT = df$exdostot
  }
  if("exdose" %in% names(df)){
    df$AMT = df$exdose
  }
  
  df = dplyr::select(df,ID,STUDY,DATETIME,PERIOD,AMT,VISIT)
  df = dplyr::mutate_all(df, as.character)
  return(df)
}
