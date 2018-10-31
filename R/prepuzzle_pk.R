#' Converting pc SDTM into pk input for the puzzle function
#'
#' @authors Mario Gonzalez Sales
#'
#' @param directory path to your external file
#' @param xpt Has your file a .xpt extension?
#' @param sas7bdat Has your file a .sas7bdat extension?
#' @param csv Has your file a .csv extension?
#' @param df R object type dataframe
#' @param lower_case if TRUE convert the names of df from upper to lower case
#' @param only_observations if TRUE only observations will be retained in the dataframe 
#' @return a dataframe
#' @export
#' @examples
#'
#'  pk = as.data.frame(prepuzzle_pk(df = PC, only_observations = T, lower_case = T))


prepuzzle_pk = function(directory=NULL,
                        xpt=FALSE,
                        sas7bdat=FALSE,
                        csv=FALSE,
                        df=NULL,
                        lower_case=F,
                        only_observations = F){
  
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
  
  if(!is.null(directory) & !is.null(df)){
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
  df$DV = df$pcstresn
  df$DV = ifelse(is.na(df$DV),0,df$DV)
  df$LLOQ = df$pclloq
  df$DATETIME = df$pcdtc
  df$BLQ = ifelse(df$DV<df$LLOQ,1,0)
  
  #Remove non-observations  
  if(only_observations){
    df = dplyr::filter(df,pcstat=="")
  }
  
  #df = select(df,ID,STUDY,DATETIME,NOMINALTAD,PERIOD,DV,VISIT,BLQ)
  df = dplyr::select(df,ID,DATETIME,DV,BLQ)
  df = dplyr::mutate_all(df,as.character)
  
  return(df)
}
