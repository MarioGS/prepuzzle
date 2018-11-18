#' Converting dm and vs into covariates properly formated for the puzzle function
#'
#' @authors Mario Gonzalez Sales
#'
#' @param ae dataframe with the adverse event information
#' @param cm dataframe with the concomitant information
#' @param dm dataframe with the demographic information
#' @param dv dataframe with the protocol deviation information 
#' @param eg dataframe with the electrocardiogram information 
#' @param is dataframe with the inmunogenicity specimen information 
#' @param lb dataframe with the laboratory information 
#' @param ms dataframe with the microbiology specimen information 
#' @param mb dataframe with the microbiology susceptibility information 
#' @param ss dataframe with the subject status information 
#' @param su dataframe with the substance use information 
#' @param tr dataframe with the tumor results information 
#' @param vs dataframe with the vital sign information 
#' @return a dataframe
#' @export
#' @examples
#'
#'  cov = prepuzzle_cov(dm=dm,
#'                      vs=vs)

prepuzzle_cov = function(ae=NULL,
                         cm=NULL,
                         dm=NULL,
                         dv=NULL,
                         eg=NULL,
                         is=NULL,
                         lb=NULL,
                         mb=NULL,
                         ms=NULL,
                         ss=NULL,
                         su=NULL,
                         tr=NULL,
                         vs=NULL){
  
  cov = list(ae=ae,
             cm=cm,
             dm=dm,
             dv=dv,
             eg=eg,
             is=is,
             lb=lb,
             mb=mb,
             ms=ms,
             su=su,
             ss=ss,
             tr=tr,
             vs=vs)
  
  if(all(ncol(cov) == ncol(cov[[1]]))==FALSE){
    stop("All files have to have the same length. If you have used prepuzzle functions, please make sure include_time argument has been consistently used")
  }
  
  cov = purrr::compact(cov)
  
  df = as.data.frame(data.table::rbindlist(cov))
  
  if(ncol(df==3)){
    df = dplyr::arrange(df,ID)
  }
  
  if(ncol(df==4)){
    df = dplyr::arrange(df,ID,DATETIME)
  }
  
  # if(is.null(dm) & is.null(vs)){
  #   stop("Please define at least the dm or the vs arguments")
  # }
  # 
  # if(!is.null(dm) & !is.null(vs)){
  #   if(length(dm)!=length(vs)){
  #     stop("dm and vs has to have the same length. If you have used prepuzzle_dm and prepuzzle_vs, please make sure time_dependent_cov argument has been consistently used")
  #   }
  # }
  # 
  # 
  # if(!is.null(dm)){
  #   dm = dm
  #   if(lower_case){
  #     names(dm) = tolower(names(dm))
  #   }
  # }
  # 
  # if(!is.null(vs)){
  #   vs = vs
  #   if(lower_case){
  #     names(vs) = tolower(names(vs))
  #   }
  # }
  # 
  # if(!is.null(dm) & !is.null(vs)){
  #   df = rbind(dm,vs)
  #   df = arrange(df,ID,DATETIME)
  # }
  # 
  # if(!is.null(dm) & is.null(vs)){
  #   df = dm
  #   df = arrange(df,ID,DATETIME)
  # }
  # 
  # if(is.null(dm) & !is.null(vs)){
  #   df = vs
  #   df = arrange(df,ID,DATETIME)
  # }
  
  return(df)
}
