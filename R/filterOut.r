#' Filter routine
#'
#' @param data 
#' @param filter 
#'
#' @return datasett
#' @export
#' 
filterOut <- function(datasett, filter1, filter2, filter3){
  
  if (!is.null(filter1)){
    datasett <- try(dplyr::filter(datasett, level1 == filter1))
  }
  if (!is.null(filter2)){
    datasett <- try(dplyr::filter(datasett, level2 == filter2))
  }
  if (!is.null(filter3)){
    datasett <- try(dplyr::filter(datasett, level3 == filter3))
  }

  return(datasett)
}