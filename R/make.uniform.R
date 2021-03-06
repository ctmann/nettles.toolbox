#' Uniformly format text
#'
#' @description Make syntactcially valid names out of character vectors
#' 
#' @details Useful for stripping out whitespace and formatting text according to pre- established R rules.
#' Converts characters to R-friendly, all caps. The character 'X' is prepended for
#' data that appears to be numeric. Invalid characters are translated to '.' Missing values
#' become 'NA.' For more details, refer to make.names function{base}
#' 
#' @param my.data List of data. Could be column, names(data), etc.
#' @usage nettle_uniform(df$my.data)
#' 

#' @export
nettle_uniform <- function(my.data){
  x <- dplyr::my.data %>% 
    stringr::str_to_upper() %>% 
    stringr::str_trim() %>% 
    make.names()
  my.data <- x
  return(my.data)
}  



