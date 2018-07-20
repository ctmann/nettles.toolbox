#' Move to New Column
#' 
#' @description Use regex to identify values and move them to a new column, replacing original with NA 
#' @details Wrapper for str_detect
#' 
#' 
#' @param df dataframe 
#' @param  my.old.col.name Column from which value will be moved leaving behind NA
#' @param my.new.col.name Name of new column that will contain values. 
#' @param my.regex Regular expression
#' 
#' 
#' @usage  move_to_newcol(df, col1, col2, my.regex)
#' 
#' @examples  
#' x <- tibble(col1 = letters[1:5])
#' move_to_newcol(x, col1, col2, "[a]")
#' 
#' 
#' @export
 move_to_newcol <- function(my.dataframe, my.old.col.name, my.new.col.name, my.regex){
    my.old.col.name <- rlang::ensym(my.old.col.name)
    my.new.col.name <- rlang::ensym(my.new.col.name)
  x <- x %>%  
    mutate(!!my.new.col.name := case_when(
                      str_detect(!!my.old.col.name, my.regex) ~ !!my.old.col.name),
           !!my.old.col.name := case_when(
                      !!my.old.col.name == !!my.new.col.name ~ NA_character_,
                      TRUE ~ !!my.old.col.name))
  return(x)
  }