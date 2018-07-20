#' Move to Existing Column
#' 
#' @description Use regex to identify values move between columns, leaving behind NA
#' @details Wrapper for str_detect
#' 
#' 
#' @param df dataframe 
#' @param  my.replacement.col Column to which value will be moved 
#' @param my.replacer.col Name of column that contain values, leaving behind NA 
#' @param my.regex Regular expression
#' 
#' 
#' @usage  move_to_existing_col(df, my.replacement.col, my.replacer.col, my.regex)
#' 
#' @examples  
#' z <- tibble(col1 = letters[1:5], col2 = c("boo","bah", "beh", "bi", "E") )
#' move_to_newcol(z, col1, col2, "[A-Z]")
#' 
#' 
move_to_existing_col <- function(my.dataframe, my.replacement.col, my.replacer.col, my.regex){
  #Treat the literal text input provided as a dplyr column name.
    my.replacement.col <- treat_input_as_col(my.replacement.col)
    my.replacer.col <- treat_input_as_col(my.replacer.col)
   
     x <- my.dataframe %>% 
      mutate(!!my.replacement.col := case_when(
                                    str_detect(!!my.replacer.col, my.regex) ~ !!my.replacer.col,
                                    TRUE ~ !!my.replacement.col),
             !!my.replacer.col := case_when(
                      !!my.replacement.col == !!my.replacer.col ~ NA_character_,
                      TRUE ~ !!my.replacer.col))
     return(x)
      }
