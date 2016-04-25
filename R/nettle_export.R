#' nettle_export
#'
#' This saves a timestamped file to declared subfolder using Hadley's write_csv function.
#' Intended for use with dataframes. Saves to either Data/Raw or Data/Processed (default) subfolders,
#'
#' @param df Choose dataframe to export
#' @param my.filename Desired filename
#' @param my.data.subfolder Data/Raw' or Data/Processed' (default) subfolder
#' @usage nettle_export(df, my.filename, my.data.subfolder = "Processed")
#' 
#' 
#' @export
nettle_export <- function(df, my.filename, my.data.subfolder = "Processed", ...){
  df <- dplyr::tbl_df(df)
  my.data.folder.location <- paste0(getwd(), "/Data")
  my.data.subfolder <- "Processed"
  my.filename <- my.filename
  my.timestamp <- paste('Updated', format(Sys.time(), format = ".%Y-%m-%d.%H%M") , sep = "")
  export.this <- sprintf("%s/%s/%s_%s.csv", my.data.folder.location, my.data.subfolder, my.filename, my.timestamp)
  return(readr::write_csv(df, export.this))
}



