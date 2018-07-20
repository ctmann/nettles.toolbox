#' Download and Unzip
#'
#' Download and Unzip file to temporary directory
#'
#' This function will download and unzip file to temporary location, then
#' read_excl.
#' 
#' 
#' @param zip.url zip url
#' @param zip.file name of file inside zip that must be read
#' @param sheet read_excel
#' @return location of unzipped file
#' 
#'  @examples x <- "http://comptroller.defense.gov/Portals/45/Documents/defbudget/fy2018/FY_2018_Green_Book.zip"
#'           y <- "FY18 PB Green Book Chap 2.xlsx" 
#'           my.path <- nettle_downzip(x, y)
#'           data <- read_excel(my.path, sheet = 1)
#' 
#' @export 
#' 
nettle_downzip <- function(zip.url, zip.file){
    my.temporary.zipped.file <- tempfile()   # Zip file will go in here
    my.temporarary.zipped.folder <- tempdir() # Unzipped file will go in here
    download.file(zip.url, dest = my.temporary.zipped.file) # Download Source Data to Temp file
    unzip(my.temporary.zipped.file, exdir = my.temporarary.zipped.folder) # Unzip to Temp directory
    location.of.unzipped.file <- paste0(my.temporarary.zipped.folder,"/", zip.file)
    return(location.of.unzipped.file )
    }
