#' nettle_prettyNum
#' 
#' A function that formats big (and small) numbers 
#' 
#' Numbers that have 12 digits are abreviated "T" for trillion
#' Numbers that have 9 digits are abreviated "B" for billion
#' Numbers that have 6 digits are abreviated "M" for Million
#' Commas are inserted. Some warning messages remain. Not sure why.
#'
#'
#' @param num A big or small number
#' @usage nettle_prettyNum(c(1000, -1000, 1000000, -1000000, 1000000000, -1000000000, 1000000000000, -1000000000000))
#'
#' @export
nettle_prettyNum <- function(num){
  t.index <- (num >= 1e12) | (num <= -1e12)
  b.index <- (num >= 1e9 & num < 1e12)| (num <= -1e9 & num > -1e12)
  m.index <- (num >= 1e5 & num < 1e9) | (num <= -1e5 & num > -1e9)
  k.index <- (num >= 1e3 & num < 1e5) |(num <= -1e3 & num > -1e5)
  output = suppressWarnings(formatC(num, format = "d", big.mark = ","))
  
  output[t.index] = paste0(formatC(num[t.index] / 1e12, digits = 1,format = "f"), "T")
  output[b.index] = paste0(formatC(num[b.index] / 1e9, digits = 1, format = "f"), "B")
  output[m.index] = paste0(formatC(num[m.index] / 1e6, digits = 1, format = "f"), "M")
  output[k.index] = paste0(formatC(num[k.index] / 1e3, digits = 1, format = "f"), "K")
  return(output)
}
  

