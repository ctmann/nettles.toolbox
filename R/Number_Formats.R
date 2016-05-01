#' nettle_big.numbers
#' 
#' A function that abreviates big numbers and adds K (thousands), M (Millions), B (Billions), T (Trillions) 
#' 
#' @param num number to evaluate
#' @examples big.numbers <- c(100000000, 98875, 86455, 2999999999)
#' nettle_big.numbers(big.numbers)
#' 
#'
#'
#'
#' @export
nettle_big.numbers <- function(num) {
	# Formats a number with more than 6 digits into an abbreviation string
	#
	# Args:
	#   num: A number of at least 6 digits
	#
	# Returns:
	#   A string of the number abbreviated to "M", "B", or "T"
	t.index = num >= 1e12
	b.index = num >= 1e9 & num < 1e12
	m.index = num >= 1e5 & num < 1e9
	k.index = num >= 1e5 & num < 1e3
	output = formatC(num, format = "d", big.mark = ",")

	output[t.index] = paste0(formatC(num[t.index] / 1e12, digits = 1,format = "f"), "T")
	output[b.index] = paste0(formatC(num[b.index] / 1e9, digits = 1, format = "f"), "B")
	output[m.index] = paste0(formatC(num[m.index] / 1e6, digits = 1, format = "f"), "M")
	output[k.index] = paste0(formatC(num[k.index] / 1e3, digits = 1, format = "f"), "K")
	return(output)
}





