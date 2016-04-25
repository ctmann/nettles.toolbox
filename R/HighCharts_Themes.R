#' hc_theme_crs
#'
#' This applies various crs color and font palette themes
#' 
#' @param  Currently, there is only one crs theme for use with highcharter package, 'crs.1'.
#' @return highcharter font and color theme
#' @usage   hc_theme_crs("crs.1")

# CRS Color Palette  ------------------------------------------------------------------

# CRS.1 Theme -------------------------------------------------------------

# Colors

color <- c('navy.blue', 'gold', 'dark.gray', 'light.gray', 'orange', 'dark.green', 'light.green','red')
hex <- c('#336A91', '#F3C117', '#7F7F7F', '#9997AD', '#DB5926','#B4B392', '#C8C7B1', '#D1282E')

# Simulate Python Dictionary with a list
crs_palette <- as.list(hex)
names(crs_palette) <- color

crs.1 <- highcharter::hc_theme(
          # Global Palette
  #       colors = c('#336A91', '#F3C117', '#7F7F7F', '#9997AD', '#DB5926','#B4B392', '#C8C7B1', '#D1282E'),
          colors = c(crs_palette$navy.blue, crs_palette$gold, crs_palette$dark.gray, crs_palette$light.gray, crs_palette$orange, crs_palette$dark.green, crs_palette$light.green, crs_palette$red),
          chart = list(backgroundColor = "white"),
          # Title Palette
          title = list(
            style = list(
              color = '#000',
              font = c('16px "Calibri", Verdana, sans-serif')
            ) # End Title/Style
          ), # End Title

          # Subtitle
          subtitle = list(
            style = list(
              color = '#666666',
              font = c('12px "Calibri", Verdana, sans-serif')
            )# End Subtitle/Style
          ), # End Subtitle

          # Legend
          legend = list(
            itemStyle = list(
              color = 'black',
              font = c('9pt Trebuchet MS, Verdana, sans-serif')
            ), # End Legend/itemStyle
            itemHoverStyle = list(
              color = 'gray'
            )# End Legend/itemHoverStyle
          ),# End Legend

          # Credits
          credits = list(
            position = list(
              align = 'left',
              x = 10
            )#End Credits/Position
          )# End Credits
        ) # Final Theme parenthesis!



# Function -----------------------------------------------------------------
#' @export
hc_theme_crs <- function(df, theme.name) {
  switch(theme.name,
         "crs.1" = hc_add_theme(df,crs.1),
         "I see nothing here")

}







