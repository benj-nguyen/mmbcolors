#' Color Palette MN Colors - Fill
#'
#' This function uses interpolated MN colors to form a color palette for graphical purposes. The three main palettes are "main", "blue", and "green".
#' "Main" is composed of MN branded colors, using interpolated colors for gradiation, sorted in the arrangement of the color wheel.
#'
#' @param palette The name of the palette; options are "main", "blue", and "green".
#' @param discrete Boolean condition to define whether relevant aesthetic is discrete or not. Default is false.
#' @param reverse Boolean condition to reverse order of colors or not. Default is false.
#' @export

scale_fill_mncol <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  mncolors <- c(
    "minnesota_blue" = "#003865",
    "minnesota_green" = "#78BE21",
    "white" = "#FFFFFF",
    "black" = "#000000",
    "accent_teal" = "#008EAA",
    "accent_green" = "#0D5257",
    "accent_orange" = "#8D3F2B",
    "accent_purple" = "#5D295F",
    "extended_accent_blue_gray" = "#A4BCC2",
    "extended_accent_cream" = "#F5E1A4",
    "extended_accent_sky_blue" = "#9BCBEB",
    "extended_accent_gold" = "#FFC845",
    "dark_gray" = "#53565A",
    "medium_gray" = "#97999B",
    "light_gray" = "#D9D9D6",
    "red" = "#A6192E",
    "orange" = "#E57200"
  )

  # Wrapper Function to return HEX color availability of mncolors
  my_cols <- function(...) {
    cols <- c(...)
    if (is.null(cols))
      return (mncolors)
    mncolors[cols]
  }


  # Define a list of pre-defined color palettes

  my_palettes <- list(
    `main` = my_cols(c("extended_accent_cream",
                       "extended_accent_gold",
                       "minnesota_green",
                       "accent_green",
                       "minnesota_blue",
                       "accent_purple")
    ),
    `blue` = my_cols(c("white",
                       "extended_accent_sky_blue",
                       "accent_teal",
                       "minnesota_blue")
    ),
    `green` = my_cols(c("white",
                        "extended_accent_cream",
                        "minnesota_green",
                        "accent_green")
    )
  )

  # access and interpolate colors from the list of palettes
  my_pal <- function(palette = "main", reverse = FALSE, ...) {
    pal <- my_palettes[[palette]]
    if (reverse) pal <- rev(pal)
    colorRampPalette(pal, ...)
  }

  pal <- my_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

