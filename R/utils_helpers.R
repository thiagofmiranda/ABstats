#' helpers
#'
#' @description Set Custom Theme
#'
#' @import fresh
#' @return The Custom Theme.
#'
#' @noRd
setTheme <- function() {
  theme <- create_theme(
    adminlte_color(
      green = "#ff548c",        # success status
      blue = "#4c446c",         #
      aqua = "#4c446c",         # info status
      red = "#ff548c",          # danger status
      yellow = "#f1c40f",       # warning status
      fuchsia = "#ff79c6",      #
      navy = "#3b3a5a",         #
      purple = "#7f6db0",       #
      maroon = "#b0447e",       #
      light_blue = "#4c446c"    # primary status
    ),
    adminlte_sidebar(
      width = "250px",
      dark_bg = "#4c446c",                 # Fundo da sidebar
      dark_hover_bg = "#ff548c",           # Hover no item do menu
      dark_color = "#ffffff",              # Texto normal
      dark_hover_color = "#4c446c"        # Texto ao passar o mouse
    ),
    adminlte_global(
      content_bg = "#ffffff",     # Fundo da área principal
      box_bg = "#f5f5fa",         # Fundo das boxes
      info_box_bg = "#fce4ec"     # Fundo dos infoBoxes
    )
  )

  return(theme)
}

#' helpers
#'
#' @description Calculate Sample Size
#'
#' @import pwr
#' @return The Sample Size.
#'
#' @noRd
calculate_sample_size <- function(baseline_rate, expected_rate, power, alpha, hypothesis_type, n_treatments=2){
  result <- pwr.2p.test(h = ES.h(baseline_rate, expected_rate),
                        sig.level = alpha,
                        power = power,
                        alternative  = hypothesis_type)
  return(result$n * n_treatments)
}

#' helpers
#'
#' @description Calculate Sample Size
#'
#' @import pwr
#' @return The Sample Size.
#'
#' @noRd
calculate_test_power <- function(baseline_sample_size, treatment_sample_size, baseline_rate, treatment_rate, alpha, hypothesis_type){
  result <- pwr.2p2n.test(
    n1 = baseline_sample_size,
    n2 = treatment_sample_size,
    h = ES.h(baseline_rate, treatment_rate),
    sig.level = alpha,
    alternative  = hypothesis_type
  )
  return(result$power)
}
