box::use(
  checkmate[assert_string],
  shiny[div, img, h1, fluidRow],
  glue[glue]
)
#' Function to create a header for the app
#'
#' @param logo_src string with the path to the logo image
#' @param app_name string with the app name
#' @param icon_with string with the icon width
#'
#' @export
header <- function(logo_src, app_name, ..., icon_width = "70px") {
  assert_string(logo_src)
  assert_string(app_name)
  assert_string(icon_width)
  div(
    id = "app-header",
    div(
      id = "brand",
      img(src = logo_src, width = icon_width),
      h1(app_name)
    ),
    ...
  )
}

#' @export
two_columns <- function(col1, col2, widths = c(6, 6), breakpoint = "lg") {
  checkmate::assert_numeric(widths, lower = 1, upper = 12, any.missing = FALSE, len = 2)
  checkmate::assert_choice(breakpoint, c("sm", "md", "lg", "xl"))

  fluidRow(
    div(
      class = glue("col-{ breakpoint }-{ widths[1] }"),
      col1
    ),
    div(
      class = glue("col-{ breakpoint }-{ widths[2] }"),
      col2
    )
  )
}
