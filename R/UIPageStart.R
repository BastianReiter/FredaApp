
#' UIPageStart
#'
#' @noRd
UIPageStart <- function()
{
  div(h4(class = "ui dividing header",
         "Connect to CCP sites"),

      ModLogin_UI("Login"))
}
