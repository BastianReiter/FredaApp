
#' UIPageStart
#'
#' @noRd
UIPageStart <- function()
{
  div(h4(class = "ui dividing header",
         "Connect to CCP sites"),

      Mod.Login.UI("Login"))
}
