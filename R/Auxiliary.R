
#===============================================================================
# AUXILIARY FUNCTIONS within CCPhosApp package
#===============================================================================



#===============================================================================
#' ColorToRGBCSS
#'
#' Turn hexadecimal color code into a string of CSS code of the form 'rgba(r, g, b, a)'.
#' Can be used with vectors.
#'
#' @param Color \code{character} - Vector of hexadecimal color code
#' @param Alpha \code{double} - Optional alpha value vector - Default: 1
#' @param RenderNATransparent \code{logical} - Indicating whether \code{NA} values for \code{Color} result in a totally transparent color
#'
#' @author Bastian Reiter
#-------------------------------------------------------------------------------
ColorToRGBCSS <- function(Color,
                          Alpha = 1,
                          RenderNATransparent = TRUE)
{
  Scalar <- function(color, alpha)
            {
                RGB <- col2rgb(color)
                if (RenderNATransparent == TRUE & is.na(color)) { Alpha <- 0 } else { Alpha <- alpha }      # If color is NA, set alpha value 0 (making resulting color effectively non-existent)
                paste0("rgba(", RGB[["red", 1]], ", ", RGB[["green", 1]], ", ", RGB[["blue", 1]], ", ", Alpha, ")")
            }

  Vectorize(Scalar)(Color, Alpha)
}
#===============================================================================


#===============================================================================
#' ConvertLogicalToIcon
#'
#' @param DataFrame \code{data.frame} or \code{tibble}
#'
#' @return \code{data.frame}
#' @export
#' @author Bastian Reiter
#-------------------------------------------------------------------------------
ConvertLogicalToIcon <- function(DataFrame)
{
  if (!is.null(DataFrame))
  {
      DataFrame %>%
          mutate(across(.cols = where(is.logical),
                        .fns = ~ case_match(.x,
                                            TRUE ~ as.character(shiny.semantic::icon(class = "small green check")),
                                            FALSE ~ as.character(shiny.semantic::icon(class = "small red times")))))
  }
  else { return(NULL) }
}
#===============================================================================


#===============================================================================
#' CreateWaiterScreen
#'
#' @param ID \code{string}
#'
#' @return Waiter object
#' @noRd
#-------------------------------------------------------------------------------
CreateWaiterScreen <- function(ID)
{
  waiter::Waiter$new(id = ID,
                     html = waiter::spin_3(),
                     color = waiter::transparent(.5))
}
#===============================================================================



#===============================================================================
#' SafeDS
#' @noRd
#-------------------------------------------------------------------------------
SafeDS <- function(Expression)
{
  tryCatch(expr = Expression,
           error = function(e)
                   {
                      structure(list(error = TRUE,
                                     msg = conditionMessage(e)),
                                class = "dsFail")
                   })
}
#===============================================================================


#===============================================================================
#' DSError
#' @noRd
#-------------------------------------------------------------------------------
ShowDSError <- function()
{
  showNotification(ui = "Server temporarily unavailable (Bad Gateway). Please repeat the action.",
                   duration = NULL,
                   type = "error")
}
#===============================================================================
