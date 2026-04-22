
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TestProcess <- callr::r_bg(function()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  TestData <- readRDS("../dsCCPhos/Development/Data/TestData/CCPTestData.rds")

  # Load namespace of CCPhosApp for new background R session
  library(CCPhosApp)

  # Source all files in CCPhos package (if library() does not work for some reason)
  #sapply(paste0("./R/", list.files("./R")), source)

  # Start CCPhos app
  shiny::runApp(StartCCPhosApp(CCPTestData = TestData),
                port = 49154,
                launch.browser = FALSE)

}, supervise = TRUE)


# Check if URL is responding and stall if it needs time loading (Credit to Will Landau)
while(!pingr::is_up(destination = "127.0.0.1", port = 49154))
{
  if (!TestProcess$is_alive()) stop(TestProcess$read_all_error())   # If process was unalived for some reason, print error messages.
  Sys.sleep(0.01)   # Stall - Effectively check every 0.01 seconds if URL is up yet
}


# Now manually open browser
browseURL("http://127.0.0.1:49154")

library(rstudioapi)
viewer(url = "http://127.0.0.1:49154")

TestProcess$is_alive()
TestProcess$read_error()
TestProcess$kill()

