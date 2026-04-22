
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AlphaPalettes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AlphaPalettes <- list(Levels_2 <- c(0.5, 0.9),
                      Levels_3 <- c(0.2, 0.5, 0.9),
                      Levels_4 <- c(0.3, 0.5, 0.7, 0.9),
                      Levels_5 <- c(0.4, 0.5, 0.6, 0.7, 0.8))

# Save data in .rda-file and make it part of package
use_data(AlphaPalettes, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Colors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Colors <- list(LightGrey = "#EDEDED",
               MediumGrey = "#D0D0D0",
               DarkGrey = "#595959",
               #---------
               Primary = "#054996",
               PrimaryLight = "#05499650",
               Secondary = "#8e1e39",
               SecondaryLight = "#8e1e3950",
               Tertiary = "#2B8C88",
               TertiaryLight = "#2B8C8850",
               #---------
               Accent = "#960551",
               AccentLight = "#96055150",
               #---------
               BlueNice = "#7EA6E0")

# Save data in .rda-file and make it part of package
use_data(Colors, overwrite = TRUE)


