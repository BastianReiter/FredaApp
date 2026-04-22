

#===============================================================================
#
# CCPhosApp DEVELOPMENT TRACKER
#
#===============================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages used for development
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(devtools)
#library(shiny.info)
#library(sass)


# Compiling of .css-file from SASS-file using package 'sass'
# sass(input = sass_file("./Development/CCPhosStyle.scss"),
#      options = sass_options(output_style = "compressed"),      # Outputs the .css-file in compressed form
#      output = "./inst/app/www/styles/CCPhosStyle.min.css")



# Set preferred license in description
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_ccby_license()


# Define part of project that should not be distributed in the package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_build_ignore("Development")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding package dependencies using usethis::use_package()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use_package("assertthat")
# use_package("callr")
# use_package("DataEditR")
# use_dev_package("dsCCPhosClient", remote = "BastianReiter/dsCCPhosClient")
# use_dev_package("dsFredaClient", remote = "BastianReiter/dsFredaClient")
# use_package("DT")
# use_package("gt")
# use_package("httpuv")
# use_package("pingr")
# use_package("plotly")
# use_package("shiny")
# use_package("shinyjs")
# use_package("shiny.router")
# use_package("shiny.semantic")
# use_package("shiny.worker")
# use_package("stringr")
# use_package("sysfonts")
# use_package("tidyr")
# use_package("waiter")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package documentation and import settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set up central roxygen "script"
#-------------------------------------------------------------------------------
use_package_doc()

# Use the %>%-operator in this package (not enough to import dplyr)
#-------------------------------------------------------------------------------
use_pipe(export = FALSE)

# Use specific functions from external packages
#-------------------------------------------------------------------------------
use_import_from("dsFredaClient", c("AddCumulativeRow",
                                   "CheckDSConnections",
                                   "DisplayTimeSeries",
                                   "ds.ExtractFromList",
                                   "ds.FilterTable",
                                   "ds.GetCrossTab",
                                   "ds.GetCurationReport",
                                   "ds.GetDataSetCheck",
                                   "ds.GetFeatureInfo",
                                   "ds.GetFrequencyTable",
                                   "ds.GetObjectMetaData",
                                   "ds.GetObjectStatus",
                                   "ds.GetSampleStatistics",
                                   "ds.GetTableCheck",
                                   "ds.GetTTEModel",
                                   "ds.JoinTables",
                                   "ds.MakeList",
                                   "ds.MutateTable",
                                   "ds.PrepareRawData",
                                   "ExploreFeature",
                                   "ExportPlot",
                                   "GetEligibleValues",
                                   "GetExplorationData",
                                   "GetServerOpalDBInfo",
                                   "GetServerWorkspaceInfo",
                                   "ggTheme",
                                   "gtTheme",
                                   "MakeBoxPlot",
                                   "MakeColumnPlot",
                                   "MakeFunctionMessage"))

