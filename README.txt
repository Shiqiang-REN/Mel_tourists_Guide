1. Please make sure the packages listed below have installed in your devices and you can access internet!
    library(dplyr)
    library(ggplot2)
    library(leaflet)
    library(shiny)
    library(shinyjs)
    library(sf)
    library(jsonlite)
    library(tidyr)
    library(httr)

 You can run the script below to install if you have not

libraries <- c('shiny','shinyjs','sf','jsonlite','dplyr','leaflet','ggplot2','tidyr', 'httr')new_libraries <- libraries[!(libraries %in% installed.packages()[,"Package"])]if(length(new_libraries)) install.packages(new_libraries)

2. Please make sure the R studio have installed in your device
   the download link : https://posit.co/downloads/

3. Now pen the project folder by using R studio

4. Click the Run App button on the right side!

5. You can explore the application now.