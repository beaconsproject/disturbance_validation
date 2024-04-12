## Disturbance Validation

April 12, 2024

The purpose of the Disturbance Validation app is to enable users to interactively examine linear and areal surface disturbance features along with several satellite imagery sources. This permits alternative digitizers or other users to quickly look at the digitized features and their assigned attributes, and visually compare them to more than one high resolution imagery source. Three satellite images are available for viewing: Esri WorldImagery, Google Imagery, and SPOT Imagery for circa 2021. Currently, the app can only be used using the demo dataset for four partial or full watersheds in the southeast Yukon. A future release will enable users to upload their own linear and areal features and will include instructions on how to do so.

The app is located at: https://beaconsproject.shinyapps.io/disturbance_validation

The app can also be run from a local machine using the following steps:

  1. Install R (download from r-project.org and follow instructions)
  2. Install the following additional packages:

    install.packages(c("sf","DT","leaflet","tidyverse","shinydashboard","leaflet.esri"))

  3. Start the Shiny app:

    shiny::runGitHub("beaconsproject/disturbance_validation")

![app](app.jpg)
