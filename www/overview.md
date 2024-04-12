## Disturbance Validation

The Disturbance Validation app is a Shiny app that enables users to i) interactively examine linear and areal surface disturbance features along with several satellite imagery sources, ii) validate industry and disturbance type attributes, and iii) randomly select individual features and their associated attributes. This permits alternative digitizers or other users to quickly look at the digitized features and their assigned attributes, and visually compare them to more than one high resolution imagery source. Three satellite images are available for viewing: Esri WorldImagery, Google Imagery, and SPOT Imagery for circa 2021.

<center><img src="app.jpg" width="600"></center>

The Disturbance Validation app consists of 3 sections:

### Introduction

This section includes this overview page plus a tab describing all permitted industry and disturbance types.

### Validate attributes

This sections allows the user to view a summary of the linear and areal disturbance attributes and validate their values. There are three steps:

1. Upload data: Upload a geopackage (".gpkg") containing study area boundary, linear disturbances, and areal disturbances (see notes below). 
2. Select layer names from the 3 dropdown boxes.
3. Validate results: results will be displayed in the 4 tabs on the right.

**Required Layers and Attributes**
  
This page describes the required map layers and attributes that are used by the Disturbance Validation app.
  
Required layers

- **studyarea** : A single polygon outlining the boundary of the study area e.g., a watershed or ecoregion or any other user-defined area.
- **linear_disturbance** : Linear anthropogenic surface disturbance features. Available from: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/
- **areal_disturbance** : Areal (polygonal) anthropogenic surface disturbance features. Available from: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/

Required attributes

The **studyarea** layer does not require any particular attributes.

The **linear_disturbance** layer must include the following attributes:
    
- TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
- TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Survey / Cutline, Access Road
  
The **areal_disturbance** layer must include the following attributes:
    
- TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
- TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Drill Pad, Clearing


### Random features

This section allows users to randomly select a feature to view it along with its attributes.

The areal disturbance layer should include the following attributes:
  - REF_ID
  - TYPE_INDUSTRY
  - TYPE_DISTURBANCE
  - CREATED_BY
  - IMAGE_DATA
  - Area_ha

The linear disturbance layer should include the following attributes:
  - REF_ID
  - TYPE_INDUSTRY
  - TYPE_DISTURBANCE
  - CREATED_BY
  - IMAGE_DATA
  - Length_km

