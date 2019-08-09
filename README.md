<img align="right" width="115" height="49" src="https://github.com/EL-BID/Modelo-de-prediccion-de-crecimiento-urbano-/blob/master/img/IDB_logo.jpg">

# Housing_Deficit

## Description and Context
Understanding housing deficit is crucial in creating housing policy. The code contained in this repo will apply a decisive methodology to determine levels of quantitative, qualitative, and total housing deficit based on census data, using Guyana's census data as an example. An association analysis is applied to indicators causing deficit to highlight patterns among sub-optimal conditions - for example, Guyanese households lacking access to electricity are 64% more likely to also lack adequate sewerage, suggesting that lack of access to utilities and infrastructure often go hand-in-hand. 
![Association analysis of sub-optimal housing conditions in Guyana](https://github.com/IDB-HUD/Housing_Deficit/blob/master/images/AAgraph.JPG "Association analysis of sub-optimal housing conditions in Guyana")

In data-scarce environments reliable nation-wide data on housing conditions might only be available once a decade. To overcome this, the repo contains an additional script that will use night lights data extracted from satellite images in order to predict the housing deficit in years where no census data is available.

More information on the methodology can be found at ______. In brief:

A household is considered to be experiencing quantitative housing deficit if they are living in either...
   1. Cohabitation (> 1 household living in the same dwelling)
   2. Acute overcrowding (more than 5 people per bedroom)

A household is considered to be experiencing qualitative housing deficit if any of the following are considered inadequate... 
   1. Wall material
   2. Roofing material
   3. Overcrowding situation
   4. Access to piped water
   5. Access to electricity
   6. Access to piped sewerage
   7. Access to sanitary garbage disposal


## User Guide
Adustments to the indicators above should be considered depending on the census data to which it is applied. For example, flooring material is an important indicator of housing quality, but Guyana's census does not collect this information. If this this methodology is applied to a dataset in which floor material data is available, it should be taken into account. 

The night lights data originally used to create these scripts was obtained from satellite images available at the National Oceanic and Atmospheric Association's  [Visible Infrared Imaging Radiometer Suite](https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html). The raster data from these images - average luminosity per NDC, a small administrative division - was extracted using QGIS and the shapefiles of the NDCs in Guyana. 

In combination with the NDC shapefiles, the results of the Indiactors.R script and the Predictions.R script can be visualized in QGIS (or similar software), or directly in R, to produce a heat map of deficit that is easily understandable at a glance. 


<p align="center">
  <img width="400" src="https://github.com/EL-BID/Modelo-de-prediccion-de-crecimiento-urbano-/blob/master/img/transport.png">
</p>


![Hotspot analysis overlaid on existing land use map](https://github.com/IDB-HUD/Housing_Deficit/blob/master/images/Qualitative%20Deficit%202012.pdf)
![Hotspot analysis overlaid on existing land use map](https://github.com/IDB-HUD/Housing_Deficit/blob/master/images/Qualitative%20Deficit%202019.pdf)


## Installation Guide
The scripts in this repo can be run from R/Rstudio after adjusting the paths of input files. As previously mentioned, QGIS or similar software will be necessary to extract the night lights raster data from the satellite images. 

#### Dependencies
All input data should be housed in the same folder with the R scripts. The working directories set at the beginning of each script should ensure that the files are located by R when indicated. 

In addition to the basic R/Rstudio software, which must be installed before the scripts can be opened or run, the following modules will be installed in the process of running the scripts:
`<readstata13>`   *this is necessary to read Guyana's census data; this may not be necessary for other census data* 
`<tidyverse>`     *if the install of this package fails, you may need to update your version of R* 
`<arules>`
`<arulesViz>`
`<ggplot2>`


## How to Contribute
Questions or suggestions about this project can be directed to Jordan Fischer at <jfisher@iadb.org>. For all contributions to this project, this repository may be forked directly. 

## Authors
Code:  [Jordan Jasuta Fischer](https://github.com/jordanjasuta)
Methodology: [Camilo Pecha]

## License
The Documentation of Support and Use of the software is licensed under Creative Commons IGO 3.0 Attribution-NonCommercial-NoDerivative (CC-IGO 3.0 BY-NC-ND)

The codebase of this repo uses [AM-331-A3 Software License](LICENSE).

## Limitation of responsibilities
The IDB is not responsible, under any circumstance, for damage or compensation, moral or patrimonial; direct or indirect; accessory or special; or by way of consequence, foreseen or unforeseen, that could arise:

I. Under any concept of intellectual property, negligence or detriment of another part theory; I

ii. Following the use of the Digital Tool, including, but not limited to defects in the Digital Tool, or the loss or inaccuracy of data of any kind. The foregoing includes expenses or damages associated with communication failures and / or malfunctions of computers, linked to the use of the Digital Tool.
