<img align="right" width="115" height="49" src="https://github.com/EL-BID/Modelo-de-prediccion-de-crecimiento-urbano-/blob/master/img/IDB_logo.jpg">

# Housing Deficit Estimation
![analytics image (flat)](https://raw.githubusercontent.com/vitr/google-analytics-beacon/master/static/badge-flat.gif)
![analytics](https://www.google-analytics.com/collect?v=1&cid=555&t=pageview&ec=repo&ea=open&dp=/Housing_Deficit/readme&dt=&tid=UA-4677001-16)

## Description and Context
Understanding housing deficit is crucial in creating housing policy. The code contained in this repo will apply a decisive methodology to determine levels of quantitative, qualitative, and total housing deficit based on census data, using census data as an example. 

In data-scarce environments reliable nation-wide data on housing conditions might only be available once a decade. To overcome this, the repo contains an additional script that will use night lights data extracted from satellite images in order to now-cast the housing deficit in years where no census data is available. More information on the methodology can be found in the [Housing Deficit Methodology and Guide](https://github.com/EL-BID/Housing_Deficit/blob/master/Housing%20Deficit%20-%20Methodology%20and%20Guide.pdf). 

Some countries’ statistical institutions publish a specific set of definitions and methodology for calculating housing deficit within that country, as is the case with Peru, and international bodies such as [CEPAL](https://www.cepal.org/es), [MINURVI](https://www.minurvi.org/), and [UN Habitat](http://unhabitat.org/un-habitat-at-a-glance/) provide general guidelines in the absence of a country-specific methodology. In countries where no national methodology exists, this exercise will use a methodology based on UN Habitat and MINURVI guidelines, and the mthodology used by Colombia’s [Departamento Administrativo Nacional de Estadística](https://www.dane.gov.co/) (DANE, the National Statistics Office). In brief:

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

After cleaning and preparing the data (a process which will be different for each area of study but can be guided by **`script1_Data_prep.R`**), these calculations can be run in an adjusted version of **`script2_Indicators.R`**. 

Two tests are run on the derived indicators using **`script3_Stat_test.R`** to determine if the prediction methodology will be an appropriate fit for the data in question: 
   1. Association analysis
   2. Coefficient of Variance

The association analysis patterns among the causes of sub-optimal conditions - for example, Guyanese households lacking access to electricity are 64% more likely to also lack adequate sewerage, suggesting that lack of access to utilities and infrastructure often go hand-in-hand. 

<p align="center">
  <img width="400" src="https://github.com/IDB-HUD/Housing_Deficit/blob/master/images/AAgraph.JPG">
</p>

Then, using satellite imagery data extracted using QGIS (see [Housing Deficit Methodology and Guide](https://github.com/EL-BID/Housing_Deficit/blob/master/Housing%20Deficit%20-%20Methodology%20and%20Guide.pdf)) or **`script4_Luminosity.R`**, a simple regression is used in **`script5_Predictions.R`** to 'predict' (now-cast) housing deficit for the year of most recently available satellite imagery based on the average luminosity of each administrative division. 


## User Guide
This analysis is run across 5 scripts: 
1. script1_Data_prep.R
2. script2_Indicators.R
3. script3_Stat_test.R
4. script4_Luminosity.R
5. script5_Prediction.R

All scripts can be run at once using the **`Master_script.R`**. Adustments to the indicators above should be considered depending on the census data to which it is applied. For example, flooring material is an important indicator of housing quality, but Guyana's census does not collect this information. If this this methodology is applied to a dataset in which floor material data is available, it should be taken into account. 

The night lights data originally used to create these scripts was obtained from satellite images available at the National Oceanic and Atmospheric Association's  [Visible Infrared Imaging Radiometer Suite](https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html) - more recently available through the [Earth Observations Group](https://eogdata.mines.edu/download_dnb_composites.html). The raster data from these images - average luminosity per NDC, a small administrative division - was extracted using QGIS and the shapefiles of the NDCs in Guyana. 

The geospatial results of **`script2_Indicators.R`** and the **`script5_Predictions.R`** can be visualized in QGIS (or similar software), or directly in R, to produce a heat map of deficit that is easily understandable at a glance. 


<p align="center">
  <img width="800" src="https://github.com/EL-BID/Housing_Deficit/blob/master/images/Def_Estimation.JPG">
</p>

Detailed, explanation and instructions for the overall Housing Deficit Estimation process can be found [here](https://github.com/EL-BID/Housing_Deficit/blob/master/Housing%20Deficit%20-%20Methodology%20and%20Guide.pdf). 


## Installation Guide
The scripts in this repo can be run from R/Rstudio after adjusting the paths of input files. As previously mentioned, QGIS or similar software will be necessary to extract the night lights raster data from the satellite images. 

#### Dependencies
All input data should be housed in the same folder with the R scripts. The working directories set at the beginning of each script should ensure that the files are located by R when indicated. 

In addition to the basic R/Rstudio software, which must be installed before the scripts can be opened or run, the following modules will be installed in the process of running the scripts:

`<readstata13>`   *........this is necessary to read Guyana's census data; this may not be necessary for other census data* 

`<tidyverse>`     *........if the install of this package fails, you may need to update your version of R* 

`<dplyr>`

`<arules>`

`<arulesViz>`

`<rgdal>`

`<sp>`

`<sf>`

`<tmap>`

`<rgeos>`



## How to Contribute
Questions or suggestions about this project can be directed to Jordan Jasuta Fischer at <jordan.j.fischer@gmail.com>. For all contributions to this project, this repository may be forked directly. 


## Authors
Code:  [Jordan Jasuta Fischer](https://github.com/jordanjasuta)

Methodology: [Camilo Pecha](http://camilopecha.webflow.io) and [Jordan Jasuta Fischer](https://github.com/jordanjasuta)


## License
The Documentation of Support and Use of the software is licensed under Creative Commons IGO 3.0 Attribution-NonCommercial-NoDerivative (CC-IGO 3.0 BY-NC-ND)

The codebase of this repo uses [AM-331-A3 Software License](https://github.com/IDB-HUD/Housing_Deficit/blob/master/LICENSE.md).


## Limitation of responsibilities
The IDB is not responsible, under any circumstance, for damage or compensation, moral or patrimonial; direct or indirect; accessory or special; or by way of consequence, foreseen or unforeseen, that could arise:

I. Under any concept of intellectual property, negligence or detriment of another part theory; I

ii. Following the use of the Digital Tool, including, but not limited to defects in the Digital Tool, or the loss or inaccuracy of data of any kind. The foregoing includes expenses or damages associated with communication failures and / or malfunctions of computers, linked to the use of the Digital Tool.
