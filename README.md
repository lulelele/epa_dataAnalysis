<p align="center">
  <img src="https://user-images.githubusercontent.com/70073886/165483450-f2e6ff6e-4dda-4904-a0d2-73f015bac11b.png">
</p>


<h1 align="center"> Data Analysis Training Workshop</h1>

## Software used  

<ul>
<li><a href = "https://cloud.r-project.org/" target = "_blank"> R </a></li>
<li><a href = "https://www.rstudio.com/products/rstudio/download/preview/" target = "_blank"> RStudio</a></li>
<li><a href = "https://git-for-windows.github.io/" target = "_blank"> Git for Windows</a></li>
<li><a href = "https://qgis.org/en/site/forusers/download.html" target = "_blank"> qGIS</a></li>
</ul>

## Main sample analysis, code packages and sample data

* [Nthiwa et al., 2020. Seroprevalence of foot-and-mouth disease virus in cattle herds raised in Maasai Mara ecosystem in Kenya. *Preventive Veterinary Medicine*, 176](https://doi.org/10.1016/j.prevetmed.2020.104929)
  + [Nthiwa et al., 2020. Dataset](https://data.mendeley.com/datasets/8drcnw9y2k/1)
* [OIE WAHIS - Botswana Diseases Reported](https://www.oie.int)
* [Gilbert M, G Nicolas, G Cinardi, S Vanwambeke, TP Van Boeckel, GRW Wint, TP Robinson (2018) Global Distribution Data for Cattle, Buffaloes, Horses, Sheep, Goats, Pigs, Chickens and Ducks in 2010. *Nature Scientific data*, 5:180227.](https://doi.org/10.1038/sdata.2018.227)
* [Robert J. Hijmans (2022). terra: Spatial Data Analysis. R package version 1.5-21](https://CRAN.R-project.org/package=terra)
* [Mark Stevenson, Evan Sergeant with contributions from Telmo Nunes, Cord Heuer, Jonathon Marshall, Javier
  Sanchez, Ron Thornton, Jeno Reiczigel, Jim Robison-Cox, Paola Sebastiani, Peter Solymos, Kazuki Yoshida,
  Geoff Jones, Sarah Pirikahu, Simon Firestone, Ryan Kyle, Johann Popp, Mathew Jay, Charles Reynard, Allison
  Cheung, Nagendra Singanallur and Aniko Szabo. 2021. epiR: Tools for the Analysis of Epidemiological Data.
  R package version 2.0.35.](https://CRAN.R-project.org/package=epiR)

## R Packages used
* `epiR`
* `ggplot2`
* `terra`
* `scales`
* `dplyr`
* `zoo`
* `lme4`

## Theme 1. Introduction - R

1. Demonstration working directory
2. Introduction to R
    1.  R as a calculator
    2. Assigning values and datatypes in R
        1. Vectors
        2. Matrixes
        3. Factors
        4. Dataframe
            1. accessing data in R
        5. Lists
3. Functions
4. Apply functions in R
    1. Importing dataset for use
    2. Basics of using `Apply` functions in R
    3. Using `tapply`

## Theme 2: Basic statistics in R
> *Note: In this section the basic functionality of R will be expanded upon. Further EDA will take place in the course*

1.  Data wrangling and merging tables
2.  Exploratory data analysis
    1.  Contingency tables
    
## Theme 3: Measures of Disease Frequency 

1.  Exploring results and dealing with data issues
    1. Total tested
    2. Total herds tested
    3. Proportions classified by sex
    4. Apparent prevalence *including discussion of Intra Class Correlation*
        1. Establishing sensitivity and specificity from Parallel tests
    5. Prevalence by Zone
    6. Task: 95% CI based sex classified prevalence
    7. Prevalence in the Botswana context
        1. Data retrieval
        2. `ggplot2`
        3. Data by year
        4. Data by longitude and latitude
2. Epidemic curves
    
## Theme 4: Measures of Association

1.  2-by-2 tables and their use with `epirR`
    1.  Standard Odds ratio with no accounting for other variables (AD/BC)
2.  Univariate logistic regression models - Herd level Fixed effects
    1.  Sex
    2.  Zones
3.  Chi squared tests of association 
4.  Multivariate analysis to look for associations controlling for other variables
    
## Theme 5: Spatial Analysis

1.  Conversion of data to spatial data and basic retrieval and plotting
    1. Plotting a `terra` vector dataset
    2. Plotting a `terra` polygon dataset
    3. Retrieving spatial shapefile from online
2. Background maps from online
3. Data analysis - study zones and cattle density
    1.  Clipping raster to an extent
    2.  Creating buffers
    3.  Spatial Joins
        1.  Point to Polygon
        2.  Point to raster
            1.  allocate underlying population density to each point
            2.  allocate underlying general (polygon orientated) population density to each point
4.  Rasterizing a vector
5.  Exporting data back to original spreadsheet
6.  qGIS Demo - publishable maps
    1.  Writing data to machine
    
## Theme 6: Communicating R Analysis using RMarkdown
> In this theme many of the previous analyses will be repeated but in a markdown format to display options for exporting R analysis into a report format

## Theme 7: Evaluating Diagnostic tests

1.  Zonal subdivisions for different diagnostic tests
2.  Test agreement using Kappa statistic
3.  Diagnostic sensitivity and specificity
4.  McNemar's test



