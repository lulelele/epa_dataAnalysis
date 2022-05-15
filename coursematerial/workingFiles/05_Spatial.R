# Theme 5: Basic spatial analysis in R ####

rm(list = ls())

# 5.a required packages and base variables ####
library(terra)
library(dplyr)
library(geodata)
library(maptiles)

#Base URL for imports
baseURL = "https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial"

#Call generic functions
source(paste0(baseURL,"/functions/01_functions.R"))

# Download data set #### Aside: using a function for repetitive work
df <- importTabularOnlineExcel(filePlusExt = "nthiwa_kenya_seroprev.xlsx")

# Get a feel for the data
str(df)
summary(df)
head(df)
plot(df$eastings, df$southings)

#Rename the location variable to lon and lat
# for terra when importing from data frame this naming is required
colnames(df)[10] <- "lat"
colnames(df)[11] <- "lon"
plot(df$lon, df$lat)

# 5.1 Very Important: Convert the locations data set and plot Sampling Villages - Aside CRS ####
# 5.1.1 Plotting a basic terra vector object ####
samplLoc = terra::vect(df,  crs="+proj=longlat +datum=WGS84")
class(samplLoc)
samplLoc
plot(samplLoc)

# 5.1.2 Plotting a basic terra polygon object ####
# Import the Kenyan polygon and plot
#You could do this from online using the geodata package
kenya <- geodata::gadm("Kenya", level=1, path=".")
head(kenya)
plot(kenya, "NAME_1")
plot(geodata::gadm("Botswana", level=1, path="."), "NAME_1")

# isolating one component - in this case the Narok county
narok =  kenya[kenya$NAME_1 == 'Narok', ]
plot(narok)
plot(samplLoc, add = TRUE)

# 5.1.3 Protected areas download - example of shapefile from Web ####

protectedAreas = importShapefilesOnline(filePlusExt = "ken_protected_areas.zip")
plot(protectedAreas)
#Finding Masai Mara
click(protectedAreas) #use Esc to exit

masaiMara = protectedAreas[protectedAreas$AREANAME == 'Masai Mara', ]
plot(masaiMara, col = "green", alpha = 0.3)
plot(narok, col = "blue", alpha = 0.3, add = TRUE)

#Do you notice the slight topology error? 
#beyond the scope of this course: qGIS was used to correct this
#import the corrected narok and Masai Mara shape files

# Read the shapefile
narok = importShapefilesOnline(filePlusExt = "narok.zip")
masaiMara = importShapefilesOnline(filePlusExt = "masaiMara.zip")
plot(masaiMara, col = "green", alpha = 0.3)
plot(narok, col = "blue", alpha = 0.3, add = TRUE)

# 5.2 Map backgrounds from online ####
# extent is the bounding box surrounding a spatial object
plot(ext(narok))
plot(narok, col = "blue", alpha = 0.3, add = TRUE)

#To get a map back ground you can use the maptiles package, although there are many online map tiles repositories
bg <- maptiles::get_tiles(ext(narok))
plot(ext(narok))
plot(bg, add = TRUE)
plot(narok, add = TRUE)

# 5.3 Data analysis - Study zones and Cattle Density ####
# Evaluate what we have
plot(bg)
polys(narok) #alternative way to add a plot - I prefer just using add = TRUE
points(samplLoc)
polys(masaiMara, col="red", alpha = 0.5)

#let's manipulate this raster to the extent of our working environment - the Narok region
# import the continental Africa from file - this will be used to clip the raster
africa = importShapefilesOnline(filePlusExt = "africa.zip")
plot(africa)
plot(ext(narok), add = TRUE)

#Getting a raster from a file online is simpler than tabular XLSX or zipped shapefiles
cattleDensAfrica = terra::rast(paste0(baseURL,"/gis/rasters/densCattleAfrica.tif"))
plot(cattleDensAfrica)
plot(africa, add = TRUE)
plot(ext(narok), add = TRUE)

# its quite difficult to work with raster data at the level of Africa - don't run the code but your computer may have
# a hard time (1-2 minutes) when you run first the Narok plot and then overlay the raster
##################################################
#plot(narok)
#plot(cattleDensAfrica, add = TRUE, alpha = 0.5)
##################################################

#5.3.1 Clipping raster to an extent ####
#Clip the cattle density raster to only extend to the Narok region
plot(ext(narok))
plot(narok, add = TRUE)
cattleDensNarok = crop(cattleDensAfrica, ext(narok))
# if you tried that without an Internet connection it would fail
sources(cattleDensAfrica)
sources(cattleDensNarok) #this is in local computer memory
cattleDensNarok

#What happens when we clip a raster to a polygon?
plot(crop(cattleDensAfrica, narok)) # it still remains rectangular

plot(cattleDensNarok)
plot(narok, add = TRUE)
points(samplLoc)
polys(masaiMara, col="red", alpha = 0.5)

# 5.3.2 Buffers: Making the study zones ####
# slight difference here from Nthiwa et al. publication since the point of their center for buffers is uncertain
plot(ext(narok))
plot(masaiMara, add = TRUE)
plot(buffer(masaiMara, 20000), add = TRUE, col = "red", alpha = 0.2)

# make all three 20, 40 and 60 km buffers using an apply function
buffers = c(20000, 40000, 60000) # these distances in Meters 
#(big bonus of the terra package - most other GIS software would require a re-projection into a projected CRS)

buffersManuscript = lapply(buffers,
                           function(x) 
                             crop(narok, 
                                  buffer(masaiMara, x)
                                  )
                           )

b20 = buffersManuscript[[1]]
b40 = buffersManuscript[[2]]
b60 = buffersManuscript[[3]]

b20

b20$class = "20km"
b40$class = "40km"
b60$class = "60km"

plot(narok)
plot(b20, add = TRUE, col="green", alpha = 0.3)
plot(b40, add = TRUE, col="blue", alpha = 0.3)
plot(b60, add = TRUE, col="red", alpha = 0.3)


#the issue however is they are not bands (so called multi-ring buffer) - so we'll need to cookie cutter them out
b60 = erase(b60, b40)
b40 = erase(b40, b20)
#b20 doesn't need erasing as the original Manuscript included all of the reserve and ~20 km buffer

plot(narok)
plot(b20, add = TRUE, col="green", alpha = 0.3)
plot(b40, add = TRUE, col="blue", alpha = 0.3)
plot(b60, add = TRUE, col="red", alpha = 0.3)
plot(samplLoc, add = TRUE)
plot(cattleDensNarok, add = TRUE, alpha = 0.5)

#combine all three buffers into the same object
bAll = rbind(b20, b40, b60)
bAll = bAll[,"class"] #retain just the class tag
plot(bAll, "class")

#5.3.3 - Spatial Joins ####
#5.3.3.1 - Point to Polygon ####
#do a spatial join of points sampled onto buffers to allocate buffers to points
e = extract(bAll, samplLoc)
str(e)
head(e)
str(samplLoc)
sampLoc_region = merge(samplLoc, e, by.x=c('rowid'), by.y=c('id.y'))
head(sampLoc_region)

# establish if there are any missing classes - i.e. any points that were not within a buffer
plot(sampLoc_region[is.na(sampLoc_region$class), ]) # there are
nrow(sampLoc_region[is.na(sampLoc_region$class), ]) #a total of 6

plot(bAll)
plot(sampLoc_region[is.na(sampLoc_region$class), ], col = "red", add = TRUE)

#establish which classes the authors allocated these points to
head(sampLoc_region[is.na(sampLoc_region$class),c("rowid","study_str")],20) #they have been allocated to the 0 level

#establish which of the 3 regions relates to the author allocated zero
head(sampLoc_region[sampLoc_region$study_str == 0,c("rowid","study_str","class")],20)

#we can see that study_str 0 is the 60 km zone - they probably had an incorrect y coordinate for those 6 values
#lets set the class to 60 km then in the data
sampLoc_region[is.na(sampLoc_region$class), ]$class <- "60km"

plot(narok)
plot(sampLoc_region, "class", col=rainbow(25), add = TRUE)
plot(bAll, add = TRUE, "class", alpha = 0.3)

# see the status of each point
plot(sampLoc_region, "fmd_exp_st", col=c("green", "red"), add = TRUE)

# can also evaluate plots side by side
plot(sampLoc_region, c("class", "fmd_exp_st"), col=rainbow(25))

#5.3.3.2 -Spatial Join: Point to Raster ####

plot(cattleDensNarok, main = "Narok raster - animals per pixel")
plot(narok, add = TRUE)
plot(bAll, "class", col=c("green", "blue", "red"), add = TRUE, alpha = 0.3)
plot(sampLoc_region, "fmd_exp_st", col=c("green", "red"), add = TRUE, alpha = 0.3)
north(type=4)
sbar(d=50,
     type = "bar",
     divs = 4,
     below = "kilometers",
     xy = "bottomleft")

# Aside: can also bin raster values ####
plot(cattleDensNarok, main = "Narok raster - animals per pixel", 
     col=rev(terrain.colors(25)), 
     breaks=seq(0, global(cattleDensNarok, max)$max+4999, 5000))
plot(narok, add = TRUE)
plot(bAll, "class", col=c("green", "blue", "red"), add = TRUE, alpha = 0.3)
plot(sampLoc_region, "fmd_exp_st", col=c("green", "red"), add = TRUE, alpha = 0.3)
north(type=4)
sbar(d=50,
     type = "bar",
     divs = 4,
     below = "kilometers",
     xy = "bottom")

# Aside: area of each raster cell - this will help with Density calculations
res(cattleDensNarok) # ~ 100km ^2 - but this is not intuitive unless it was a projected coordinate system
cellSize(cattleDensNarok, unit="km", names="corrected")@ptr@.xData$range_min #min of 85 km squared
head(values(cattleDensNarok))

#Aside: Raster summary statistics
global(cattleDensNarok, 'sum')
global(cattleDensNarok, 'mean')

#5.3.3.2.1 -Spatial Join: Point to Raster : allocate underlying population density to each point ####
sampLoc_regionCellCount <- extract(cattleDensNarok, sampLoc_region)
sampLoc_region$cattleCellCount = sampLoc_regionCellCount[, -1] #another way of allocating extracted values back onto the spatial object
head(sampLoc_region)
tail(sampLoc_region)

#5.3.3.2.2 -Spatial Join: Point to Raster : allocate underlying general (polygon orientated) population density to each point ####
# extract cattle total for each buffer area
bAll$rowID = seq(1, nrow(bAll), 1) # to get a rowID to use for linking back the extracted values to the spatial object
bAllCattleCount <- extract(cattleDensNarok, bAll) # All raster values intersecting the polygon are linked to the Buffer

mean_bAllCattleCount <- 
  bAllCattleCount %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarize(cattleCount = mean(`5_Ct_2010_Da`))

bAll = merge(bAll, mean_bAllCattleCount, by.x=c('rowID'), by.y=c('ID'))
head(bAll, 10)


#you can now see the issue of limiting the buffer to Narok - any polygon cut-off by a human boundary becomes less valuable

########################### not RUN ##########################################
# an alternative is to already use the mean in the extract function
#mean_bAllCattleCount2 <- extract(cattleDensNarok, bAll, fun = mean)
#bAll = merge(bAll, mean_bAllCattleCount, by.x=c('rowID'), by.y=c('ID'))
#########################################################################

#Cattle count is one thing, but really density would be a better predictor
bAll$area = expanse(bAll,unit="km")
bAll$density = bAll$cattleCount/bAll$area
head(bAll)
plot(bAll, "density")

#exercise - Allocate the cattle density for each region to the sampLoc_region Vector
plot(sampLoc_region, add = TRUE)

# 5.4 - example of rasterizing a vector ####
sampLoc_regionRaster <- rasterize(sampLoc_region, cattleDensNarok, fun=sum) #number of points per cell
plot(sampLoc_regionRaster)
plot(sampLoc_region, "fmd_exp_st", col=c("green", "red"), add = TRUE, alpha = 0.3)

# 5.5 Exporting data to add to spreadsheet data ####
dfnew = cbind(sampLoc_region$rowid, df, cattleCount = sampLoc_region$cattleCellCount)
write.csv(dfnew, "C:/Users/User/Desktop/temp.csv")

# 5.6 Demo for qGIS ####
# data we need to plot and try reproduce the map in Question ####

plot(kenya)
plot(narok, add = TRUE)
plot(masaiMara, add = TRUE)
plot(bAll, add = TRUE)
plot(sampLoc_region, "fmd_exp_st", col=c("green", "red"), add = TRUE, alpha = 0.3)
plot(cattleDensNarok, alpha = 0.5, add = TRUE)


#create a folder on PC to access in qGIS
folderExport = "C:/Users/User/Desktop/plotqGIS"
  
#write these files to that folder
writeRaster(cattleDensNarok, filename = paste0(folderExport, "/cattleDensNarok.tif"), overwrite=TRUE)
writeVector(kenya, filename = paste0(folderExport, "/kenya.shp"), overwrite=TRUE)
writeVector(narok, filename = paste0(folderExport, "/narok.shp"), overwrite=TRUE)
writeVector(bAll, filename = paste0(folderExport, "/bAll.shp"), overwrite=TRUE)
writeVector(sampLoc_region, filename = paste0(folderExport, "/sampLoc_region.shp"), overwrite=TRUE)
writeVector(masaiMara, filename = paste0(folderExport, "/masaiMara.shp"), overwrite=TRUE)
  