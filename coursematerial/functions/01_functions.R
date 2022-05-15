# mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#design effect
deff <- function(rho, m){
  rho * (m-1) + 1
}

SeParrTwoTest<- function(Se1, Se2){
  Se1+Se2 - (Se1*Se2)
}

SpParrTwoTest<- function(Sp1, Sp2){
  Sp1*Sp2
}


#importing tabular excel files
importTabularOnlineExcel<- function(
  baseURL. = baseURL,
  folder = "tabular",
  filePlusExt = NULL
  ){
  
  if (missing(filePlusExt)) {
    stop("Please provide a vaild file to be sourced")
  }

temp = tempfile()
download.file(paste0(baseURL., "/",folder,"/",filePlusExt), destfile=temp, mode='wb')
return(data.frame(readxl::read_excel(temp)))
unlink(temp)
}


#importing shapefiles
importShapefilesOnline<- function(
  baseURL. = baseURL,
  folder = "gis/shapefiles",
  filePlusExt = NULL
){
  
  if (missing(filePlusExt)) {
    stop("Please provide a vaild file to be sourced")
  }
  library(terra)
  temp <- tempfile()
  tempunzipped <- tempfile()
  URL = paste0(baseURL.,"/", folder,"/", filePlusExt)
  download.file(URL, temp)
  unzip(zipfile = temp, exdir = tempunzipped)
  return(vect(tempunzipped))
  unlink(temp)
  unlink(tempunzipped)
}

importShapefilesLocal<- function(
  baseURL. = baseURL,
  folder = "gis/shapefiles",
  filePlusExt = NULL 
  #filePlusExt = "narok.zip"
){
  
  if (missing(filePlusExt)) {
    stop("Please provide a vaild file to be sourced")
  }
  library(terra)
  tempunzipped <- tempfile()
  URL = paste0(baseURL.,"/", folder,"/", filePlusExt)
  unzip(zipfile = URL, exdir = tempunzipped)
  return(vect(tempunzipped))
  unlink(tempunzipped)
}
