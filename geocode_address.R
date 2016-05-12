library(ggmap)
library(jsonlite)
library(RCurl)
source("geocodeAPI.R")
#data <- read.csv('address.csv')

#normalize the data


#get the input data
#change the name of the input csv file accordingly
infile <- "data_address"
data <- read.csv(paste0('./', infile, '.csv'), header = TRUE)

#get the address list, and append "Country" to the end to set the paramater
addresses = data$ADDRESS
addresses = paste0(addresses, ", Indonesia")

api.key <- 'your API key'

#define geocode function
getGeoData <- function(location, api_key){
  #using ssl connection, not working from computer with internet proxy
  location <- gsub(' ','+',location)
  geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",
                           location,sprintf("&key=%s",api_key), sep=""))
  geo_data <- fromJSON(geo_data)
  return(geo_data$results[[1]]$geometry$location)
}
#define a function that will process google servers response
getGeoDetails <- function(address, api_key){
  #use the geocode function to query google servers
  geo_reply = geocodeAPI(address, source="google", output ='all', 
                         messaging=TRUE, override_limit=TRUE, api_key = api_key)
  #geo_reply = getGeoData(address, api_key)
  #extract the bits that are needed
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply <- geocodeAPI(address, output='all', messaging=TRUE, 
                         override_limit=TRUE, api_key)
    #geo_reply = getGeoData(address, api_key)
    answer$status <- geo_reply$status
  }
  
  #return NA's if there is no match
  if (geo_reply$status !="OK"){
    return(answer)
  }
  #else, extract what is needed
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  #answer$address_components <- geo_reply$results[[1]]$address_components
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()

###########################################
startindex <- 1

#if a temp file exists - load it up and count the rows!
tempfilename <- '_temp_geocoded.rds'
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}
#loop to perform the geocoding
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii], api.key) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

#adding geocoded information back into the original dataset
data$lat <- geocoded$lat
data$long <- geocoded$long
data$accuracy <- geocoded$accuracy
data$formatted_address <- geocoded$formatted_address
#data$address_components <- gecoded$address_components

#finally write it all to the output files
file.rds <- "_geocoded.rds"
file.csv <- "_geocoded.csv"
saveRDS(data, file.rds)
write.table(data, file=file.csv, sep=",", row.names=FALSE)
