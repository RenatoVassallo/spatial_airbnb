### Final Project - Analysis of Spatial Data and Images
### By: Renato Vassallo - Data Science for Decision Making 2022-2023
### Title: Spatial interpolation of apartment prices in Barcelona using Airbnb web scraped dataset
###------------------------------------------------------------------------------------------------

# PART I: Data exploration and Pre-processing
#--------------------------------------------
install.packages("readr")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(readr)

airbnb_raw <- read_csv("C:/Users/rvs88/iCloudDrive/BSE/Term 2/Spatial/Final project/Airbnb/data/listings_details.csv", show_col_types = FALSE, guess_max =  10000)

dim(airbnb_raw)

airbnb <- airbnb_raw[c('id', 'last_scraped', 'host_name', 'host_since', 'host_location', 'host_about',
                          'host_is_superhost', 'host_has_profile_pic', 'host_identity_verified', 
                         'neighbourhood_cleansed', 'neighbourhood_group_cleansed', 'latitude', 'longitude', 
                         'property_type', 'room_type', 'accommodates', 'bathrooms_text', 'bedrooms', 'beds', 
                         'amenities', 'price','minimum_nights', 'first_review', 'last_review',
                         'number_of_reviews', 'review_scores_accuracy', 'review_scores_cleanliness', 
                         'review_scores_checkin', 'review_scores_communication', 'review_scores_location',
                         'review_scores_value', 'instant_bookable','calculated_host_listings_count')]

names(airbnb)[1] <- "listing_id"
names(airbnb)[17] <- "bathrooms"

# Remove listings that have number of reviews equal to 0
airbnb <- subset(airbnb, number_of_reviews > 0)
airbnb <- subset(airbnb, property_type %in% c("Entire rental unit", "Private room in rental unit"), drop = TRUE)
#airbnb <- subset(airbnb, room_type %in% c("Entire home/apt", "Private room"), drop = TRUE)
airbnb$host_about[is.na(airbnb$host_about)] <- ''
dim(airbnb)


# Conversion from strings/categorical to numeric
airbnb$host_is_superhost <- as.numeric(ifelse(airbnb$host_is_superhost == 't', 1, 0))
airbnb$host_has_profile_pic <- as.numeric(ifelse(airbnb$host_has_profile_pic == 't', 1, 0))
airbnb$host_identity_verified <- as.numeric(ifelse(airbnb$host_identity_verified == 't', 1, 0))
airbnb$price <- airbnb$price %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()
airbnb$bathrooms <- airbnb$bathrooms %>% str_extract_all("\\(?[0-9,.]+\\)?") %>% gsub(",", "", .) %>% as.numeric()
airbnb$instant_bookable <- as.numeric(ifelse(airbnb$instant_bookable == 't', 1, 0))

# Drop observations with remaining NaN
airbnb <- subset(airbnb, rowSums(is.na(airbnb))==0)
dim(airbnb)

# Create some features
airbnb <- airbnb %>% 
  mutate(listing_duration = as.numeric(difftime(airbnb$last_scraped, airbnb$first_review, unit = 'days')), 
         hosting_duration = as.numeric(difftime(airbnb$last_scraped, airbnb$host_since, unit = 'days')), 
         host_local = as.numeric(str_detect(host_location, 'barcelona|Barcelona')), 
         host_about_len = ifelse(is.na(host_about), 0, nchar(host_about)), 
         total_amenities = ifelse(nchar(amenities)>2, str_count(amenities, ',')+1, 0),
         price_per_person = price / accommodates)

# Remove outliers using IQR

quartiles <- quantile(airbnb$price, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(airbnb$price)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

airbnb <- subset(airbnb, airbnb$price > Lower & airbnb$price < Upper)
dim(airbnb)


# Top 100 reviewed listings
airbnb$is_top_100 <- ifelse(rank(-airbnb$number_of_reviews) <= 100, 1, 0)


categorical <- c("neighbourhood_group_cleansed", "property_type", "room_type")
for (colname in categorical) {
  
  
  temp <- subset(airbnb, is_top_100 == 1) 
  temp <- temp %>% 
    group_by(is_top_100, temp[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp)[2] <- colname
  
  temp1 <- subset(airbnb, is_top_100 == 0) 
  temp1 <- temp1 %>% 
    group_by(is_top_100, temp1[,colname]) %>% 
    summarise(density = n()/nrow(.))
  colnames(temp1)[2] <- colname
  
  temp2 <- rbind(temp, temp1)
  
  plot <- ggplot(data=temp2, aes(x=temp2[[colname]], y=density, fill=as.factor(is_top_100))) + 
    geom_bar(position = 'dodge', stat='identity') + labs(fill = "is_top_100", x = colname, 
                                                         title = paste(colname, " relative density grouped by is_top_100")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(plot)
  
}


# Box plot
hist(airbnb$price)
boxplot(airbnb$price ~ airbnb$is_top_100)

# Aggregate by neighbor
airbnb_agg <- aggregate(airbnb, list(airbnb$neighbourhood_cleansed), FUN=median)
names(airbnb)[1] <- "neighbourhood_name"
dim(airbnb_agg)

# Subset
airbnb_agg <- airbnb_agg[c('latitude', 'longitude','accommodates', 'bathrooms', 
                       'bedrooms', 'beds','price','number_of_reviews','listing_duration', 
            'hosting_duration','host_local','host_about_len','total_amenities','price_per_person' )]


# Convert to a SF object
airbnb_sf <- st_as_sf(airbnb_agg, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84") 
#airbnb_sf <- st_as_sf(airbnb_agg, coords = c("longitude", "latitude"), crs= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
#airbnb_sf <- st_set_crs(airbnb_sf, "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 


# PART II: Preparing maps with spatial data
#-------------------------------------------

### Map of Barcelona

library(tidyverse)
library(sf)
library(ggplot2)
library(mapview)
library(viridis)

bound_bcn <- st_read("C:/Users/rvs88/iCloudDrive/BSE/Term 2/Spatial/Final project/Airbnb/data/bcn_shape/")

#class(map)
#head(map)
#plot(map['AREA'], key.pos = 4)

bound_bcn <- st_transform(bound_bcn, "+proj=longlat +datum=WGS84")


plot(bound_bcn['AREA'], key.pos = 1)

mapview(bound_bcn['PERIMETRE'])



library(geojsonio)
library(leaflet)
nb_geo <- geojson_read('C:/Users/rvs88/iCloudDrive/BSE/Term 2/Spatial/Final project/Airbnb/data/neighbourhoods.geojson', what = 'sp')

other <- airbnb %>% 
  filter(is_top_100 == 0)

top_100 <- airbnb %>% 
  filter(is_top_100 == 1) 

leaflet() %>% setView(lng = 2.154007, lat = 41.390205, zoom = 12) %>%
  addTiles() %>%
  addPolygons(data = nb_geo, color = "#444444", weight = 2, opacity = 1) %>%
  addCircleMarkers(  lng = other$longitude, 
                     lat = other$latitude,
                     radius = 2, 
                     stroke = FALSE,
                     color = "blue",
                     fillOpacity = 0.5, 
                     group = "Other"
  ) %>%
  addCircleMarkers(  lng = top_100$longitude, 
                     lat = top_100$latitude,
                     radius = 3, 
                     stroke = FALSE,
                     color = "red",
                     fillOpacity = 0.9, 
                     group = "Top 100"
  )



map <- st_union(bound_bcn) %>% st_sf()

mapview(map) + mapview(airbnb_sf, zcol = "price")


## Prediction locations

library(dplyr)
grid <- st_make_grid(map, what = "centers", n = c(100, 100)) %>%  st_sf()
coop <- st_filter(grid, map)
plot(coop)


# PART III: Spatial Interpolation
#---------------------------------

## Closest observation (Voronoi)

library(terra)
v <- terra::voronoi(x = terra::vect(airbnb_sf), bnd = map)
v <- st_as_sf(v)

ggplot(data = v) + geom_sf(aes(fill = price)) +
  geom_sf(data = map, fill = NA, lwd = 2) + scale_fill_viridis()

p <- st_intersection(v, coop)

ggplot(p) + geom_sf(aes(col = price)) + scale_color_viridis() # color points


p$x <- st_coordinates(p)[,1]
p$y <- st_coordinates(p)[,2]
ggplot(p, aes(x, y)) + geom_tile(aes(fill = price)) + scale_fill_viridis() # fill tiles


## Nearest neighbors interpolation
library(gstat)
res <- gstat(formula = price ~ 1, locations = airbnb_sf, nmax = 5, set = list(idp = 0))
resp <- predict(res, coop)

resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
ggplot(resp, aes(x, y)) + geom_tile(aes(fill = var1.pred)) + scale_fill_viridis()


## Inverse distance weighting
res <- gstat(formula = price ~ 1, locations = airbnb_sf, nmax = 5, # use all the neighbors locations
             set = list(idp = 1)) # beta = 1 
resp <- predict(res, coop)

resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
ggplot(resp, aes(x, y)) + geom_tile(aes(fill = var1.pred)) + scale_fill_viridis()


## Ensemble approach

# Voronoi
v <- terra::voronoi(x = terra::vect(airbnb_sf), bnd = map)
v <- st_as_sf(v)
p1 <- st_intersection(v, coop)$price
# Nearest neighbors
nn <- gstat(formula = price ~ 1, locations = airbnb_sf, nmax = 5, set = list(idp = 0))
p2 <- predict(nn, coop)$var1.pred
# IDW
gs <- gstat(formula = price ~ 1, locations = airbnb_sf, nmax= 5, set = list(idp = 1))
p3 <- predict(gs, coop)$var1.pred
# Ensemble (equal weights)
weights <- c(1/3, 1/3, 1/3)
p4 <- p1 * weights[1] + p2 * weights[2] + p3 * weights[3]

# Plot
resp <- data.frame(x = st_coordinates(coop)[, 1],
                   y = st_coordinates(coop)[, 2],
                   value = p4)

ggplot(resp, aes(x, y)) + geom_tile(aes(fill = value)) + scale_fill_viridis()



# PART IV: Cross Validation Analysis
#-----------------------------------

set.seed(123)

# Function to calculate the RMSE
RMSE <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

# Split data in 5 sets
kf <- dismo::kfold(nrow(airbnb_sf), k = 5) # k-fold partitioning

# Vectors where RMSE values obtained with each of the methods will be stored
rmse1 <- rep(NA, 5) # Voronoi
rmse2 <- rep(NA, 5) # Nearest neighbors
rmse3 <- rep(NA, 5) # IDW
rmse4 <- rep(NA, 5) # Ensemble

for(k in 1:5) {
  # Split data in test and train
  test <- airbnb_sf[kf == k, ]
  train <- airbnb_sf[kf != k, ]
  # Voronoi
  v <- terra::voronoi(x = terra::vect(train), bnd = map)
  v <- st_as_sf(v)
  p1 <- st_intersection(v, test)$price
  rmse1[k] <- RMSE(test$price, p1)
  # Nearest neighbors
  nn <- gstat(formula = price ~ 1, locations = train, nmax = 5, set = list(idp = 0))
  p2 <- predict(nn, test)$var1.pred
  rmse2[k] <- RMSE(test$price, p2)
  # IDW
  gs <- gstat(formula = price ~ 1, locations = train, nmax = 5, set = list(idp = 1))
  p3 <- predict(gs, test)$var1.pred
  rmse3[k] <- RMSE(test$price, p3)
  # Ensemble (weights are inverse RMSE so lower RMSE has higher weight)
  w <- 1/c(rmse1[k], rmse2[k], rmse3[k])
  weights <- w/sum(w)
  p4 <- p1 * weights[1] + p2 * weights[2] + p3 * weights[3]
  rmse4[k] <- RMSE(test$price, p4)
}

# RMSE obtained for each of the 5 splits
data.frame(voronoi = rmse1, near.neigh = rmse2, IDW = rmse3, ensemble = rmse4)

# Average RMSE over the 5 splits
data.frame(voronoi = mean(rmse1), near.neigh = mean(rmse2), IDW = mean(rmse3), ensemble = mean(rmse4))
