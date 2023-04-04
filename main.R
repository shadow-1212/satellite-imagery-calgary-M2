library(raster)
library(tidyverse)
library(sf)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(mapedit)
library(mapview)
library(caret)
library(forcats)
# charge all bands
band1 <- raster('./data/band1.tif')
band2 <- raster('./data/band2.tif')
band3 <- raster('./data/band3.tif')
band4 <- raster('./data/band4.tif')
band5 <- raster('./data/band5.tif')
band6 <- raster('./data/band6.tif')
band7 <- raster('./data/band7.tif')
band8 <- raster('./data/band8.tif')
band9 <- raster('./data/band9.tif')
band10 <- raster('./data/band10.tif')
band11 <- raster('./data/band11.tif')

#band8 res is 15 15 so we must mutipply it by 2 to get 30 30
band8 <- aggregate(band8, 2)

# combine to multi-bands
image <- stack(band1, band2, band3, band4, band5, band6, band7,
               band8, band9, band10, band11)
#to plotRGB true color
par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 4, g = 3, b = 2, axes = TRUE,
        stretch = "lin", main = "True Color Composite")
box(col="white")

#to plotRGB false color
par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 5, g = 4, b = 3, axes = TRUE, stretch = "lin", main = "False Color Composite")
box(col="white")

#calculate NDVI to indentify vegetation =(NIR - Red)/(NIR + Red)
ndvi <- (image[[5]] - image[[4]])/(image[[5]] + image[[4]])
#minimum
min(ndvi@data@values, na.rm = T)
#maximum
max(ndvi@data@values, na.rm = T)

# ggplot the ndvi
as(ndvi, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  ggplot(data = .) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "NDVI for Calagary",
       x = " ",
       y = " ") +
  scale_fill_gradient(high = "#CEE50E",
                      low = "#087F28",
                      name = "NDVI")
#If we compare the result to the false color composite created above, the areas with a higher NDVI correspond to the pockets of red that indicate more vegetation.

#supervised classification
# create training points in mapview

#! no longer needed because we use shapefile to load training_points
# true_color_points <- viewRGB(image, r = 4, g = 3, b = 2) %>% editMap()
#
# # save as clouds after first iteration
# clouds <- true_color_points$finished$geometry %>% st_sf() %>% mutate(class = "clouds", id = 1)
# # save as developed land second time
# developed <- true_color_points$finished$geometry %>% st_sf() %>% mutate(class = "developed", id = 2)
#
# false_color_points <- viewRGB(image, r = 4, g = 3, b = 2) %>% editMap()
#
# # then save as undeveloped land after third iteration
# undeveloped <- false_color_points$finished$geometry %>% st_sf() %>% mutate(class = "undeveloped", id = 3)
# # finally save as water
# water <- false_color_points$finished$geometry %>% st_sf() %>% mutate(class = "water", id = 4)
# # combine all training points

training_points <- st_read("./data/calgary_trainingPoints.shp", quiet = TRUE)
# read in the city boundary for calgary
library(patchwork)
cityBoundary <- st_read("./data/CityBoundary.geojson", quiet = TRUE)
# create a map looking at just the distribution of points
A <- ggplot() +
  geom_sf(data = cityBoundary, fill = "light gray", color = NA) +
  geom_sf(data = training_points, size = 0.5) +
  labs(title = "Distribution of\nclassification points") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

# create a map looking at the distribution of points by classification type
B <- ggplot() +
  geom_sf(data = cityBoundary, fill = "light gray", color = NA) +
  geom_sf(data = training_points, aes(color = class), size = 0.5) +
  scale_color_manual(values = c('cyan', 'burlywood', 'darkgreen', 'blue')) +
  labs(title = "Classification points by land use") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

# plot side by side
A + B + plot_layout(ncol = 2)

#extract the values from the image for each training point
#format training points as SpatialPointsDataFrame(df)
training_points <- as(training_points, 'Spatial')
df <- raster::extract(image, training_points) %>%
  round()
head(df)

#get all spectral profil by land cover types
profiles <- df %>%
  as.data.frame() %>%
  cbind(., training_points$id) %>%
  rename(id = "training_points$id") %>%
  na.omit() %>%
  group_by(id) %>%
  summarise(band1 = mean(band1),
            band2 = mean(band2),
            band3 = mean(band3),
            band4 = mean(band4),
            band5 = mean(band5),
            band6 = mean(band6),
            band7 = mean(band7),
            band8 = mean(band8),
            band9 = mean(band9),
            band10 = mean(band10),
            band11 = mean(band11)) %>%
  mutate(id = case_when(id == 1 ~ "clouds",
                        id == 2 ~ "developed",
                        id == 3 ~ "undeveloped",
                        id == 4 ~ "water")) %>%
  as.data.frame()

head(profiles)
#plot the profile
profiles %>%
  select(-id) %>%
  gather() %>%
  mutate(class = rep(c("clouds", "developed", "undeveloped", "water"), 11)) %>%
  ggplot(data = ., aes(x = fct_relevel(key, c("band1", "band2", "band3", "band4", "band5", "band6", "band7", "band8", "band9", "band10", "band11")), y = value, group = class, color = class)) +
  geom_point(size = 2.5) +
  geom_line(lwd = 1.2) +
  scale_color_manual(values=c('cyan', 'burlywood', 'darkgreen', 'blue')) +
  labs(title = "Spectral Profile from Landsat 8 Imagery", x = "Bands", y = "Reflectance") +
  theme(panel.background = element_blank(), panel.grid.major = element_line(color = "gray", size = 0.5), panel.grid.minor = element_line(color = "gray", size = 0.5), axis.ticks = element_blank())
#histogram of spectral profiles
profiles %>%
  select(-id) %>%
  gather() %>%
  mutate(class = rep(c("clouds", "developed", "undeveloped", "water"), 11)) %>%
  ggplot(., aes(x=value, group=as.factor(class), fill=as.factor(class))) +
  geom_density(alpha = 0.75) +
  geom_vline(data = . %>% group_by(class) %>% summarise(grp.mean = mean(value)),
             aes(xintercept=grp.mean, color = class), linetype="dashed", size=1) +
  scale_fill_manual(values=c('cyan', 'burlywood', 'darkgreen', 'blue'),
                    name = "class") +
  scale_color_manual(values=c("gray", "#CD853F", "#3CB371", "#33CEFF")) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5),
        axis.ticks = element_blank()) +
  labs(x = "Reflectance Value",
       y = "Density",
       title = "Density histograms of spectral profiles",
       subtitle = "Vertical lines represent mean group reflectance values")
#combine the class and the extracted values into a dataframe
df <- data.frame(training_points$class, df)
model.class <- rpart(as.factor(training_points.class)~., data = df, method = 'class')
#plot the model
rpart.plot(model.class, box.palette = 0, main = "Classification Tree")
#predict the classification
pr <- predict(image, model.class, type ='class', progress = 'text') %>%
  ratify()

levels(pr) <- levels(pr)[[1]] %>%
  mutate(legend = c("cloud","developed","undeveloped","water"))
#plot the classification
levelplot(pr, maxpixels = 1e6,
          col.regions = c('cyan', 'burlywood', 'darkgreen', 'blue'),#change to colorblind friendly colors
          scales=list(draw=FALSE),
          main = "Supervised Classification of Imagery")
#evaluate the classification
test <- raster::extract(pr, training_points) %>%
  as.data.frame() %>%
  rename(id = ".")

testProbs <- data.frame(
  obs = as.factor(training_points$id),
  pred = as.factor(test$id)
) %>%
  mutate(correct = ifelse(obs == pred, 1, 0))

confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix
#tips to show hydroogy
#using the hydrology from Calgary’s open data site, we can mask the image, so it only shows areas that fall inside hydrological features.
hydro <- st_read("./data/Hydrology.geojson", quiet = TRUE) %>%
  st_transform(crs = st_crs(image))
image_mask <- mask(image, hydro)
points <- viewRGB(image_mask, r = 4, g = 3, b = 2) %>% editMap()
#get data from the raster to a better format
imagePoints <- rasterToPoints(pr) %>%
  as.data.frame()