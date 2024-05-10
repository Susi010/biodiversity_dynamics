# <span class='exercise'>Course 2 Exercise 1</span>

**Please hand in the R-Script and the plots. Please add your name or name of group to the file/script names.**
  
  For example: <myplotname>_course2_ex1_<yourlastname>_<yourfirstname>.pdf/png/html and Rscript_course2_ex1_<yourlastname>_<yourfirstname>.Rmd/R

**Revisit the data set from course 1 on wild boar observations in Berlin: `data_wb_melden_en.csv`. 
(it is here: `....\d6_teaching_collection\data\data_berlin\animal_data`).

output_wd <- here("output")

root_wd <- setwd("C:/Users/User/OneDrive - Universität Potsdam/IZW/biodiversity_dynamics/d6_teaching_collection-main")

dber_wd <- here("data","data_berlin")

maps_wd <- paste0(dber_wd, "/geo_raster_current_asc")

anim_wd   <- paste0(dber_wd, "/animal_data")

Now, we want to study the spatial patterns of the wild boar observations. We may hypothesize that wild boar observations are also related to local differences in weather. Answer the following question using spatial data sets and visualizations:**
  
  * **Question 1.1) Were wild boar with many piglets seen more often in warm parts of the city?**
  * **Question 1.2) Were wild boars more often observed at colder spots of the city during sunny weather?**
  * **Question 1.3) *Additional question for the fast ones:* Were large groups of wild boar more frequently seen in areas providing a dense tree cover?**
  
  Follow these steps to answer the questions:
  
#* a. Load the wild boar data (`data_wb_melden_en.csv`) and spatial data on temperature in Berlin (`summer_temp_100m_3035.asc` in `data_berlin`). Remember to set the correct CRS to the raster! (Hint: we need the Lambert Azimuthal Equal Area (LAEA) projection for Europe. Tip: always save the CRS as EPSG code to the filename ;-) )

wb <- read.csv("data_wb_melden_en.csv")

Ber_temp <- rast(paste0(maps_wd, "/summer_temp_100m_3035.asc"))

crs(Ber_temp) <- "+init=epsg:3035"

#* b. Turn the wild boar data into a simple features object.  Remember to set the correct CRS (the wild boar locations were collected in WGS 84) and to transform it to the same CRS as the raster afterwards!
  

wb_sf <- st_as_sf(wb, coords = c("gps_x", "gps_y"), crs = 4326, sf_column_name = "geometry")

wb_sf_3035<-  st_transform(wb_sf, 3035)
  
* c. Inspect both spatial data sets by plotting the temperature raster with the wild boar locations on top using the R basic plot function. The locations should match the map (if not something went wrong when setting the CRS; check if they are the same).

plot(Ber_temp/100)
points(wb_sf_3035, col="blue", pch=4)

#tmap_mode("plot")
#tm_shape((Ber_temp/100)) + tm_raster(palette = "viridis",  title = "Summer temperature Berlin")
#points(wb_sf_3035, col="blue", pch=4)

* d. Select one of the temperature layers and extract the values for each wild boar location.

summer_t <- extract(x = Ber_temp, y = vect(wb_sf_3035))
wb_sf_3035$summer_t <- summer_t$summer_temp_100m_3035

* e. Create a plot that visualizes the temperature at each wild boar observation with 3 or more piglets (y axis) for each number of piglets (e.g. make a boxplot and plot the raw data on top). 

plot( na.omit(wb_sf_3035$summer_t[wb_sf_3035$n_piglets>=3])/100, na.omit(wb_sf_3035$n_piglets[wb_sf_3035$n_piglets>=3]), xlab="Temperature", ylab="No. of piglets", pch=4)

wb_sf_sub <- subset(wb_sf_3035, wb_sf_3035$n_piglets>=3)

ggplot(wb_sf_sub,aes(x=summer_t/100, y= n_piglets)) + geom_boxplot() + geom_jitter(height = 0, width = 0.05, alpha = 0.2, size = 1.5, colour = "#3366FF")

* f. Create a new variable that holds the weather category as either "sunny" or "other".

wb_sf_3035$weather2[wb_sf_3035$weather2!="sunny"] <- "other"

* g. Visualize the temperature at each wild boar observation (y axis) for sunny and other weather (box and whisker plot + raw data). 

ggplot(wb_sf_3035,aes(x=summer_t/100, y= n_piglets, fill=weather2)) + geom_boxplot() + geom_jitter(height = 0, width = 0.05, alpha = 0.2, size = 1.5, colour = "#3366FF")

* h. Save the wild boar observations with the local temperature information as an .Rds file.

saveRDS(wb_sf_3035, paste0(output_wd, "/ wb_temp_3035.rds"))

For the additional question: 
  
* i. Load the tree cover data set (`tree_cover_density_2012_100m_3035.asc` in `data_berlin`).

tree_cov <- rast(paste0(maps_wd, "/tree_cover_density_2012_100m_3035.asc"))

crs(tree_cov) <- "+init=epsg:3035"

* j. Inspect the data set by plotting the raster using the R basic plot function.

plot(tree_cov)

* k. Extract the tree cover within a buffer of 100m around each wild boar location (hint: use the help to inspect the arguments of `extract()`). 

buffer_100 <- st_buffer(wb_sf_3035, dist = 100)

tree_buff <- terra::crop(tree_cov,buffer_100, mask=TRUE)

plot(tree_buff)
points(wb_sf_3035, col="blue", pch=4)

* l. Create a boxplot that shows the tree cover (y axis) based on the group size.

<br><hr><br>
  
  **Session Info**
  
  ```{r sessionInfo, echo=FALSE}
sessionInfo()
```
