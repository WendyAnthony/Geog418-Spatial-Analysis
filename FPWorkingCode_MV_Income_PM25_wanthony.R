#############################################################
## ---- Metro Vancouver Income~PM25 Spatial Analysis ----- ##
## ----------------- Wendy Anthony ----------------------- ##
## --------------- Geog 418 B02  2019-12-04 -------------- ##
#############################################################

## ----Set_Working_Directory
dir <- "/Users/wendyanthony/Documents/Geog418-AdvSpatialAnalysis/FinalProject/Working" # macBook
setwd(dir)
getwd()

## ----Install_Packages_at_once---------------------------------------
install.packages(c("sf", "plyr", "dplyr", "spdep", "GISTools", "raster", "rgdal", "spatstat", "sp", "tmap", "gstat", "maptools", "spgwr", "grid", "moments", "bcmaps"))
install.packages("bcmapsdata", repos = "https://bcgov.github.io/drat/")

## ----Load_libraries-------------------------------------------------
library("sf")
library("plyr")
library("dplyr")
library("spdep")
library("GISTools")
library("raster") # for making rasters for interpollated surfaces
library("rgdal")
library("spatstat")
library("sp")
library("spatstat")
library("tmap") # for creating themed choropleth maps
library("gstat")
library("maptools")
library("spgwr") # for geographic weighted regression
library("grid") # used to create inset map
library("moments") # used for skewness and kurtosis Descriptive Stats
library("bcmaps")
library("bcmapsdata") # for study area inset map

######################################################################
## ----------------- Data Preparation ----------------------------- ##
######################################################################
## ----Read_in_PM25_particulate_matter_dataset------------------------
## Dataset I. PM2.5 
pm25 <- read.csv("PM25.csv")
summary(pm25)

## ----Select_columns_1_and_2_pm25_Change_column_names ---------------
pm25 <- pm25[, 1:2]
colnames(pm25) <- c("POSTALCODE", "PM25")
head(pm25) # check new column names

## ----Read_in_postal_code_shapefile_data-----------------------------
## Dataset II. BC Postal Codes
postalcodes <- shapefile("./BC_PostalCodes/BC_Postal_Codes.shp")
 
## ----Projection_of_postalcodes_data_to_BCAlbers---------------------
crs(postalcodes)
postalcodes.t <- spTransform(postalcodes, CRS("+init=epsg:3005"))
crs(postalcodes.t)

## ----Read_in_census_tract_income_data_csv---------------------------
## Dataset III. Median Income
income <- read.csv("Income.csv")
head(income)

## ----Select_only_ID_and_Income_columns------------------------------
colnames(income) <- c("DAUID", "Income") 
head(income)

## ----Read_in_dissemination_tract_shapefile--------------------------
## Dataset IV. Census tract polygons
census.tracts <- shapefile("./BC_DA/BC_DA.shp")

## ----Projection_of_censustracts_data_to_BCAlbers--------------------
crs(census.tracts)
census.tracts.t <- spTransform(census.tracts, CRS("+init=epsg:3005"))
crs(census.tracts.t)
head(census.tracts.t)

## ----Merge_income_and_census.tracts_dissemination_data--------------
income.tracts.t <- merge(census.tracts.t, income, by = "DAUID") 

## ----Remove_na_income.tracts----------------------------------------
income.tracts.t <- income.tracts.t[!is.na(income.tracts.t$Income),]

## ----Create_choropleth_map_of_income--------------------------------
map_MdInc.t <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "Income", 
              title = "Median Income", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) + 
  # add compass
  tm_layout(title = "Metro Vancouver Census Tracts Median Income \n2016", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .08, .03))

png("map_MdInc.t_income.tracts.t_fisher.png")  
map_MdInc.t
dev.off()

## ----Intersect_postalcodes_incometracts – check projection----------
postalcodes.t <- intersect(postalcodes.t, income.tracts.t)
crs(postalcodes.t)

## ----Plot_data_spatially--------------------------------------------
plot(postalcodes.t) 
png("plot_postalcodes.t_incometracts_intersect.png")
plot(postalcodes.t)
dev.off()

## ----Merge_PM2.5_data_with_postal_code_data-------------------------
pm25.spatial <- merge(postalcodes.t, pm25, by = "POSTALCODE")

## ----Remove_na_pm25.spatial-----------------------------------------
pm25.spatial <- pm25.spatial[!is.na(pm25.spatial$PM25),] 

## ----Aggregate_PM2.5_values_in_DA_for_single_value_per_DA-----------
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID, FUN=max)

## ----Re-name_columns_pm25_aggregate---------------------------------
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") 
head(pm25.aggregate)

## ----Merge_income.t_and_pm25_aggregate_dissemination_data-----------
income.pm25 <- merge(income.tracts.t, pm25.aggregate, by = "DAUID") 
summary(income.pm25) 

## ----Remove_na_income.pm25$PM25AGG----------------------------------
income.pm25 <- income.pm25[!is.na(income.pm25$PM25AGG),] 
summary(income.pm25) 

## ----Re-join_aggregated_data_to_pm25.spatial_points_layer-----------
pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID")
pm25.points.aggregate
summary(pm25.points.aggregate)

## ---Create_subsample_of_datapoints_PM2.5_dataset_using_sample_n=100-
set.seed(100)
sampleSize=100
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate), sampleSize),]
spSample
summary(spSample)

## ----Plot_spSample & create png-------------------------------------
plot(spSample)
png("plot_spSample_100.png")
plot(spSample)
dev.off()

## ----Create_grid_to_use_in_interpolation_n_is_total_number_cells----
grd <- as.data.frame(spsample(spSample, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(spSample)
grd

## ----Create_choropleth_map_with_layer_of_sample_100_data_points-----
map_pm25_vit.data_medInc <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "Income", 
              title = "Median Income", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) +
  tm_shape(spSample) +
  tm_dots(col="PM25AGG", palette = "Reds", n=5,
          title="Sampled PM2.5 \n(ug/m^3)", size=0.08) + 
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_layout(inner.margins = c(.15, .05, .15, .03), 
            title = "Metro Vancouver 2016 Census Tracts\nMedian $ Income & Sampled PM2.5 Values", title.position = c("LEFT", "TOP")) + 
  tm_scale_bar(position = c(0.4, 0.05)) + 
  tm_compass(type= "4star", position=c("RIGHT", "TOP"))

png("map_pm25_vit.data_medIncome_smp100_1_dot.08.png")
map_pm25_vit.data_medInc
dev.off()

## ----Create_inset_map_for_study_site_location_map-------------------
### ----Create_bc_spatial_map------------------------------------------
# needs libraries: bcmapsdata, bcmaps

## ----Create_inset_map_for_study_site_location_map-------------------
### ----create_bc_spatial_map------------------------------------------
bc <- as_Spatial(bc_neighbours()) #Get shp of BC bounds
bc <- spTransform(bc, CRS("+init=epsg:3005")) #project to BC Albers
bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province
bc

## ----Create_inset_map_for_study_site_location_map-------------------
### ----1_Main_Map-----------------------------------------------------
map_MV_ct <- tm_shape(income.tracts.t) + 
  #tm_fill(col = "gray60") +  #fill polygons +
  tm_borders("grey5", alpha=1, lwd=.21) +
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) + 
  tm_layout(title = "Metro Vancouver Census Tracts 2016", title.position = c("center", "TOP"))
map_MV_ct

## ----2_Create_inset_map_for_context_to_main_message_map-------------
map_tm_bc1 <- tm_shape(bc) + # make the main shape
  tm_fill(col = "gray80") +  # fill polygons
  tm_shape(pm25.points.aggregate) + # map_tm_bc1 (full set of PM2.5 data points)
  #tm_shape(spSample) + # map_tm_bc (fewer PM2.5 sample points don't show up as much)
  tm_dots(col="PM25AGG", palette = "Greys", n=3) +
  tm_legend(show=FALSE)
map_tm_bc1

## ----Create_inset_map_for_study_site_location_map-------------------
### ----3_Define_area_of_interest_create_spatial_object----------------
bc_region = st_bbox(c(xmin = 273435.7, xmax = 368702.7,
                      ymin = 368702., ymax = 1735755),
                    crs = st_crs(map_tm_bc1)) %>% 
  st_as_sfc()

## ----Create_inset_map_for_study_site_location_map-------------------
### ----4_Combine_map_inset_using_viewport_study_area_using_viewport_grid_package
png("map_MV_CT_BC1_inset.png")
map_MV_ct
print(map_tm_bc1, vp = viewport(0.13, 0.33, width = 0.2, height = 0.2))
dev.off()

######################################################################
## ---------------- Descriptive Statistics ------------------------ ##
######################################################################
## ---------Summary_data---------------------------------------------------
summary(income.tracts.t)
summary(income.pm25)
summary(pm25.points.aggregate)
summary(spSample)

## ----Mean_calc-----------------------------------------------------------
mean.income.tracts.t.Income <- mean(income.tracts.t$Income)  # used by Moran's i
mean.income.tracts.t.Income # [1] 33868
mean.income.pm25.PM25AGG <- mean(income.pm25$PM25AGG)
mean.income.pm25.PM25AGG # [1] 2.13386

mean.spSample.Income <- mean(spSample$Income)
mean.spSample.Income # [1] 36021.82
mean.spSample.PM25AGG <- mean(spSample$PM25AGG)
mean.spSample.PM25AGG # [1] 1.996

## ----SD_calc-------------------------------------------------------------
sd.income.tracts.t.Income <- sd(income.tracts.t$Income)
sd.income.tracts.t.Income # [1] 8835.894
sd.income.pm25.PM25AGG <- sd(income.pm25$PM25AGG)
sd.income.pm25.PM25AGG # [1] 1.509542

sd.spSample.Income <- sd(spSample$Income)
sd.spSample.Income # [1] 9416.431
sd.spSample.PM25AGG <- sd(spSample$PM25AGG)
sd.spSample.PM25AGG # [1] 1.491316

## ----Mode_calc-----------------------------------------------------------
mode.income.tracts.t.Income <- as.numeric(names(sort(table(income.tracts.t$Income), decreasing = TRUE))[1])
mode.income.tracts.t.Income # [1] 34176
mode.income.pm25.PM25AGG <- as.numeric(names(sort(table(income.pm25$PM25AGG), decreasing = TRUE))[1])
mode.income.pm25.PM25AGG # [1] 0.1

mode.spSample.Income <- as.numeric(names(sort(table(spSample$Income), decreasing = TRUE))[1])
mode.income.tracts.t.Income # [1] 34176
mode.spSample.PM25AGG <- as.numeric(names(sort(table(spSample$PM25AGG), decreasing = TRUE))[1])
mode.spSample.PM25AGG # [1] 0.1

## ----Median_calc---------------------------------------------------------
med.income.tracts.t.Income <- median(income.tracts.t$Income, na.rm = TRUE)
med.income.tracts.t.Income # [1] 33152
med.income.pm25.PM25AGG <- median(income.pm25$PM25AGG, na.rm = TRUE)
med.income.pm25.PM25AGG # [1] 2

med.spSample.Income <- median(spSample$Income, na.rm = TRUE)
med.spSample.Income # [1] 34752
med.spSample.PM25AGG <- median(spSample$PM25AGG, na.rm = TRUE)
med.spSample.PM25AGG # [1] 1.8

## ----Skewness_calc-------------------------------------------------------
skew.income.tracts.t.Income <- skewness(income.tracts.t$Income, na.rm = TRUE)[1]
skew.income.tracts.t.Income # [1] 0.4087659
skew.income.pm25.PM25AGG <- skewness(income.pm25$PM25AGG, na.rm = TRUE)[1]
skew.income.pm25.PM25AGG # [1] 0.2479925

skew.spSample.Income <- skewness(spSample$Income, na.rm = TRUE)[1]
skew.spSample.Income # [1] 0.4296544
skew.spSample.PM25AGG <- skewness(spSample$PM25AGG, na.rm = TRUE)[1]
skew.spSample.PM25AGG # [1] 0.3529682

## ----Kurtosis_calc-------------------------------------------------------
kurt.income.tracts.t.Income <- kurtosis(income.tracts.t$Income, na.rm = TRUE)[1]
kurt.income.tracts.t.Income # [1] 2.902638
kurt.income.pm25.PM25AGG <- kurtosis(income.pm25$PM25AGG, na.rm = TRUE)[1]
kurt.income.pm25.PM25AGG # [1] 2.044512

kurt.spSample.Income <- kurtosis(spSample$Income, na.rm = TRUE)[1]
kurt.spSample.Income # [1] 2.328493
kurt.spSample.PM25AGG <- kurtosis(spSample$PM25AGG, na.rm = TRUE)[1]
kurt.spSample.PM25AGG # [1] 2.05602

## ----CoV_calc------------------------------------------------------------
CoV.income.tracts.t.Income <- (sd.income.tracts.t.Income / mean.income.tracts.t.Income) * 100
CoV.income.tracts.t.Income # [1] 26.08921
CoV.income.pm25.PM25AGG <- (sd.income.pm25.PM25AGG / mean.income.pm25.PM25AGG) * 100
CoV.income.pm25.PM25AGG # [1] 70.7423

CoV.spSample.Income <- (sd.spSample.Income / mean.spSample.Income) * 100
CoV.spSample.Income # [1] 26.14091
CoV.spSample.PM25AGG <- (sd.spSample.PM25AGG / mean.spSample.PM25AGG) * 100
CoV.spSample.PM25AGG # [1] 74.71524

## ----Normal_dist_calc----------------------------------------------------
norm.income.tracts.t.Income_PVAL <- shapiro.test(income.tracts.t$Income)$p.value
norm.income.tracts.t.Income_PVAL # [1] 7.991269e-20
norm.income.pm25.PM25AGG_PVAL <- shapiro.test(income.pm25$PM25AGG)$p.value
norm.income.pm25.PM25AGG_PVAL # [1] 6.341768e-34

norm.spSample.Income_PVAL <- shapiro.test(spSample$Income)$p.value
norm.spSample.Income_PVAL # [1] 0.005087551
norm.spSample.PM25AGG_PVAL <- shapiro.test(spSample$PM25AGG)$p.value
norm.spSample.PM25AGG_PVAL # [1] 7.730187e-05

## ----Label_object--------------------------------------------------------
Samples <- c("Income (All)", "Income (Sample)", "PM2.5 (All)", "PM2.5 (Sample)") 
Samples

## ----Mean_object---------------------------------------------------------
Mean <- c(mean.income.tracts.t.Income, mean.spSample.Income, mean.income.pm25.PM25AGG, mean.spSample.PM25AGG) 
Mean = round(Mean, 3)
Mean

## ----SD_object-----------------------------------------------------------
StandardDeviation <- c(sd.income.tracts.t.Income, sd.spSample.Income, sd.income.pm25.PM25AGG, sd.spSample.PM25AGG) 
StandardDeviation = round(StandardDeviation, 3)
StandardDeviation

## ----Median_object-------------------------------------------------------
Median <- c(med.income.tracts.t.Income, med.spSample.Income, med.income.pm25.PM25AGG, med.spSample.PM25AGG) 
Median = round(Median, 3)
Median

## ----Mode_object---------------------------------------------------------
Mode <- c(mode.income.tracts.t.Income, mode.spSample.Income, mode.income.pm25.PM25AGG, mode.spSample.PM25AGG) 
Mode = round(Mode, 3)
Mode

## ----Skewness_object-----------------------------------------------------
Skewness <- c(skew.income.tracts.t.Income, skew.spSample.Income, skew.income.pm25.PM25AGG, skew.spSample.PM25AGG) 
Skewness = round(Skewness, 3)
Skewness

## ----Kurtosis_object-----------------------------------------------------
Kurtosis <- c(kurt.income.tracts.t.Income, kurt.spSample.Income, kurt.income.pm25.PM25AGG, kurt.spSample.PM25AGG) 
Kurtosis = round(Kurtosis, 3)
Kurtosis

## ----CoV_object----------------------------------------------------------
CoefficientOfVariation <- c(CoV.income.tracts.t.Income, CoV.spSample.Income, CoV.income.pm25.PM25AGG, CoV.spSample.PM25AGG) 
CoefficientOfVariation = round(CoefficientOfVariation, 3)
CoefficientOfVariation

## ----PVAL_object---------------------------------------------------------
Normality <- c(norm.income.tracts.t.Income_PVAL, norm.spSample.Income_PVAL, norm.income.pm25.PM25AGG_PVAL, norm.spSample.PM25AGG_PVAL) 
Normality

## ----Table_dataframe_DescStats-------------------------------------------
data.for.table1.1 = data.frame(Samples, Mean, Median, Mode, StandardDeviation, CoefficientOfVariation, Skewness, Kurtosis, Normality)
data.for.table1 = data.frame(Samples, Mean, Median, Mode, StandardDeviation, CoefficientOfVariation)
data.for.table2 = data.frame(Samples, Skewness, Kurtosis, Normality)

data.for.table1.1
data.for.table1
data.for.table2

## ----Table_csv_write_DescStats-------------------------------------------
write.csv(data.for.table1.1, "DescriptiveStats1.1.csv", row.names = FALSE)
write.csv(data.for.table1, "DescriptiveStats1.csv", row.names = FALSE)
write.csv(data.for.table2, "DescriptiveStats2.csv", row.names = FALSE)

######################################################################
## ---------------- Autocorrelation Moran's I --------------------- ##
######################################################################
## ----Defining_Queen_Neighbours--------------------------------------
vit.nb <- poly2nb(income.tracts.t)
vit.net <- nb2lines(vit.nb, coords=coordinates(income.tracts.t))

## ----Map_Queen_Neighbours_default_weight_scheme---------------------
map_vit.net <- tm_shape(income.tracts.t) + tm_borders(col='lightgrey') + 
  tm_shape(vit.net) + tm_lines(col='red') +
  tm_scale_bar(width = 0.22, position = c("RIGHT", "BOTTOM")) + 
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nQueen Neighbours", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .08, .03))
map_vit.net

png("map_vit.net_Income_QueenN.png")  
map_vit.net
dev.off()

## ----Creating_Neighbourhood_Weights_Matrix--------------------------
vit.lw <- nb2listw(vit.nb, zero.policy = TRUE, style = "W")
print.listw(vit.lw, zero.policy = TRUE)

## ----Calculate_Lag_Means--------------------------------------------
income.tracts.t$IncLagMeans = lag.listw(vit.lw, income.tracts.t$Income, zero.policy = TRUE)

## ----Map_Lagged_Means-----------------------------------------------
## Use "fisher" instead of "jenks" for larger data sets
map_LagMean <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "IncLagMeans", 
              title = "Median 2016 $ Income\nLagged Means", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) +
  tm_compass(position = c("LEFT", "BOTTOM")) +
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) + 
  tm_layout(title = "Metro Vancouver Census Tracts \nMedian 2016 $ Income Lagged Means", 
            title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))
map_LagMean

png("map_LagMean_vit_Income.png")  
map_LagMean
dev.off()

## ----Global_Morans_I_Test-------------------------------------------
mi <- moran.test(income.tracts.t$Income, vit.lw, zero.policy = TRUE)
mi

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vit.lw)

mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]
z <- (mI - eI) / sqrt(var)

## ----Add_Stats_data_objects_to_data.frame---------------------------
data.for.table1 = data.frame(mI, eI, var, z)
data.for.table1
write.csv(data.for.table1, "moransGlobal_vit_Income.csv", row.names = FALSE)

## ----Local_Morans_i_Lisa_Test---------------------------------------
lisa.test <- localmoran(income.tracts.t$Income, vit.lw)

income.tracts.t$Ii <- lisa.test[,1]
income.tracts.t$E.Ii<- lisa.test[,2]
income.tracts.t$Var.Ii<- lisa.test[,3]
income.tracts.t$Z.Ii<- lisa.test[,4]
income.tracts.t$P<- lisa.test[,5]

## --z_Ii_values_test_FALSE_if_z_less_0.95_if_Ii_less_0_negative--
income.tracts.t$Z.Ho <- income.tracts.t$Z.Ii < 1.96

## ----Ii_value_lessthan0_negative------------------------------------
income.tracts.t$Ii.Neg <- income.tracts.t$Ii < 0

## ----P_onetail_value_test_multiply_by_2_for_twotail-----------
income.tracts.t$Po <- (income.tracts.t$P * 2) > 0.05
income.tracts.t$Po

## ----Info_about_income.tracts_Lisa----------------------------------
income.tracts.t
head(income.tracts.t)

## ----Mapping_Local_Morans_LISA--------------------------------------
map_LISA <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's i", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))
map_LISA

png("map_LISA_vit_Income.png")  
map_LISA
dev.off()

## ----Mapping_Local_Morans_LISA_Z (manual breaks)--------------------
# https://stackoverflow.com/questions/49423007/how-to-adjust-color-palette-with-customized-breaks-in-tmap
map_LISA_Z <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "Z.Ii", 
              title = "\nLocal Moran's i Z values", 
              breaks = c(-Inf, -2.326, -1.96, 1.96, 2.326, Inf ),
              palette = "PuOr", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i Z values", title.position = c("LEFT", "TOP"), 
            inner.margins = c(.08, .03, .13, .03))
map_LISA_Z

png("map_LISA_vit_Income_Z.png")  
map_LISA_Z
dev.off()

## ----Mapping_Local_Morans_LISA_P (manual breaks)--------------------
map_LISA_P <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "P", 
              title = "\nLocal Moran's i P values",
              breaks = c(0, 0.001, 0.01, 0.05, Inf ),  # 95% p=0.05
              palette = "-BuPu", n = 5,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i P values", title.position = c("LEFT", "TOP"), 
            inner.margins = c(.08, .03, .13, .03))
map_LISA_P

png("map_LISA_vit_Income_P.png")  
map_LISA_P
dev.off()

## ----Local_Morans_i_Scatter_Plot------------------------------------
moran.plot(income.tracts.t$Income, vit.lw, zero.policy=NULL, 
           spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", 
           ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, 
           main ="Local Moran's i Plot for Metro Vancouver 2016 $ Income")

png("moran.plot_vit_Income.png")  
moran.plot(income.tracts.t$Income, vit.lw, zero.policy=NULL, 
           spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", 
           ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, 
           main ="Local Moran's i Plot for Metro Vancouver 2016 $ Income")
dev.off()

######################################################################
## ----------------- Spatial Interpolation  KO -------------------- ##
######################################################################
## ----Define_KO_formula----------------------------------------------
f.0 <- as.formula(PM25AGG ~ 1) 

## ----Create_KO_variogram_Gaussian_model-----------------------------
var.smpl <- variogram(f.0, spSample, cloud = FALSE) 
dat.fit.gau  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE, vgm(model="Gau"))
plot(var.smpl, dat.fit.gau, main = "PM2.5 Variogram Gaussian Model")

## -------------------------------------------------------------------
summary(dat.fit.gau)

## ----Create_png_for_KO_Variogram_Gaussian_Model---------------------
png("KO_PM25_Variogram_f0_Gau_bestfit.png")
plot(var.smpl, dat.fit.gau, main = "PM2.5 Variogram Gaussian Model")
dev.off()

## ----Create_KO_variogram_Spherical_model----------------------------
var.smpl <- variogram(f.0, spSample, cloud = FALSE) 
dat.fit.sph  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE, vgm(model="Sph"))
plot(var.smpl, dat.fit.sph, main = "PM2.5 Variogram Spherical Model")

## -------------------------------------------------------------------
summary(dat.fit.sph)

## ----Create_png_for_KO_Variogram_Sperical_Model---------------------
png("KO_PM25_Variogram_f0_Sph_bestfit.png")
plot(var.smpl, dat.fit.sph, main = "PM2.5 Variogram Spherical Model")
dev.off()

## ----Create_KO_variogram_Exponential_model--------------------------
var.smpl <- variogram(f.0, spSample, cloud = FALSE) 
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE, vgm(model="Exp"))

## ----Plot_PM25_KO_Variogram_Exponential_Model-----------------------
plot(var.smpl, dat.fit, main = "PM2.5 Variogram Exponential Model")

## -------------------------------------------------------------------
summary(dat.fit)

## ----Create_png_for_KO_Variogram_Exponential_Model------------------
png("KO_PM25_Variogram_f0_Exp_bestfit.png")
plot(var.smpl, dat.fit, main = "PM2.5 Variogram Exponential Model")
dev.off()

## ----Perform_KO_krige_interpolation---------------------------------
dat.krg <- krige( f.0, spSample, grd, dat.fit)

## ----Convert_KO_kriged_surface_raster_object------------------------
r <- raster(dat.krg)

## ----Info_about_KO_kriged_surface_raster_object---------------------
r
class(r)
nrow(r)
summary(r)
head(r)

## ----Plot_map_KO_Predicted_PM25_values------------------------------
KO_PM25_predict_f0_Exp <- tm_shape(r) + 
  tm_raster(n=10, palette="Reds",  
            title="Predicted PM25 \n(ug/m^3)") +
  tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_scale_bar(width = 0.22, position = c("RIGHT", "BOTTOM")) + 
  tm_layout(title = "Metro Vancouver Ordinary Kriging\nPM2.5 Predicted Values", title.position = c("LEFT", "TOP"), inner.margins = c(.15, .20, .13, .03))
KO_PM25_predict_f0_Exp

png("KO_PM25_predict_f0_Exp.png")
KO_PM25_predict_f0_Exp
dev.off()

## ----Create_raster_KO_variance--------------------------------------
r <- raster(dat.krg, layer="var1.var")

## ----Create_raster_map_PM25_variance--------------------------------
KO_PM25_variance_f0_Exp <- tm_shape(r) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance PM25 \n(squared ug/m^3)") +
  tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_scale_bar(width = 0.22, position = c("RIGHT", "BOTTOM")) + 
  tm_layout(title = "Metro Vancouver Ordinary Kriging \nPM2.5 Variance", title.position = c("LEFT", "TOP"), inner.margins = c(.15, .20, .13, .03))
KO_PM25_variance_f0_Exp

## ----Summary_info_raster_map_PM25_variance--------------------------
summary(r)
nrow(r)

png("KO_PM25_variance_f0_Exp.png")
KO_PM25_variance_f0_Exp
dev.off()

## ----Create_raster_map_PM25_CI--------------------------------------
r <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
summary(r)

KO_PM25_CI_f0_Exp <- tm_shape(r) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI PM25 \n(ug/m^3)") + tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_scale_bar(width = 0.22, position = c("RIGHT", "BOTTOM")) + 
  tm_layout(title = "Metro Vancouver Ordinary Kriging \nPM2.5 Confidence Intervals", title.position = c("LEFT", "TOP"), inner.margins = c(.15, .20, .13, .03))
KO_PM25_CI_f0_Exp

png("KO_PM25_CI_f0_Exp.png")
KO_PM25_CI_f0_Exp
dev.off()

## ----Perform_KO_krige_interpolation_again_for_regresson_raster------
dat.krg <- krige( f.0, spSample, grd, dat.fit)
r <- raster(dat.krg)

## ----Class-of-r-raster----------------------------------------------
class(r)
summary(r)

######################################################################
## ---------------- Linear Regression Analysis -------------------- ##
######################################################################
## ----Extract_incometracts_and_raster_to_create_new_object-----------
pm.income.poly <- extract(r, income.tracts.t, fun=mean, sp=TRUE)
pm.income.poly

## ----Summary_pm.incom.poly-----------------------------------------------
class(pm.income.poly) # [1] "SpatialPolygonsDataFrame"
head(pm.income.poly)
summary(pm.income.poly)  # var1.pred has NA's 115

## ----Summary_pm.incom.poly$layer-----------------------------------------
class(pm.income.poly$var1.pred) # [1] "numeric"
head(pm.income.poly$var1.pred)
summary(pm.income.poly$var1.pred) # NA's 115

## ----Info_about_pm.income.poly$layer-------------------------------------
str(pm.income.poly$var1.pred)
pm.income.poly$var1.pred

## ----Remove_nas_pm.income.poly$layer-------------------------------------
pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$var1.pred),] 
summary(pm.income.poly)  # NA's 0

## ----More_info_about_pm.income.poly--------------------------------------
class(pm.income.poly) # [1] "SpatialPolygonsDataFrame"
head(pm.income.poly)
class(pm.income.poly$var1.pred) # [1] "numeric"
names(pm.income.poly)  # names of columns

## ----Rename_pm.income.poly$layer-----------------------------------------
names(pm.income.poly)[names(pm.income.poly) == "var1.pred"] <- "PM25.mean"
names(pm.income.poly)  # names of columns

summary(pm.income.poly)

## ----Head_columns_Income_PM25.mean---------------------------------------
head(pm.income.poly$Income)
head(pm.income.poly$PM25.mean)

## ----Class_columns_Income_PM25.mean--------------------------------------
# Both PM2.5 and Income are stored in a dataset called pm.income.poly.
class(pm.income.poly$PM25.mean) # [1] "numeric"
class(pm.income.poly$Income)
nrow(pm25.spatial) # 57870

## ----Summary_pm.income.poly_columns--------------------------------------
summary(pm.income.poly$Income)
summary(pm.income.poly$PM25.mean)
summary(pm.income.poly)

## ----Plot_data_columns_Income_PM25---------------------------------------
plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income ~ PM2.5 Mean")
png("pm.income.poly_Income_PM25.mean.png")
plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income ~ PM2.5 Mean")
dev.off()

## ----Regression_model_1--------------------------------------------------
lm.model.1 <- lm(pm.income.poly$Income~pm.income.poly$PM25.mean)

## ------------------------------------------------------------------------
summary(lm.model.1)

## ----Plot_of_income_PM25_abline-----------------------------------------
plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income ~ PM2.5 Mean")
abline(lm.model.1, col="red", lw=2,lty=2)

png("pm.income.poly_Income_PM25.mean.png")
plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income ~ PM2.5 Mean")
abline(lm.model.1, col="red", lw=2, lty=2)
dev.off()

## ----Get_residuals_from_model1-------------------------------------------
model.1.resids <- as.data.frame(residuals.lm(lm.model.1))

## ----Add_residuals1_to_spatialpolygondataframe---------------------------
pm.income.poly$residuals1 <- residuals.lm(lm.model.1)

## ----Observe_result_add_residuals_to_spatialpolygondataframe-------------
head(pm.income.poly)

## ----Names_pm.income.poly------------------------------------------------
head(pm.income.poly)
names(pm.income.poly)

## ----Map_residuals1_PM25_data_with_sample--------------------------------
map_pm25_vit.data_regress_residuals1 <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "residuals1", 
              title = "Regression Residuals1", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) +
  #tm_shape(spSample) +
  #tm_dots(col="PM25AGG", palette = "Reds", n=5,
          title="Sampled PM2.5 \n(ug/m^3)", size=0.08) + 
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_layout(inner.margins = c(.12, .17, .15, .03), title = "Metro Vancouver 2016 Census Tracts\nIncome ~ PM2.5 Regression Residuals1", title.position = c("LEFT", "TOP")) + 
  tm_scale_bar(position = c(0.4, 0.05)) +  
  tm_compass(type= "4star", position=c("RIGHT", "TOP")) 
# Use "fisher" instead of "jenks" for larger data sets

map_pm25_vit.data_regress_residuals1

#create png
png("map_pm25_vit.data_regress_residuals1.noDot.png")
map_pm25_vit.data_regress_residuals1
dev.off()

## ----After_regression_info_pm.income.poly--------------------------------
summary(pm.income.poly)
pm.income.poly

######################################################################
## ------------- Autocorrelation Moran's I (after regression) ----- ##
######################################################################
## ----After_regression_info_pm.income.poly---------------------------
summary(pm.income.poly)
pm.income.poly

## -------------------------------------------------------------------
vit.nb <- poly2nb(pm.income.poly)
vit.net <- nb2lines(vit.nb, coords=coordinates(pm.income.poly))

## -------------------------------------------------------------------
map_vit.net <- tm_shape(pm.income.poly) + tm_borders(col='lightgrey') + 
  tm_shape(vit.net) + tm_lines(col='red') +
  # add scale bar - first value is TOP, second value is BOTTOM
  tm_scale_bar(width = 0.22, position = c("RIGHT", "BOTTOM")) + 
  # add compass
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_layout(title = "Metro Vancouver Census Tracts 2016 Income ~ PM2.5 \nQueen Neighbours (after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .08, .03))
map_vit.net

png("map_vit.net_Income_QueenN_afterRegression.png")  
map_vit.net
dev.off()

## -------------------------------------------------------------------
vit.lw <- nb2listw(vit.nb, zero.policy = TRUE, style = "W")
print.listw(vit.lw, zero.policy = TRUE)

## -------------------------------------------------------------------
pm.income.poly$IncLagMeans = lag.listw(vit.lw, pm.income.poly$residuals, zero.policy = TRUE)

## -------------------------------------------------------------------
map_LagMean <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "IncLagMeans", 
              title = "Median 2016 $ Income\nLagged Means", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) +
  # add scale bar - first value is TOP, second value is BOTTOM
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) + 
  tm_layout(title = "Metro Vancouver Census Tracts 2016 Residuals Lagged Means \n Income ~ PM2.5 (after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))
map_LagMean
## Use "fisher" instead of "jenks" for larger data sets

png("map_LagMean_vit_Income_afterRegression.png")  
map_LagMean
dev.off()

## -------------------------------------------------------------------
mi <- moran.test(pm.income.poly$residuals, vit.lw, zero.policy = TRUE)
mi

## -------------------------------------------------------------------
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vit.lw)  

## -------------------------------------------------------------------
mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]
z <- (mI - eI) / sqrt(var)

## -------------------------------------------------------------------
data.for.table1 = data.frame(mI, eI, var, z)
write.csv(data.for.table1, "moransGlobal_vit_Income_afterRegression.csv", row.names = FALSE)

## ----Lisa_Test_afterRegression--------------------------------------
lisa.test <- localmoran(pm.income.poly$residuals, vit.lw)

## -------------------------------------------------------------------
pm.income.poly$Ii <- lisa.test[,1]
pm.income.poly$E.Ii<- lisa.test[,2]
pm.income.poly$Var.Ii<- lisa.test[,3]
pm.income.poly$Z.Ii<- lisa.test[,4]
pm.income.poly$P<- lisa.test[,5]

## -------------------------------------------------------------------
# if z value is less than 95% significance level, value is FALSE
pm.income.poly$Z.Ho <- pm.income.poly$Z.Ii < 1.96
# if Ii < 0, value is negative
pm.income.poly$Ii.Neg <- pm.income.poly$Ii < 0
# P-value is one-tailed, need to multiply p-value *2 to correspond to 95% 2 -tailed if P *2>0.05 - higher chance of wrongly deciding significance
pm.income.poly$Po <- (pm.income.poly$P * 2) > 0.05
pm.income.poly$Po

## -------------------------------------------------------------------
map_LISA <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "Ii", 
              title = "\nLocal Moran's i", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts 2016 Income ~ PM2.5 \nLocal Moran's i Residuals (after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_LISA
## Use "fisher" instead of "jenks" for larger data sets

png("map_LISA_vit_Income_afterRegression.png")  
map_LISA
dev.off()

## -------------------------------------------------------------------
# > use manual breaks
map_LISA_Z <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "Z.Ii", 
              title = "\nLocal Moran's i Z values", 
              breaks = c(-Inf, -2.326, -1.96, 1.96, 2.326, Inf ),
              palette = "PuOr", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts 2016 Income ~ PM2.5 \nLocal Moran's i Residuals Z values \n(after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_LISA_Z

png("map_LISA_vit_Income_Z_afterRegression.png")  
map_LISA_Z
dev.off()

## -------------------------------------------------------------------
map_LISA_P <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "P", 
              title = "\nLocal Moran's i P values",
              breaks = c(0, 0.001, 0.01, 0.05, Inf ),  # 95% p=0.05
              palette = "-BuPu", n = 5,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts 2016 Income ~ PM2.5 \nLocal Moran's i Residuals P values \n(after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_LISA_P

png("map_LISA_vit_Income_P_afterRegression.png")  
map_LISA_P
dev.off()

## -------------------------------------------------------------------
moran.plot(pm.income.poly$residuals1, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Income ~ PM2.5 Residuals", ylab="Spatially Lagged Mean Income ~ PM2.5 Residuals", quiet=NULL, main ="Local Moran's i Residuals Plot \nMetro Vancouver 2016 Income ~ PM2.5  (after Regression)")

png("moran.plot_vit_Residuals_afterRegression.png")  
moran.plot(pm.income.poly$residuals1, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Income ~ PM2.5 Residuals", ylab="Spatially Lagged Mean Income ~ PM2.5 Residuals", quiet=NULL, main ="Local Moran's i Residuals Plot \nMetro Vancouver 2016 Income ~ PM2.5  (after Regression)")
dev.off()

######################################################################
## ------------- GWR Geographically Weighted Regression ----------- ##
######################################################################
## ----GWR_Geographic_Weighted_Regression-----------------------------
# First add the polygon coordinates to the spatialpolygondataframe.
crs(pm.income.poly)
# Obtain coordinates using "coordinates" function from the sp library
pm.income.poly.coords <- sp::coordinates(pm.income.poly)

## ----GWR_results----------------------------------------------------
head(pm.income.poly.coords)

## ----Add_coordinates_back_to_spatialpolygondataframe----------------
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)
head(pm.income.poly)
names(pm.income.poly)

## ----Determine_bandwidth_for_GWR------------------------------------
## Need to Install package spgwr
## this will take a while
GWRbandwidth <- gwr.sel(pm.income.poly$Income~pm.income.poly$PM25.mean, 
                        data=pm.income.poly, coords=cbind(pm.income.poly$X, pm.income.poly$Y),adapt=T) 

## ----Perform_GWR_on_two_variables_with_bandwidth_determined---------
### This will take a looooooong time
gwr.model = gwr(pm.income.poly$Income~pm.income.poly$PM25.mean, 
                data=pm.income.poly, coords=cbind(pm.income.poly$X, pm.income.poly$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#Print the results of the model
gwr.model

## ----GWR_results_SDF------------------------------------------------
#Look at the results in detail
results <- as.data.frame(gwr.model$SDF)
head(results)

## ----Add_local_r_square_values_to_map-------------------------------
pm.income.poly$localr <- results$localR2
pm.income.poly$localr
head(pm.income.poly)

## ----Create_choropleth_map_of_r_square_values-----------------------
local.r.square <- pm.income.poly$localr

## ----Map_r_square---------------------------------------------------
map_gwr_r_square <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "localr", 
              title = "\nGWR r^2",
              style = "fisher", 
              palette = "viridis", n = 4,
              border.col = "grey", 
              border.alpha = 0.03) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census 2016 Tracts  Income ~ PM 2.5 GWR r^2", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_gwr_r_square

# Use "fisher" instead of "jenks" for larger data sets

png("map_gwr_r_square.png")  
map_gwr_r_square
dev.off()

## ----Results_Map_coefficients---------------------------------------
# View(results)
class(results)
names(results)
class(results$pm.income.poly.PM25.mean)
head(results)
results$pm.income.poly.PM25.mean
summary(results$pm.income.poly.PM25.mean)

## ----Assign_coefficients--------------------------------------------
pm.income.poly$coeff <- results$pm.income.poly.PM25.mean
head(pm.income.poly)
summary(pm.income.poly$coeff)

## ----Create_choropleth_map_of_coefficients--------------------------
local.coefficient <- pm.income.poly$coeff
local.coefficient
summary(local.coefficient)

## ----Map_coefficient------------------------------------------------
map_gwr_coefficient <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "coeff", 
              title = "\nGWR coefficient",
              style = "fisher", 
              palette = "viridis", n = 4,
              border.col = "grey", 
              border.alpha = 0.03) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts 2016 GWR Income ~ PM 2.5 Coefficient", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_gwr_coefficient

png("map_gwr_coefficient.png")  
map_gwr_coefficient
dev.off()

######################################################################
## ------------- Point Pattern Analysis --------------------------- ##
######################################################################
## ----spSample_info_PPA----------------------------------------------
head(spSample)
summary(spSample)

## ----Coordinates_added_to_spSample----------------------------------
spSample$x <- coordinates(spSample)[,1]
spSample$y <- coordinates(spSample)[,2]

## ----Finds_zero_distance_among_points-------------------------------
zd <- zerodist(spSample)

## ----Check_for_and_remove_duplicated_points-------------------------
spSample <- remove.duplicates(spSample)
spSample

## -------------------------------------------------------------------
spSample.ext <- as.matrix(extent(spSample)) 

## ----Observation_window---------------------------------------------
window <- as.owin(list(xrange = spSample.ext[1,], yrange = spSample.ext[2,]))

## ----Create_ppp_object_from_spatstat--------------------------------
spSample.ppp <- ppp(x = spSample$x, y = spSample$y, window = window)
spSample.ppp

## ----Number_quads---------------------------------------------------
quads <- 10

## ----Count_number_of_quads------------------------------------------
qcount <- quadratcount(spSample.ppp, nx = quads, ny = quads)
qcount

## ----Create_dataframe_for_quadcount---------------------------------
qcount.df <- as.data.frame(qcount)
qcount.df

## ----Count_number_of_quadrats_with_distinct_number_of_points--------
qcount.df <- plyr::count(qcount.df,'Freq')
qcount.df

## --Change_column_names_to_xnumber_points_and_frequency_of_quadrats -
colnames(qcount.df) <- c("x","f")
qcount.df

## -------------------------------------------------------------------
qcount.df$x
qcount.df$f

X2 <- qcount.df$x ^ 2
X2

sum.f.x2 <- sum(qcount.df$f * qcount.df$x ^ 2)
sum.f.x2

M <- sum(qcount.df$f)
M

N <- sum(qcount.df$x * qcount.df$f)
N

sum.fx.2 <- (sum(qcount.df$f * qcount.df$x))^2
sum.fx.2

VAR <- (sum.f.x2 -(sum.fx.2/M))/M-1
VAR

MEAN <- N / M
MEAN

VMR <- VAR / MEAN
VMR

## ----Chi_square_test_for_random_spatial_pattern---------------------
chi.square = VMR *(M - 1)
p = 1 - pchisq(chi.square, (M - 1))
p  

## ----Add_NND_Stats_data_objects_4_to_data.frame---------------------
data.for.table4 = data.frame(qcount.df$x, qcount.df$f, X2, n, nnd, sum.f.x2, M, N, sum.fx.2, VAR, MEAN, VMR, p)
data.for.table4

## ----write_csv_datatable4_NND--------------------------------------
write.csv(data.for.table4, "NND.data.for.table4.csv", row.names = FALSE)

## ----Calculate_nearestNeighbour-------------------------------------
nearestNeighbour <- nndist(spSample.ppp)
nearestNeighbour

## ----Convert_nearestNeighbor_object_into_dataframe------------------
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
nearestNeighbour

## ----Nearest_Neighbour_info-----------------------------------------
class(nearestNeighbour)
head(nearestNeighbour)

## ----Change_column_name_to_Distance---------------------------------
colnames(nearestNeighbour) = "Distance"
head(nearestNeighbour)

n <- nrow(nearestNeighbour)
n  

nnd <- sum(nearestNeighbour$Distance) / n
nnd 

studyArea <- gArea(spgeom = income.tracts.t, byid = FALSE)
studyArea

pointDensity <- n / studyArea
pointDensity 

r.nnd <- 1/(2*sqrt(pointDensity))
r.nnd

d.nnd <- 1.07453 / pointDensity^(1/2)
d.nnd

R <- nnd / r.nnd
R 

SE.NND <- 0.26136 / (n * pointDensity)^(1/2)
SE.NND  

z <-  (nnd - r.nnd) / SE.NND
z  

## ----Add_NND_Stats_data_objects_5_to_data.frame---------------------
data.for.table5 = data.frame(n, nnd, studyArea, pointDensity, r.nnd, d.nnd, R, SE.NND, z)
data.for.table5

## ----create_csv_datatable5_PPA--------------------------------------
write.csv(data.for.table5, "PPA.data.for.table5.csv", row.names = FALSE)

######################################################################
## ------------- End Spatial Analysis ----------------------------- ##
######################################################################