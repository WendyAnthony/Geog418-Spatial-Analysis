##################################################
## ------- Vancouver PM25 Income Equity ------- ##
## ------------ Wendy Anthony ----------------- ##
## ----------- Geog 418 B02  2019-11-26 ------- ##
##################################################

## ----Set_Working_Directory
dir <- "/Users/wendyanthony/Documents/Geog418-AdvSpatialAnalysis/FinalProject/Working" # macBook
setwd(dir)
getwd()

## ----Install_Packages_at_once---------------------------------------
# install.packages(c("sf", "plyr", "dplyr", "spdep", "GISTools", "raster", "rgdal", "spatstat", "sp", "tmap", "gstat", "maptools"))

## ----Load_libraries-------------------------------------------------
library("sf")
library("plyr")
library("dplyr")
library("spdep")
library("GISTools")
library("raster")
library("rgdal")
library("spatstat")
library("sp")
library("spatstat")
library("tmap")
library("gstat")
library("maptools")

######################################################################
## ----------------- Data Preparation ----------------------------- ##
######################################################################

## ----Read_in_PM25_particulate_matter_dataset------------------------
## Dataset I. Read in particulate matter dataset
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data

## ----Info_about_pm25_data_class-------------------------------------
class(pm25)
head(pm25)
str(pm25)
summary(pm25)

## ----Select_columns_1_and_2_pm25_Change_column_names ---------------
pm25 <- pm25[, 1:2]
colnames(pm25) <- c("POSTALCODE", "PM25")
head(pm25)
class(pm25$PM25)
class(pm25$POSTALCODE)
nrow(pm25)

## ----Reading_postal_code_shapefile_data-----------------------------
postalcodes <- shapefile("./BC_PostalCodes/BC_Postal_Codes.shp")

## ----Class_of_postalcodes_data--------------------------------------
class(postalcodes)
str(postalcodes) 

## ----Projection_of_postalcodes_data---------------------------------
crs(postalcodes)
postalcodes.t <- spTransform(postalcodes, CRS("+init=epsg:3005"))
crs(postalcodes.t)

## ----Info_about_data_1----------------------------------------------
class(postalcodes.t)
class(postalcodes.t$POSTALCODE)

## ----Read_in_census_income_data_excel_csv---------------------------
income <- read.csv("Income_exl_num.csv")

## ----Class_income_data----------------------------------------------
class(income)
head(income)
class(income$COL1)
class(income$COL0)
summary(income)

## ----Select_only_ID_and_Income_columns------------------------------
colnames(income) <- c("DAUID", "Income") 
head(income)

## ----Read_in_dissemination_tract_shapefile--------------------------
census.tracts <- shapefile("./BC_DA/BC_DA.shp")
census.tracts

## ----Info_census.tracts---------------------------------------------
head(census.tracts)
class(census.tracts$DAUID) # [1] "character"

## ----Projection_of_censustracts_data--------------------------------
crs(census.tracts)
census.tracts.t <- spTransform(census.tracts, CRS("+init=epsg:3005"))
crs(census.tracts.t)

## ----Merge_income_and_census.tracts_dissemination_data--------------
income.tracts.t <- merge(census.tracts.t, income, by = "DAUID") 
income.tracts.t

## ----Info_about_merge_income.tracts.t_head--------------------------
head(income.tracts.t)
summary(income.tracts.t)

## ----Include_income_not_na------------------------------------------
nrow(income.tracts.t)
income.tracts.t <- income.tracts.t[!is.na(income.tracts.t$Income),]
nrow(income.tracts.t)

## ----Summary_income.tracts.t----------------------------------------
summary(income.tracts.t)
income.tracts.t@data

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
map_MdInc.t

png("map_MdInc.t_income.tracts.t_fisher.png")  
map_MdInc.t
dev.off()

## ----Find_projections-----------------------------------------------
crs(income.tracts.t)
crs(postalcodes.t)

## ----Intersect_postalcodes_incometracts – check projection----------
postalcodes.t <- intersect(postalcodes.t, income.tracts.t)
crs(postalcodes.t)

## ----Plot_data_spatially--------------------------------------------
plot(postalcodes.t) 
png("plot_postalcodes.t_incometracts_intersect.png")
plot(postalcodes.t)
dev.off()

## ----Pm25_summary---------------------------------------------------
head(postalcodes.t) 
summary(pm25)

## ----Merge_PM2.5_data_with_postal_code_data-------------------------
pm25.spatial <- merge(postalcodes.t, pm25, by = "POSTALCODE")

## ----Info_about_pm25.spatial_data-----------------------------------
nrow(pm25.spatial) 
ncol(pm25.spatial) 
head(pm25.spatial)
summary(pm25.spatial)

## ----Info_pm25.spatial----------------------------------------------
nrow(pm25.spatial)  
ncol(pm25.spatial) 
head(pm25.spatial)
class(pm25.spatial)
class(pm25.spatial$PM25)

## ----Remove_na_pm25.spatial-----------------------------------------
summary(pm25.spatial)  
pm25.spatial <- pm25.spatial[!is.na(pm25.spatial$PM25),] 
class(pm25.spatial$PM25)
summary(pm25.spatial)  

## ----Aggregate_PM2.5_values_in_DA_for_single_value_per_DA-----------
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID, FUN=max)
head(pm25.aggregate)

## ----Info_about_pm25.aggregate--------------------------------------
nrow(pm25.aggregate) 
head(pm25.aggregate)
summary(pm25.aggregate)

## ----Info_about_pm25.spatial----------------------------------------
class(pm25.spatial$PM25) 
pm25.spatial$DAUID
summary(pm25.spatial$DAUID)
pm25.spatial$PM25
summary(pm25.spatial$PM25)
is.na(pm25.spatial$PM25)
summary(pm25.spatial)

## ----Re-join_aggregated_data_to_the_income.tracts.t layer-----------
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") 
head(pm25.aggregate)

## ----Info_pm25.aggregate$PM25AGG------------------------------------
summary(pm25.aggregate$PM25AGG)
class(pm25.aggregate$PM25AGG)
pm25.aggregate$PM25AGG

## ----Merge_income.t_and_pm25_aggregate_dissemination_data-----------
income.pm25 <- merge(income.tracts.t, pm25.aggregate, by = "DAUID") 
income.pm25
summary(income.pm25) 

## ----Remove_na_income.pm25$PM25AGG----------------------------------
income.pm25 <- income.pm25[!is.na(income.pm25$PM25AGG),] 
summary(income.pm25) 

## ----Re-join_aggregated_data_to_pm25.spatial_points_layer-----------
pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID")
pm25.points.aggregate

## ----Info_about_data_pm25.points.aggregate--------------------------
class(pm25.points.aggregate) 
head(pm25.points.aggregate)
summary(pm25.points.aggregate)
nrow(pm25.points.aggregate) 

## ---Create_subsample_of_datapoints_PM2.5_dataset_using_sample_n=100-
set.seed(100)
sampleSize=100
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate), sampleSize),]
spSample

## ----Info_about_subset_data_spSample--------------------------------
head(spSample)
nrow(spSample) 
summary(spSample)
spSample@data

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

## ----Info_about_grd_data--------------------------------------------
head(grd)
crs(grd)
class(grd)
summary(grd)

## ----Create_choropleth_map_with_layer_of_sample_100_data_points-----
map_pm25_vit.data_medInc <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "Income", 
              title = "Median Income", 
              style = "jenks", 
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
map_pm25_vit.data_medInc

png("map_pm25_vit.data_medIncome_smp100_1_dot.08.png")
map_pm25_vit.data_medInc
dev.off()

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
income.tracts.t$Z.Ii
income.tracts.t$P

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
## ----------------- Spatial Interpolation ------------------------ ##
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

r <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96

## ----Summary_info_raster_map_PM25_variance--------------------------
summary(r)
nrow(r)

png("KO_PM25_variance_f0_Exp.png")
KO_PM25_variance_f0_Exp
dev.off()

## ----Create_raster_map_PM25_CI--------------------------------------
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


## ----Class-of-r-raster----------------------------------------------
class(r)


######################################################################
## ---------------- Linear Regression Analysis -------------------- ##
######################################################################

## ----Extract_incometracts_and_raster_to_create_new_object-----------
pm.income.poly <- extract(r, income.tracts.t, fun=mean, sp=TRUE)
pm.income.poly

## ----Summary_pm.incom.poly------------------------------------------
class(pm.income.poly)
head(pm.income.poly)
summary(pm.income.poly) 

## ----Summary_pm.incom.poly$layer------------------------------------
class(pm.income.poly$layer)
head(pm.income.poly$layer)
summary(pm.income.poly$layer) 
str(pm.income.poly$layer)
pm.income.poly$layer

## ----Remove_nas_pm.income.poly$layer--------------------------------
pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$layer),] 
summary(pm.income.poly)  

## ----More_info_about_pm.income.poly---------------------------------
class(pm.income.poly) 
head(pm.income.poly)
class(pm.income.poly$layer) 
names(pm.income.poly)  

## ----Rename_pm.income.poly$layer------------------------------------
names(pm.income.poly)[names(pm.income.poly) == "layer"] <- "PM25.mean"
names(pm.income.poly)  

summary(pm.income.poly)

## ----Head_columns_Income_PM25.mean----------------------------------
head(pm.income.poly$Income)
head(pm.income.poly$PM25.mean)

## ----Class_columns_Income_PM25.mean---------------------------------
#Let's say your dataset with both PM2.5 and Income are stored in a dataset called pm.income.poly.
class(pm.income.poly$PM25.mean)
class(pm.income.poly$Income)
nrow(pm25.spatial) 

## ----Summary_pm.income.poly_columns---------------------------------
summary(pm.income.poly$Income)
summary(pm.income.poly$PM25.mean)
summary(pm.income.poly)

## ----Plot_data_columns_PM25_Income----------------------------------
# Now plot the data again
plot(pm.income.poly$PM25.mean~pm.income.poly$Income, main = "Plot of Predicted PM2.5 & Income")
png("PM25.mean_pm.income.poly_Income.png")
plot(pm.income.poly$PM25.mean~pm.income.poly$Income, main = "Plot of Predicted PM2.5 & Income")
dev.off()

## ----Regression_model-----------------------------------------------
lm.model <- lm(pm.income.poly$PM25.mean~pm.income.poly$Income)
plot(lm.model, main = "PM25.mean ~ Plot Income")
#Add the regression model to the plot you created
abline(lm.model)
### Hit <Return> to see next plot: 

## -------------------------------------------------------------------
summary(lm.model)

## ----Get_residuals_from_model---------------------------------------
model.resids <- as.data.frame(residuals.lm(lm.model))

## ----Add_residuals_to_spatialpolygondataframe-----------------------
pm.income.poly$residuals <- residuals.lm(lm.model)


## ----Observe_result_add_residuals_to_spatialpolygondataframe--------
head(pm.income.poly)

## ----Names_pm.income.poly-------------------------------------------
head(pm.income.poly)
names(pm.income.poly)

## ----Map_residuals_PM25_data_with_sample----------------------------
map_pm25_vit.data_regress_residuals <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "residuals", 
              title = "Regression Residuals", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) +
  tm_shape(spSample) +
  tm_dots(col="PM25AGG", palette = "Reds", n=5, 
          title="Sampled PM2.5 \n(ug/m^3)", size=0.08) + 
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_layout(inner.margins = c(.12, .17, .15, .03), title = "Metro Vancouver 2016 Census Tracts\nMedian $ Income Regression Residuals", title.position = c("LEFT", "TOP")) + 
  tm_scale_bar(position = c(0.4, 0.05)) +  
  tm_compass(type= "4star", position=c("RIGHT", "TOP"))
map_pm25_vit.data_regress_residuals

# Use "fisher" instead of "jenks" for larger data sets

png("map_pm25_vit.data_regress_residuals.png")
map_pm25_vit.data_regress_residuals
dev.off()

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
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nQueen Neighbours", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .08, .03))
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
  tm_layout(title = "Metro Vancouver Census Tracts Residuals Lagged Means \nMedian 2016 $ Income (after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))
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
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i Residuals (after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
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
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i Residuals Z values \n(after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
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
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i Residuals P values \n(after Regression)", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_LISA_P

png("map_LISA_vit_Income_P_afterRegression.png")  
map_LISA_P
dev.off()

## -------------------------------------------------------------------
moran.plot(pm.income.poly$residuals, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, main ="Local Moran's i Residuals Plot \nMetro Vancouver 2016 $ Income (after Regression)")

png("moran.plot_vit_Income_afterRegression.png")  
moran.plot(pm.income.poly$residuals, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, main ="Local Moran's i Residuals Plot \nMetro Vancouver 2016 $ Income (after Regression)")
dev.off()
######################################################################
## ------------- GWR Geographically Weighted Regression ----------- ##
######################################################################





######################################################################
## ------------- Point Pattern Analysis --------------------------- ##
######################################################################

