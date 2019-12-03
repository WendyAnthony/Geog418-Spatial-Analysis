## ----Set_Working_Directory, include=TRUE, results="hide"-----------------
#dir <- "/Users/geographydepartment/Documents/Geog418/FinalProject-Geog418/Working"  # homeComputer
 dir <- "/Users/wendyanthony/Documents/Geog418-AdvSpatialAnalysis/FinalProject/Working" # laptop
# dir <- "Z:/Geog418-FinalProject/Working" # labComputer
# dir <- "/Users/UVicCMC/Downloads/Data"  # CMCComputer
setwd(dir)
getwd()


## ----Install_Packages_at_once, include=TRUE, results="hide"--------------
# install.packages(c("sf", "plyr", "dplyr", "spdep", "GISTools", "raster", "rgdal", "spatstat", "sp", "tmap", "gstat", "maptools", "spgwr"))
#install.packages("kable")
#install.packages("kableExtra")
#install.packages("bcmapsdata", repos = "https://bcgov.github.io/drat/")


## ----Load_libraries, include=TRUE, results="hide"------------------------
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
library("knitr")
library("kableExtra")
library("bcmapsdata") # for study area inset map
library("grid") # used to create inset map


## ----Read_in_PM25_particulate_matter_dataset-----------------------------
## Dataset I. PM2.5 particulate matter dataset
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data


## ----Info_about_pm25_data_class------------------------------------------
class(pm25)

## ----Info_about_pm25_data_str--------------------------------------------
str(pm25)

## ----Info_about_pm25_data_head-------------------------------------------
head(pm25)

## ----Info_about_pm25_data_summary----------------------------------------
summary(pm25)


## ----Select_columns_1_and_2_pm25-----------------------------------------
pm25 <- pm25[, 1:2]


## ----Change_column_names_pm25--------------------------------------------
colnames(pm25) <- c("POSTALCODE", "PM25")
head(pm25)


## ----kable_head_pm25-----------------------------------------------------
# makes html table
kable(head(pm25))
write.csv(pm25, "pm25.csv", row.names = FALSE)


## ----Class_new_columns_pm25_pm25-----------------------------------------
class(pm25$PM25)


## ----Class_new_columns_pm25_POSTALCODE-----------------------------------
class(pm25$POSTALCODE)


## ----Number_of_rows_in_pm25----------------------------------------------
nrow(pm25)


## ----Read_in_postal_code_shapefile_data----------------------------------
## Dataset II. BC Postal Codes
postalcodes <- shapefile("./BC_PostalCodes/BC_Postal_Codes.shp")


## ----Class_of_postalcodes_data-------------------------------------------
class(postalcodes)


## ----String_structure_postalcodes----------------------------------------
str(postalcodes) 


## ----Projection_of_postalcodes_data--------------------------------------
crs(postalcodes)


## ----Transform_reproject_postalcodes_BC_Albers---------------------------
postalcodes.t <- spTransform(postalcodes, CRS("+init=epsg:3005"))

## ----Recheck_projection_postalcoades-------------------------------------
crs(postalcodes.t)


## ----Info_about_data_1---------------------------------------------------
class(postalcodes.t)


## ----kable_head_postalcode-----------------------------------------------
# makes html table
kable(head(postalcodes.t))
write.csv(postalcodes.t, "postalcodes.t.csv", row.names = FALSE)


## ----Class_postalcodes_POSTALCODE----------------------------------------
class(postalcodes.t$POSTALCODE)


## ----Read_in_census_income_csv-------------------------------------------
## Dataset III. Median Income
income <- read.csv("Income.csv")


## ----Class_income_data---------------------------------------------------
class(income)
head(income)
class(income$COL1)
class(income$COL0)


## ----Summary_income_data-------------------------------------------------
summary(income)


## ----Select_only_ID_and_Income_columns-----------------------------------
colnames(income) <- c("DAUID", "Income") 
head(income)


## ----kable_head_income---------------------------------------------------
# makes html table
kable(head(income))
write.csv(income, "income.csv", row.names = FALSE)


## ----Read_in_dissemination_tract_shapefile-------------------------------
## Dataset IV. Census tract polygons
census.tracts <- shapefile("./BC_DA/BC_DA.shp")
census.tracts


## ----Info_census.tracts--------------------------------------------------
head(census.tracts)
class(census.tracts$DAUID) # [1] "character"


## ----kable_head_censustracts---------------------------------------------
# makes html table
kable(head(census.tracts))
write.csv(census.tracts, "census.tracts.csv", row.names = FALSE)


## ----Projection_of_censustracts_data-------------------------------------
crs(census.tracts)


## ----Reproject_censustracts_data_to_BCAlbers-----------------------------
census.tracts.t <- spTransform(census.tracts, CRS("+init=epsg:3005"))
crs(census.tracts.t)


## ----Merge_income_and_census.tracts_dissemination_data-------------------
income.tracts.t <- merge(census.tracts.t, income, by = "DAUID") 
income.tracts.t


## ----Info_about_merge_income.tracts.t_head-------------------------------
head(income.tracts.t)


## ----kable_head_censustracts2--------------------------------------------
# makes html table
kable(head(census.tracts))
write.csv(census.tracts, "census.tracts.1.csv", row.names = FALSE)


## ----Info_about_merge_income.tracts.t_summary----------------------------
summary(income.tracts.t)


## ----Number_rows_income.tracts_before_is.na------------------------------
nrow(income.tracts.t)


## ----Include_income_not_na-----------------------------------------------
income.tracts.t <- income.tracts.t[!is.na(income.tracts.t$Income),]


## ----Number_rows_income.tracts.t_after_is.na-----------------------------
nrow(income.tracts.t)


## ----Summary_income.tracts.t---------------------------------------------
summary(income.tracts.t)


## ----Income.tracts.t_data------------------------------------------------
income.tracts.t@data


## ----Create_choropleth_map_of_income-------------------------------------
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


## ----Create_choropleth_map_png-------------------------------------------
png("map_MdInc.t_income.tracts.t_fisher.png")  
map_MdInc.t
dev.off()

## ----Find_projections----------------------------------------------------
crs(income.tracts.t)
crs(postalcodes.t)


## ----Intersect_postalcodes_incometracts----------------------------------
postalcodes.t <- intersect(postalcodes.t, income.tracts.t)


## ----Find_projections_intersection_postalcodes---------------------------
crs(postalcodes.t)


## ----Plot_data_spatially-------------------------------------------------
plot(postalcodes.t) 


## ----Data_Tabular--------------------------------------------------------
head(postalcodes.t) 


## ----kable_head_postalcodes2---------------------------------------------
# makes html table
kable(head(postalcodes.t))
write.csv(postalcodes.t, "postalcodes.t.csv", row.names = FALSE)


## ----Create_postalcodes.t_plot_png---------------------------------------
png("plot_postalcodes.t_incometracts_intersect.png")
plot(postalcodes.t)
dev.off()

## ----Pm25_summary--------------------------------------------------------
# pm25
summary(pm25)


## ----Merge_PM2.5_data_with_postal_code_data------------------------------
pm25.spatial <- merge(postalcodes.t, pm25, by = "POSTALCODE")


## ----Info_about_pm25.spatial_data----------------------------------------
nrow(pm25.spatial)  # 58320
ncol(pm25.spatial) # 29
head(pm25.spatial)
summary(pm25.spatial)   # NA's 450

## ----Info_pm25.spatial---------------------------------------------------
nrow(pm25.spatial)  # 58320
ncol(pm25.spatial) # 29
head(pm25.spatial)
class(pm25.spatial) # [1] "SpatialPointsDataFrame"
class(pm25.spatial$PM25) # [1] "factor"

## ----Summary_pm25_before_is.na-------------------------------------------
summary(pm25.spatial)   # NA's 450 in PM25


## ----Remove_na_pm25.spatial----------------------------------------------
pm25.spatial <- pm25.spatial[!is.na(pm25.spatial$PM25),]  # removed all NA's
class(pm25.spatial$PM25) # [1] "factor"

## ----Summary_pm25_after_is.na--------------------------------------------
summary(pm25.spatial)   # NA's 0 in PM25


## ----kable_head_pm25spatial----------------------------------------------
# makes html table
kable(head(pm25.spatial))
write.csv(pm25.spatial, "pm25.spatial.csv", row.names = FALSE)


## ----Aggregate_PM2.5_values_in_DA_for_single_value_per_DA----------------
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID, FUN=max)
head(pm25.aggregate)

## ----Info_about_pm25.aggregate-------------------------------------------
nrow(pm25.aggregate)  # 3355
head(pm25.aggregate)
summary(pm25.aggregate)


## ----Info_about_pm25.spatial---------------------------------------------
class(pm25.spatial$PM25) # [1] "factor"
pm25.spatial$DAUID
summary(pm25.spatial$DAUID)
pm25.spatial$PM25
summary(pm25.spatial$PM25)  # NA's 0
is.na(pm25.spatial$PM25)
summary(pm25.spatial)


## ----Re-join_aggregated_data_to_the_income.tracts.t layer----------------
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") 
head(pm25.aggregate)


## ----kable_head_pm25aggregatel-------------------------------------------
# makes html table
kable(head(pm25.aggregate))
write.csv(pm25.aggregate, "pm25.aggregate.csv", row.names = FALSE)

## ----Info_pm25.aggregate$PM25AGG-----------------------------------------
summary(pm25.aggregate$PM25AGG)
class(pm25.aggregate$PM25AGG) # [1] "numeric"
pm25.aggregate$PM25AGG


## ----Merge_income.t_and_pm25_aggregate_dissemination_data----------------
income.pm25 <- merge(income.tracts.t, pm25.aggregate, by = "DAUID") 
income.pm25
summary(income.pm25)  # NA;s 8 in $PM25AGG


## ----Remove_na_income.pm25$PM25AGG---------------------------------------
income.pm25 <- income.pm25[!is.na(income.pm25$PM25AGG),] 
summary(income.pm25)  # no NA's


## ----Re-join_aggregated_data_to_pm25.spatial_points_layer----------------
pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID")
pm25.points.aggregate


## ----Info_about_data_pm25.points.aggregate-------------------------------
class(pm25.points.aggregate) # [1] "SpatialPointsDataFrame"
head(pm25.points.aggregate)
summary(pm25.points.aggregate) # PM25 NA's 0  PM25AGG NA's 0
nrow(pm25.points.aggregate)   #58320


## ----kable_head_pm25.pointsaggregate-------------------------------------
# makes html table
kable(head(pm25.points.aggregate))
write.csv(pm25.points.aggregate, "pm25.points.aggregate.csv", row.names = FALSE)


## ----Create_subsample_of_datapoints_PM2.5_dataset_using_sample_n=100-----
set.seed(100)
sampleSize=100
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate), sampleSize),]
spSample


## ----Info_about_subset_data_spSample-------------------------------------
head(spSample)
nrow(spSample)   # 100 > sample size
summary(spSample)  # no NA's
spSample@data


## ----kable_head_spSample-------------------------------------------------
# makes html table
kable(head(spSample))
write.csv(spSample, "spSample.csv", row.names = FALSE)


## ----Plot_spSample-------------------------------------------------------
plot(spSample)


## ----Create_sample_plot_png----------------------------------------------
png("plot_spSample_100.png")
plot(spSample)
dev.off()


## ----Create_grid_to_use_in_interpolation_n_is_total_number_cells---------
grd <- as.data.frame(spsample(spSample, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(spSample)
grd


## ----Info_about_grd_data-------------------------------------------------
head(grd)
summary(grd)

## ----Grd_crs_class_summary_info------------------------------------------
crs(grd)
class(grd)
summary(grd)


## ----Create_choropleth_map_with_layer_of_sample_100_data_points----------
map_pm25_vit.data_medInc <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "Income", 
              title = "Median Income", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) +
  tm_shape(spSample) +
  # using minus sign before color -PuOr n=9, -RdBu n=9 reverses RdBu Brewer color scheme so Rd is hot & Bu is cold  
  #  tm_bubbles(size = "value", col = "red", scale = 1.3) + 
  tm_dots(col="PM25AGG", palette = "Reds", n=5, # n=9 gets finer detail? n = 7 or n=5  n=4, same with only 5 classes, even with no n=
          title="Sampled PM2.5 \n(ug/m^3)", size=0.08) + # , alpha = .5
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  ##?????? only showing 11 points /s/b 13 ??
  tm_layout(inner.margins = c(.15, .05, .15, .03), title = "Metro Vancouver 2016 Census Tracts\nMedian $ Income & Sampled PM2.5 Values", title.position = c("LEFT", "TOP")) + # 0.03, 0.92
  tm_scale_bar(position = c(0.4, 0.05)) +  # "LEFT", "BOTTOM"
  tm_compass(type= "4star", position=c("RIGHT", "TOP")) #"RIGHT", "TOP" 0.87, 0.82
map_pm25_vit.data_medInc

## ----create_png_map_CT_MI------------------------------------------------
png("map_pm25_vit.data_medIncome_smp100_1_dot.08.png")
map_pm25_vit.data_medInc
dev.off()


## ----Defining_Queen_Neighbours-------------------------------------------
vit.nb <- poly2nb(income.tracts.t)
vit.net <- nb2lines(vit.nb, coords=coordinates(income.tracts.t))


## ----Map_Queen_Neighbours_default_weight_scheme--------------------------
# Map Queen Neighbours (default weight scheme).
map_vit.net <- tm_shape(income.tracts.t) + tm_borders(col='lightgrey') + 
  tm_shape(vit.net) + tm_lines(col='red') +
  # add scale bar - first value is TOP, second value is BOTTOM
  tm_scale_bar(width = 0.22, position = c("RIGHT", "BOTTOM")) + 
  # add compass
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nQueen Neighbours", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .08, .03))
# Create Neighbourhood map for Queen's weight.
map_vit.net


## ----Create_png_of_Median_Income_vit.net_Queen_Neighbour-----------------
png("map_vit.net_Income_QueenN.png")  
map_vit.net
dev.off()


## ----Creating_Neighbourhood_Weights_Matrix-------------------------------
vit.lw <- nb2listw(vit.nb, zero.policy = TRUE, style = "W")
print.listw(vit.lw, zero.policy = TRUE)


## ----Calculate_Lag_Means-------------------------------------------------
income.tracts.t$IncLagMeans = lag.listw(vit.lw, income.tracts.t$Income, zero.policy = TRUE)


## ----Mapping_Lagged_Means------------------------------------------------
map_LagMean <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "IncLagMeans", 
              title = "Median 2016 $ Income\nLagged Means", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) +
  # add scale bar - first value is TOP, second value is BOTTOM
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) + 
  tm_layout(title = "Metro Vancouver Census Tracts \nMedian 2016 $ Income Lagged Means", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))

######
## Use "fisher" instead of "jenks" for larger data sets

map_LagMean


## ----Create_png_map_LagMean_Median_Income--------------------------------
png("map_LagMean_vit_Income.png")  
map_LagMean
dev.off()


## ----Global_Morans_I_Test------------------------------------------------
mi <- moran.test(income.tracts.t$Income, vit.lw, zero.policy = TRUE)
mi


## ----Moran_Range---------------------------------------------------------
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vit.lw)


## ----Calculate_Global_Morans_z_value-------------------------------------
mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]
z <- (mI - eI) / sqrt(var)


## ----Add_Stats_data_objects_to_data.frame--------------------------------
data.for.table1 = data.frame(mI, eI, var, z)
data.for.table1


## ----kable_head_datatable1-----------------------------------------------
# makes html table
kable(head(data.for.table1))
write.csv(data.for.table1, "data.for.table1.csv", row.names = FALSE)


## ----Write_csv_file_with_Global_Morans_results---------------------------
write.csv(data.for.table1, "moransGlobal_vit_Income.csv", row.names = FALSE)


## ----Local_Morans_i_Lisa_Test--------------------------------------------
lisa.test <- localmoran(income.tracts.t$Income, vit.lw)


## ----kable_lisatest------------------------------------------------------
# makes html table
kable(head(lisa.test))
write.csv(lisa.test, "lisa.test.csv", row.names = FALSE)


## ----Lisa_Test-----------------------------------------------------------
income.tracts.t$Ii <- lisa.test[,1]
income.tracts.t$E.Ii<- lisa.test[,2]
income.tracts.t$Var.Ii<- lisa.test[,3]
income.tracts.t$Z.Ii<- lisa.test[,4]
income.tracts.t$P<- lisa.test[,5]


## ----z_Ii_values_test_value_FALSE_if_z_less_0.95_if_Ii_less_0_negative----
income.tracts.t$Z.Ho <- income.tracts.t$Z.Ii < 1.96


## ----Ii_value_lessthan0_negative-----------------------------------------
income.tracts.t$Ii.Neg <- income.tracts.t$Ii < 0


## ----P_onetail_value_test_multiply_by_2_for_twotail, include=TRUE, results="hide"----
income.tracts.t$Po <- (income.tracts.t$P * 2) > 0.05
income.tracts.t$Po


## ----Info_about_income.tracts_Lisa---------------------------------------
income.tracts.t
head(income.tracts.t)
income.tracts.t$Z.Ii
income.tracts.t$P


## ----Make_data_frome_from_Lisa_Test_Data---------------------------------
data.for.table2 = data.frame(income.tracts.t$DAUID, income.tracts.t$Income, income.tracts.t$IncLagMeans, income.tracts.t$Ii, income.tracts.t$E.Ii, income.tracts.t$Var.Ii, income.tracts.t$Z.Ii, income.tracts.t$P, income.tracts.t$Ii.Neg, income.tracts.t$Z.Ho, income.tracts.t$Po)


## ----kable_head_datatable2-----------------------------------------------
# makes html table
kable(head(data.for.table2))
write.csv(data.for.table2, "Moransi.LISA.data.for.table2.csv", row.names = FALSE)


## ----Mapping_Local_Morans_LISA-------------------------------------------
map_LISA <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "Ii", 
              title = "\nLocal Moran's i", 
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))

map_LISA


## ----Create_png_Map_LISA_Income------------------------------------------
png("map_LISA_vit_Income.png")  
map_LISA
dev.off()


## ----Mapping_Local_Morans_LISA_Z-----------------------------------------
# use manual breaks
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
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i Z values", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))
map_LISA_Z


## ----Create_png_Map_LISA_Income_Z----------------------------------------
png("map_LISA_vit_Income_Z.png")  
map_LISA_Z
dev.off()


## ----Mapping_Local_Morans_LISA_P-----------------------------------------
# use manual breaks
map_LISA_P <- tm_shape(income.tracts.t) + 
  tm_polygons(col = "P", 
              title = "\nLocal Moran's i P values",
              breaks = c(0, 0.001, 0.01, 0.05, Inf ),  # 95% p=0.05
              palette = "-BuPu", n = 5,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nLocal Moran's i P values", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .13, .03))

map_LISA_P


## ----Create_png_Map_LISA_Income_P----------------------------------------
png("map_LISA_vit_Income_P.png")  
map_LISA_P
dev.off()


## ----Local_Morans_i_Scatter_Plot-----------------------------------------
moran.plot(income.tracts.t$Income, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", 
           ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, main ="Local Moran's i Plot for Metro Vancouver 2016 $ Income")


## ----Create_png_Moran_scatter_plot---------------------------------------
png("moran.plot_vit_Income.png")  
moran.plot(income.tracts.t$Income, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", 
           ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, main ="Local Moran's i Plot for Metro Vancouver 2016 $ Income")
dev.off()


## ----Create_idw_raster---------------------------------------------------
P.idw <- gstat::idw(PM25AGG ~ 1, spSample, newdata=grd, idp=4)
r       <- raster(P.idw)


## ----Create_predicted_IDW_idp4_map---------------------------------------
map_fp_IDW_PM25_Predicted_idp4 <- tm_shape(r) + 
#  tm_raster(n=10,palette = "-RdBu", # -RdBu, Reds
  tm_raster(n=10,palette = "Reds", # -RdBu, Reds
            title="Predicted PM25 \n(inug/m^3)") + 
  tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) +
  tm_layout(inner.margins = c(.22, .05, .15, .03), title = "IDW Predicted PM2.5 Values\n(idp=4)", title.position = c(0.03, 0.92)) + 
  tm_scale_bar(position = c(0.4, 0.05)) +  # "LEFT", "BOTTOM"
  tm_compass(type= "4star", position=c(0.87, 0.82)) #"RIGHT", "TOP"
map_fp_IDW_PM25_Predicted_idp4


## ----Create_Confidence_Interval_Jacknife_IDW_idp4------------------------
png("map_fp_IDW_PM25_Predicted_idp4.png")  
map_fp_IDW_PM25_Predicted_idp4
dev.off()


## ----Leave_one_out_validation_routine_IDW_idp4---------------------------
IDW.out <- vector(length = length(spSample))
for (i in 1:length(spSample)) {
  IDW.out[i] <- gstat::idw(PM25AGG ~ 1, spSample[-i,], spSample[i,], idp=4)$var1.pred
}


## ----Plot_IDW_idp4_Differences-------------------------------------------
# parameters: maris margin bottom, left, top, right - leave room at top for main title
OP <- par(pty="s", mar=c(4,3,5,0))
#plots the dot values
png("plot_IDW_idp4.png")
plot(IDW.out ~ spSample$PM25AGG, asp=1, xlab="Observed PM25 (ug/m^3)", ylab="Predicted PM25 (ug/m^3)", main = "IDW Differences Between \nObserved and Predicted PM25 values\n(idp=4)", pch=16,
     col=rgb(0,0,0,0.5))
# plots dotted red line
abline(lm(IDW.out ~ spSample$PM25AGG), col="red", lw=2,lty=2)
# plots sold black line 1:1
# try abline 0.-1 (0,1)
#abline(a=0,b=1, col="black")
abline(a=0,b=1, col="black")
dev.off()


## ----Sets_parameters_from_OP_before--------------------------------------
par(OP)

## ----sqrt----------------------------------------------------------------
OP_idp4 <- sqrt(sum((IDW.out - spSample$PM25AGG)^2) / length(spSample))

## ------------------------------------------------------------------------
# Create the interpolated surface
img <- gstat::idw(PM25AGG~1, spSample, newdata=grd, idp=4)
n   <- length(spSample)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

## ------------------------------------------------------------------------
# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(PM25AGG~1, spSample[-i,], newdata=grd, idp=4)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

## ------------------------------------------------------------------------
# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

## ------------------------------------------------------------------------
# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

## ------------------------------------------------------------------------
# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

## ------------------------------------------------------------------------
# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 


## ----Clip_confidence_raster----------------------------------------------
# Clip the confidence raster
r <- raster(img.sig, layer="v")


## ----Plot_the_map_IDW_idp4_CI--------------------------------------------
# Plot the map
map_fp_IDW_PM25_ConfInt_idp4 <- tm_shape(r) + tm_raster(n=7,title="95% CI \n(ug/m^3)") +
  tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_layout(inner.margins = c(.22, .05, .15, .03), title = "IDW Confidence Interval of PM2.5 Values\n(idp=4)", title.position = c(0.03, 0.92)) + 
  tm_scale_bar(position = c(0.4, 0.05)) +  # "LEFT", "BOTTOM"
  tm_compass(type= "4star", position=c(0.87, 0.82)) #"RIGHT", "TOP"
map_fp_IDW_PM25_ConfInt_idp4


## ----Png_Map_of_Confidence_Interval_Jacknife_IDW_idp4--------------------
png("map_fp_IDW_PM25_ConfInt_idp4.png")  
map_fp_IDW_PM25_ConfInt_idp4
dev.off()


## ----img_sig$v-----------------------------------------------------------
img.sig$v
write.csv(img.sig$v, "csv_fp_IDW_PM25_ConfInt_idp4.csv", row.names = FALSE)

## ----Create_idw_raster---------------------------------------------------
P.idw <- gstat::idw(PM25AGG ~ 1, spSample, newdata=grd, idp=4)
r       <- raster(P.idw)
summary(r)

###########################################################################


## ----Extract_incometracts_and_raster_to_create_new_object----------------
pm.income.poly <- raster::extract(r, income.tracts.t, fun=mean, sp=TRUE)
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
# PM25.mean
# Min.   :0.1000
# 1st Qu.:0.8132
# Median :1.7044
# Mean   :1.9480  
# 3rd Qu.:3.2503 
# Max.   :5.4923  



## ----Head_columns_Income_PM25.mean---------------------------------------
head(pm.income.poly$Income)
head(pm.income.poly$PM25.mean)


## ----Class_columns_Income_PM25.mean--------------------------------------
#Let's say your dataset with both PM2.5 and Income are stored in a dataset called pm.income.poly.
class(pm.income.poly$PM25.mean) # [1] "numeric"
class(pm.income.poly$Income)
nrow(pm25.spatial) # 57870


## ----Summary_pm.income.poly_columns--------------------------------------
summary(pm.income.poly$Income)
summary(pm.income.poly$PM25.mean)
summary(pm.income.poly)


###########
###########
###########

## USE THIS *********
## ----Plot_data_columns_Income_PM25---------------------------------------
plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income & Predicted PM2.5")
png("pm.income.poly_Income_PM25.mean.png")
plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income & Predicted PM2.5")
dev.off()

## DON"T USE THIS
## ----Plot_data_columns_PM25_Income---------------------------------------
# Now plot the data again
## plot(pm.income.poly$PM25.mean~pm.income.poly$Income, main = "Plot of Predicted PM2.5 & Income")
## png("PM25.mean_pm.income.poly_Income.png")
## plot(pm.income.poly$PM25.mean~pm.income.poly$Income, main = "Plot of Predicted PM2.5 & Income")
## dev.off()

## USE THIS *********
## ----Regression_model_1--------------------------------------------------
#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model.1 <- lm(pm.income.poly$Income~pm.income.poly$PM25.mean)
# Call:
#   lm(formula = pm.income.poly$Income ~ pm.income.poly$PM25.mean)
# 
# Coefficients:
#   (Intercept)  pm.income.poly$PM25.mean  
# 32020.9                     802.1  

#plot(lm.model.1, main = "Plot Income ~ PM25.mean")
#Add the regression model to the plot you created
#abline(lm.model.1)
### Hit <Return> to see next plot: 

## DON"T USE THIS
## ----Regression_model----------------------------------------------------
## lm.model <- lm(pm.income.poly$PM25.mean~pm.income.poly$Income)
## plot(lm.model, main = "PM25.mean ~ Plot Income")
#Add the regression model to the plot you created
#abline(lm.model)
### Hit <Return> to see next plot: 


## ------------------------------------------------------------------------
summary(lm.model.1)

# Call:
  #   lm(formula = pm.income.poly$Income ~ pm.income.poly$PM25.mean)
# 
# Residuals:
  #   Min     1Q Median     3Q    Max 
# -22887  -6641   -774   6294  45011 
# 
# Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               32020.9      264.5 121.049  < 2e-16 ***
  #   pm.income.poly$PM25.mean    802.1      111.6   7.186 8.25e-13 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8585 on 3246 degrees of freedom
# Multiple R-squared:  0.01566,	Adjusted R-squared:  0.01536 
# F-statistic: 51.64 on 1 and 3246 DF,  p-value: 8.247e-13


plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income & Predicted PM2.5")
abline(lm.model.1)
png("pm.income.poly_Income_PM25.mean.png")
plot(pm.income.poly$Income~pm.income.poly$PM25.mean, main = "Plot of Income & Predicted PM2.5")
abline(lm.model.1)
dev.off()

## DON"T USE THIS
## ------------------------------------------------------------------------
#summary(lm.model)

## DON"T USE THIS
## ----Plot_data_columns_PM25_Income_2-------------------------------------
# Now plot the data again
## plot(pm.income.poly$PM25.mean~pm.income.poly$Income, main = "Plot of Predicted PM2.5 & Income")
## png("PM25.mean_pm.income.poly_Income.png")
## plot(pm.income.poly$PM25.mean~pm.income.poly$Income, main = "Plot of Predicted PM2.5 & Income")
## dev.off()


## ----Get_residuals_from_model1-------------------------------------------
model.1.resids <- as.data.frame(residuals.lm(lm.model.1))

## DON"T USE THIS
## ----Get_residuals_from_model--------------------------------------------
## model.resids <- as.data.frame(residuals.lm(lm.model))


## ----Add_residuals1_to_spatialpolygondataframe---------------------------
pm.income.poly$residuals1 <- residuals.lm(lm.model.1)

## DON"T USE THIS
## ----Add_residuals_to_spatialpolygondataframe----------------------------
## pm.income.poly$residuals <- residuals.lm(lm.model)


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
  tm_shape(spSample) +
  tm_dots(col="PM25AGG", palette = "Reds", n=5,
          title="Sampled PM2.5 \n(ug/m^3)", size=0.08) + 
  tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
  tm_layout(inner.margins = c(.12, .17, .15, .03), title = "Metro Vancouver 2016 Census Tracts\nMedian $ Income Regression Residuals1", title.position = c("LEFT", "TOP")) + 
  tm_scale_bar(position = c(0.4, 0.05)) +  
  tm_compass(type= "4star", position=c("RIGHT", "TOP")) 
# Use "fisher" instead of "jenks" for larger data sets

map_pm25_vit.data_regress_residuals1

#create png
png("map_pm25_vit.data_regress_residuals1.png")
map_pm25_vit.data_regress_residuals1
dev.off()

## DON"T USE THIS
## ----Map_residuals_PM25_data_with_sample---------------------------------
## map_pm25_vit.data_regress_residuals <- tm_shape(pm.income.poly) + 
##   tm_polygons(col = "residuals", 
##               title = "Regression Residuals", 
##               style = "fisher", 
##               palette = "viridis", n = 6,
##               border.col = "grey", 
##               border.alpha = 0.05) +
##   tm_shape(spSample) +
##   tm_dots(col="PM25AGG", palette = "Reds", n=5, 
##           title="Sampled PM2.5 \n(ug/m^3)", size=0.08) + 
##   tm_legend(legend.outside=FALSE, position = c(0.01, 0.01)) + 
##   tm_layout(inner.margins = c(.12, .17, .15, .03), title = "Metro Vancouver 2016 Census Tracts\nMedian $ Income Regression Residuals", title.position = c("LEFT", "TOP")) + 
##   tm_scale_bar(position = c(0.4, 0.05)) +  
##   tm_compass(type= "4star", position=c("RIGHT", "TOP"))
# Use "fisher" instead of "jenks" for larger data sets

## map_pm25_vit.data_regress_residuals

#create png
## png("map_pm25_vit.data_regress_residuals.png")
## map_pm25_vit.data_regress_residuals
## dev.off()


## ----After_regression_info_pm.income.poly--------------------------------
summary(pm.income.poly)
pm.income.poly


## ----Defining_Queen_Neighbours_after_regression--------------------------
vit.nb <- poly2nb(pm.income.poly)
vit.net <- nb2lines(vit.nb, coords=coordinates(pm.income.poly))


## ----Map_Queen_Neighbours_default_weight_scheme_after_regression---------
map_vit.net <- tm_shape(pm.income.poly) + tm_borders(col='lightgrey') + 
  tm_shape(vit.net) + tm_lines(col='red') +
  # add scale bar - first value is TOP, second value is BOTTOM
  tm_scale_bar(width = 0.22, position = c("RIGHT", "BOTTOM")) + 
  # add compass
  tm_compass(position = c("RIGHT", "TOP")) + 
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income\nQueen Neighbours", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .08, .03))
map_vit.net


## ----Create_png_of_Median_Income_vitnet_Queen_Neighbour------------------
png("map_vit.net_Income_QueenN_afterRegression.png")  
map_vit.net
dev.off()


## ----Creating_Neighbourhood_Weights_Matrix_after_regression--------------
vit.lw <- nb2listw(vit.nb, zero.policy = TRUE, style = "W")
print.listw(vit.lw, zero.policy = TRUE)


## ----Calculate_Lag_Means_after_regression--------------------------------
pm.income.poly$IncLagMeans = lag.listw(vit.lw, pm.income.poly$residuals1, zero.policy = TRUE)


## ----Mapping_Lagged_Means_after_regression-------------------------------
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
######
## Use "fisher" instead of "jenks" for larger data sets


## ----Create_png_map_LagMean_Median_Income_CRDnet2_after_regression-------
png("map_LagMean_vit_Income_afterRegression.png")  
map_LagMean
dev.off()


## ----Morans_I_test_after_regression--------------------------------------
mi <- moran.test(pm.income.poly$residuals1, vit.lw, zero.policy = TRUE)
mi


## ----Morans_range_after_regression---------------------------------------
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vit.lw)   # [1] -0.8129663  1.0482653


## ----Calculate_z_value_after_regression----------------------------------
mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]
z <- (mI - eI) / sqrt(var)


## ----Create_data_table_after_regression----------------------------------
data.for.table3 = data.frame(mI, eI, var, z)
data.for.table3


## ----Write_csv_file_with_Global_Morans_results_for_I_E_Var_Z_P_after_regression----
write.csv(data.for.table3, "moransGlobal_vit_Income_afterRegression.csv", row.names = FALSE)


## ----Lisa_Test_afterRegression-------------------------------------------
lisa.test <- localmoran(pm.income.poly$residuals1, vit.lw)

## ------------------------------------------------------------------------
pm.income.poly$Ii <- lisa.test[,1]
pm.income.poly$E.Ii<- lisa.test[,2]
pm.income.poly$Var.Ii<- lisa.test[,3]
pm.income.poly$Z.Ii<- lisa.test[,4]
pm.income.poly$P<- lisa.test[,5]

## ------------------------------------------------------------------------
# if z value is less than 95% significance level, value is FALSE
pm.income.poly$Z.Ho <- pm.income.poly$Z.Ii < 1.96
# if Ii < 0, value is negative
pm.income.poly$Ii.Neg <- pm.income.poly$Ii < 0
# P-value is one-tailed, need to multiply p-value *2 to correspond to 95% 2 -tailed if P *2>0.05 - higher chance of wrongly deciding significance
pm.income.poly$Po <- (pm.income.poly$P * 2) > 0.05
pm.income.poly$Po


## ----Mapping_Local_Morans_LISA_after_regression--------------------------
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

####
## Use "fisher" instead of "jenks" for larger data sets


## ----Create_png_Map_LISA_Income_after_regression-------------------------
png("map_LISA_vit_Income_afterRegression.png")  
map_LISA
dev.off()


## ----Map_Lisa_Z_after_regression-----------------------------------------
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


## ----Create_png_Map_LISA_Z_Income_after_regression-----------------------
png("map_LISA_vit_Income_Z_afterRegression.png")  
map_LISA_Z
dev.off()


## ----Map_Lisa_P_after_Regression-----------------------------------------
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


## ----Create_png_Map_LISA_Income_P_after_Regression-----------------------
png("map_LISA_vit_Income_P_afterRegression.png")  
map_LISA_P
dev.off()


## ----Local_Morans_i_Scatter_Plot_after_Regression------------------------
moran.plot(pm.income.poly$residuals1, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, main ="Local Moran's i Residuals Plot \nMetro Vancouver 2016 $ Income (after Regression)")


## ----Create_png_Moran_scatter_plot_after_Regression----------------------
png("moran.plot_vit_Income_afterRegression.png")  
moran.plot(pm.income.poly$residuals1, vit.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Median 2016 $ Income", ylab="Spatially Lagged Mean Median 2016 $ Income", quiet=NULL, main ="Local Moran's i Residuals Plot \nMetro Vancouver 2016 $ Income (after Regression)")
dev.off()


## ----GWR_Geographic_Weighted_Regression----------------------------------
# First add the polygon coordinates to the spatialpolygondataframe.
crs(pm.income.poly)
# Obtain the coordinates using "coordinates" function from the sp library
pm.income.poly.coords <- sp::coordinates(pm.income.poly)

## ----GWR_results---------------------------------------------------------
head(pm.income.poly.coords)

## ----Add_coordinates_back_to_spatialpolygondataframe---------------------
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)
head(pm.income.poly)
names(pm.income.poly)


## ----Determine_bandwidth_for_GWR-----------------------------------------
## Need to Install package spgwr
## this will take a while
GWRbandwidth <- gwr.sel(pm.income.poly$PM25.mean~pm.income.poly$Income, 
                        data=pm.income.poly, coords=cbind(pm.income.poly$X, pm.income.poly$Y),adapt=T) 


## Warning messages:
## 1: In gwr.sel(pm.income.poly$PM25.mean ~ pm.income.poly$Income, data = pm.income.poly,  :
##                 data is Spatial* object, ignoring coords argument
##               2: In optimize(gwr.cv.adapt.f, lower = beta1, upper = beta2, maximum = FALSE,: ##                   NA/Inf replaced by maximum positive value

## ----Perform_GWR_on_two_variables_with_bandwidth_determined--------------
### This will take a looooooong time
gwr.model = gwr(pm.income.poly$PM25.mean~pm.income.poly$Income, 
                data=pm.income.poly, coords=cbind(pm.income.poly$X, pm.income.poly$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#Print the results of the model
gwr.model

## ----GWR_results_SDF-----------------------------------------------------
#Look at the results in detail
results <- as.data.frame(gwr.model$SDF)
head(results)


## ----Add_local_r_square_values_to_map------------------------------------
pm.income.poly$localr <- results$localR2
pm.income.poly$localr
head(pm.income.poly)


## ----Create_choropleth_map_of_r_square_values----------------------------
local.r.square <- pm.income.poly$localr


## ----Map_r_square--------------------------------------------------------
map_gwr_r_square <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "localr", 
              title = "\nGWR r square",
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income GWR r square", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_gwr_r_square

# Use "fisher" instead of "jenks" for larger data sets

png("map_map_gwr_r_square.png")  
map_gwr_r_square
dev.off()


## ----Results_Map_coefficients--------------------------------------------
View(results)
class(results$pm.income.poly.Income)
head(results)
results$pm.income.poly.Income
summary(results$pm.income.poly.Income)


## ----Assign_coefficients-------------------------------------------------
pm.income.poly$coeff <- results$pm.income.poly.Income
head(pm.income.poly)
summary(pm.income.poly$coeff)


## ----Create_choropleth_map_of_coefficients-------------------------------
local.coefficient <- pm.income.poly$coeff
local.coefficient
summary(local.coefficient)


## ----Map_coefficient-----------------------------------------------------
map_gwr_coefficient <- tm_shape(pm.income.poly) + 
  tm_polygons(col = "coeff", 
              title = "\nGWR coefficient",
              style = "fisher", 
              palette = "viridis", n = 6,
              border.col = "grey", 
              border.alpha = 0.05) + 
  tm_compass(position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(width = 0.22, position = c("LEFT", "BOTTOM")) +  
  tm_layout(title = "Metro Vancouver Census Tracts Median 2016 $ Income GWR coefficient", title.position = c("LEFT", "TOP"), inner.margins = c(.08, .03, .17, .03))
map_gwr_coefficient

png("map_map_gwr_coefficient.png")  
map_gwr_coefficient
dev.off()


## ----spSample_info_PPA---------------------------------------------------
head(spSample)
summary(spSample)


## ----Coordinates_added_to_spSample---------------------------------------
spSample$x <- coordinates(spSample)[,1]
spSample$y <- coordinates(spSample)[,2]


## ----Finds_zero_distance_among_points------------------------------------
zd <- zerodist(spSample)


## ----Check_for_and_remove_duplicated_points------------------------------
spSample <- remove.duplicates(spSample)
spSample


## ------------------------------------------------------------------------
spSample.ext <- as.matrix(extent(spSample)) 


## ----Observation_window--------------------------------------------------
window <- as.owin(list(xrange = spSample.ext[1,], yrange = spSample.ext[2,]))


## ----Create_ppp_object_from_spatstat-------------------------------------
spSample.ppp <- ppp(x = spSample$x, y = spSample$y, window = window)
spSample.ppp


## ----Number_quads--------------------------------------------------------
quads <- 10


## ----Count_number_of_quads-----------------------------------------------
qcount <- quadratcount(spSample.ppp, nx = quads, ny = quads)
qcount


## ----Create_dataframe_for_quadcount--------------------------------------
qcount.df <- as.data.frame(qcount)
qcount.df


## ----Count_number_of_quadrats_with_distinct_number_of_points-------------
qcount.df <- plyr::count(qcount.df,'Freq')
qcount.df


## ----Change_column_names_to_xnumber_points_and_frequency_of_quadrats-----
colnames(qcount.df) <- c("x","f")
qcount.df

## ------------------------------------------------------------------------
qcount.df$x
qcount.df$f

## ------------------------------------------------------------------------
X2 <- qcount.df$x ^ 2
X2


## ----sum_f_x2------------------------------------------------------------
sum.f.x2 <- sum(qcount.df$f * qcount.df$x ^ 2)
sum.f.x2


## ----Number_of_cells-----------------------------------------------------
M <- sum(qcount.df$f)
M


## ----Number_dataset_points-----------------------------------------------
N <- sum(qcount.df$x * qcount.df$f)
N


## ----Sum_qcount_fx_2-----------------------------------------------------
sum.fx.2 <- (sum(qcount.df$f * qcount.df$x))^2
sum.fx.2


## ----Var_fx2-------------------------------------------------------------
VAR <- (sum.f.x2 -(sum.fx.2/M))/M-1
VAR


## ----Mean_datapoints-----------------------------------------------------
MEAN <- N / M
MEAN


## ----VMR_datapoints------------------------------------------------------
VMR <- VAR / MEAN
VMR


## ----Chi_square_test_for_random_spatial_pattern--------------------------
chi.square = VMR *(M - 1)
p = 1 - pchisq(chi.square, (M - 1))
p  


## ----Add_NND_Stats_data_objects_4_to_data.frame--------------------------
data.for.table4 = data.frame("qcount", "qcount.df$x", "qcount.df$f", "X2", "n", "nnd", "sum.f.x2", "M", "N", "sum.fx.2", "VAR", "MEAN", "VMR", "p")
data.for.table4


## ----kable_head_datatable4_NND-------------------------------------------
# makes html table
kable(head(data.for.table4))
write.csv(data.for.table4, "NND.data.for.table4.csv", row.names = FALSE)


## ----Calculate_nearestNeighbour------------------------------------------
nearestNeighbour <- nndist(spSample.ppp)
nearestNeighbour


## ----Convert_nearestNeighbor_object_into_dataframe-----------------------
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
nearestNeighbour


## ----Nearest_Neighbour_info----------------------------------------------
class(nearestNeighbour)
head(nearestNeighbour)


## ----Change_column_name_to_Distance--------------------------------------
colnames(nearestNeighbour) = "Distance"
head(nearestNeighbour)


## ----nrow_nearestNeighbour-----------------------------------------------
n <- nrow(nearestNeighbour)
n  


## ----nnd_nearestNeighbour------------------------------------------------
nnd <- sum(nearestNeighbour$Distance) / n
nnd 


## ----Calcuate_area-------------------------------------------------------
studyArea <- gArea(spgeom = income.tracts.t, byid = FALSE)
studyArea


## ----Calculate_Density---------------------------------------------------
pointDensity <- n / studyArea
pointDensity 


## ----r_nnd---------------------------------------------------------------
r.nnd <- 1/(2*sqrt(pointDensity))
r.nnd


## ----d_nnd---------------------------------------------------------------
d.nnd <- 1.07453 / pointDensity^(1/2)
d.nnd


## ----R_nnd---------------------------------------------------------------
R <- nnd / r.nnd
R 


## ----SE_nnd--------------------------------------------------------------
SE.NND <- 0.26136 / (n * pointDensity)^(1/2)
SE.NND  


## ----z_nnd---------------------------------------------------------------
z <-  (nnd - r.nnd) / SE.NND
z  


## ----Add_NND_Stats_data_objects_5_to_data.frame--------------------------
data.for.table5 = data.frame("n", "nnd", "studyArea", "pointDensity", "r.nnd", "d.nnd", "R", "SE.NND", "z")
data.for.table5


## ----kable_head_datatable5_PPA-------------------------------------------
# makes html table
kable(head(data.for.table5))
write.csv(data.for.table5, "PPA.data.for.table5.csv", row.names = FALSE)

## ----Extract_R_Code------------------------------------------------------
## Uncomment code to run after document knits successfully
 #library(knitr)
 #purl("FP_Rmd_Van_PM25_EnvEquity_2019_12_02_home.Rmd")

## ----session_info_packages_used------------------------------------------
#sessionInfo()



















################################################
# Descriptive Stats
################################################
# Descriptive Statistics ########
# Objects of interest:
# income.tracts.t
# income.pm25
# pm25.points.aggregate (before sample subset)
# spSample

# ensure no na values
summary(income.tracts.t)
summary(income.pm25)
summary(pm25.points.aggregate)
summary(spSample)

############
# mean
mean.income.tracts.t.Income <- mean(income.tracts.t$Income)
mean.income.tracts.t.Income # [1] 33868
mean.income.pm25.Income <- mean(income.pm25$Income)
mean.income.pm25.Income # [1] 33854.67

mean.income.pm25.PM25AGG <- mean(income.pm25$PM25AGG)
mean.income.pm25.PM25AGG # [1] 2.13386

mean.pm25.points.aggregate.Income <- mean(pm25.points.aggregate$Income)
mean.pm25.points.aggregate.Income # [1] 34787.16
mean.pm25.points.aggregate.PM25AGG <- mean(pm25.points.aggregate$PM25AGG)
mean.pm25.points.aggregate.PM25AGG # [1] 2.142796

mean.spSample.Income <- mean(spSample$Income)
mean.spSample.Income # [1] 36021.82
mean.spSample.PM25AGG <- mean(spSample$PM25AGG)
mean.spSample.PM25AGG # [1] 1.996

############
# Standard Deviation
sd.income.tracts.t.Income <- sd(income.tracts.t$Income)
sd.income.tracts.t.Income # [1] 8835.894
sd.income.pm25.Income <- sd(income.pm25$Income)
sd.income.pm25.Income # [1] 8840.028

sd.income.pm25.PM25AGG <- sd(income.pm25$PM25AGG)
sd.income.pm25.PM25AGG # [1] 1.509542

sd.pm25.points.aggregate.Income <- sd(pm25.points.aggregate$Income)
sd.pm25.points.aggregate.Income # [1] 9016.321
sd.pm25.points.aggregate.PM25AGG <- sd(pm25.points.aggregate$PM25AGG)
sd.pm25.points.aggregate.PM25AGG # [1] 1.494465
sd.spSample.Income <- sd(spSample$Income)
sd.spSample.Income # [1] 9416.431
sd.spSample.PM25AGG <- sd(spSample$PM25AGG)
sd.spSample.PM25AGG # [1] 1.491316

############
# Mode
# make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)
mode.income.tracts.t.Income <- as.numeric(names(sort(table(income.tracts.t$Income), decreasing = TRUE))[1])
mode.income.tracts.t.Income # [1] 34176











############
#Median
medPop <- median(df_2019$CURRENT_SI, na.rm = TRUE)
medPop

############
#Skewness
skewPop <- skewness(df_2019$CURRENT_SI, na.rm = TRUE)[1]
skewPop

############
#Kurtosis
kurtPop <- kurtosis(df_2019$CURRENT_SI, na.rm = TRUE)[1]
kurtPop

############
#CoV
CoVPop <- (sdPop / meanPop) * 100
CoVPop

############
#Normal distribution test
normPop_PVAL <- shapiro.test(df_2019$CURRENT_SI)$p.value
normPop_PVAL

################################################
################################################

################################################
################################################
# Create Table of Stats
################################################
# Create a table of descriptive stats ########









# Create an object for the labels
Samples <- c("Population", "Summer") 
Samples

# Create an object for the means
Mean <- c(meanPop, meanSummer) 
Mean = round(Mean, 3)
Mean

# Create an object for the standard deviations
StandardDeviation <- c(sdPop, sdSummer) 
StandardDeviation = round(StandardDeviation, 3)
StandardDeviation

# Create an object for the medians
Median <- c(medPop, medSummer) 
Median = round(Median, 3)
Median

# Create an object for the modes
Mode <- c(modePop, modeSummer) 
Mode = round(Mode, 3)
Mode

# Create an object for the skewness
Skewness <- c(skewPop, skewSummer) 
Skewness = round(Skewness, 3)
Skewness

# Create an object for the kurtosis
Kurtosis <- c(kurtPop, kurtSummer) 
Kurtosis = round(Kurtosis, 3)
Kurtosis

# Create an object for the CoV
CoefficientOfVariation <- c(CoVPop, CoVSummer) 
CoefficientOfVariation = round(CoefficientOfVariation, 3)
CoefficientOfVariation

# Create an object for the normality PVALUE
Normality <- c(normPop_PVAL, normSummer_PVAL) 
Normality

# Add Stats data objects to data.frame ########
data.for.table1 = data.frame(Samples, Mean, Median, Mode, StandardDeviation, CoefficientOfVariation)
data.for.table2 = data.frame(Samples, Skewness, Kurtosis, Normality)

data.for.table1
data.for.table2

# Make table 1 with gtable ########
# \n to start new line for captions and titles ########
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (Grob) 
table1
t1Caption <- textGrob("Table 1: BC Wildfire Size (ha) statistics for 2019  \n displaying Measures of Central Tendencies and Dispersion \ncomparing Population (Fire Season Mar 1 - Sep 18) \n to Summer Months (Jul 1 - Aug 31)", gp = gpar(fontsize = 12))
padding <- unit(10, "mm")
t1Caption
table1 <- gtable_add_rows(table1, 						
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)
table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)
table1

table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Table 2: BC Wildfire Size (ha) Statistics for 2019  \n displaying Measures of Relative Position \nComparing Population (Fire Season Mar 1 - Sep 18) \n to Summer Months (Jul 1 - Aug 31)", gp = gpar(fontsize = 12))
padding <- unit(10, "mm")
table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)
table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)
table2

grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png)) ########
png("Output_Table1.png") #Create an object to print the table to
grid.arrange(table1, newpage = TRUE)
dev.off() #Print table

png("Output_Table2.png") #Create an object to print the table to
grid.arrange(table2, newpage = TRUE) #Create table
dev.off()



################################################
################################################

################################################
################################################
# Create Histogram
################################################
# Create and Print a histogram ########
png("Output_Histogram.png")
hist(df_2019$CURRENT_SI, breaks = 30, main = "BC 2019 Fire Size (ha) Frequency Distribution Histogram", xlab = "Fire Size (ha") #Base R style
dev.off()

### histogram of whole year 2019 ########
histogram <- ggplot(df_2019, aes(x = CURRENT_SI)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "gray") + #make histogram with 30 bins, black outline, white fill
  labs(title = "BC 2019 Annual Fire Size (ha) Frequency Distribution", x = "Fire Size (ha)", y = "Frequency") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 800, by = 100)) # set y axis labels to 0 - 700 incrimenting by 100
# , caption = "Figure 2: This distribution histogram shows the frequency of each fire size (in hectares)\n during the annual BC 2019 fire season, from March 1 to September 18."
png("Output_Histogram_ggplot.png")
histogram
dev.off()

### Histogram of Summer ########
histogram_summer <- ggplot(df_2019_summer, aes(x = CURRENT_SI)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "gray") + #make histogram with 30 bins, black outline, white fill
  labs(title = "BC 2019 Summer Fire Size (ha) Frequency Distribution", x = "Fire Size (ha)", y = "Frequency") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 300, by = 50)) # set y axis labels to 0 - 700 incrimenting by 100
#, caption = "Figure 3: This distribution histogram shows the frequency of each fire size (in hectares)\n during the summer 2019 BC fire season, from July 1 to August 31."
png("Output_Histogram_ggplot_summer.png")
histogram_summer
dev.off()

#?geom_

#df_2019

################################################
################################################

################################################
################################################
# Creating Bar Graphs
################################################
# Creating bar graph of whole year ########
df_2019$IGN_Month <- month(df_2019$IGNITION_D, label = TRUE, abbr = TRUE) #create new column with month of ignition

sumMar = sum(subset(df_2019, IGN_Month == "Mar")$CURRENT_SI, na.rm = TRUE) #create new object for March
sumApr = sum(subset(df_2019, IGN_Month == "Apr")$CURRENT_SI, na.rm = TRUE) #create new object for April
sumMay = sum(subset(df_2019, IGN_Month == "May")$CURRENT_SI, na.rm = TRUE) #create new object for May
sumJun = sum(subset(df_2019, IGN_Month == "Jun")$CURRENT_SI, na.rm = TRUE) #create new object for June
sumJul = sum(subset(df_2019, IGN_Month == "Jul")$CURRENT_SI, na.rm = TRUE) #create new object for July
sumAug = sum(subset(df_2019, IGN_Month == "Aug")$CURRENT_SI, na.rm = TRUE) #create new object for August
sumSep = sum(subset(df_2019, IGN_Month == "Sep")$CURRENT_SI, na.rm = TRUE) #create new object for September
months = c("Mar","Apr","May","Jun","Jul", "Aug", "Sep")  #Create labels for the bar graph
months_summer = c("Jul", "Aug")  #Create labels for the bar graph

png("Output_BarGraph.png") #Create an object to print the bar graph 
barplot(c(sumMar, sumApr, sumMay, sumJun, sumJul, sumAug, sumSep), names.arg = months, 
        main = "Total Monthly BC Fire Size (in hectares) for 2019", ylab = "Total Fire Size (ha)", xlab = "Month of Fire Ignition") #Create the bar graph
dev.off() #Print bar graph

png("Output_BarGraph_summer.png") #Create an object to print the bar graph 
barplot(c(sumJul, sumAug), names.arg = months_summer, 
        main = "Total Monthly BC Fire Size (in hectares) for Summer 2019", ylab = "Total Fire Size (ha)", xlab = "Month of Fire Ignition") #Create the bar graph
dev.off() #Print bar graph

# Total Size by Month GGPLOT for Full Year ########
#### lubridate ########
barGraph <- df_2019 %>% #store graph in bargraph variable and pass data frame as first argument in next line
  group_by(IGN_Month) %>% #use data frame and group by month and pass to first argument in next line
  summarize(sumSize = sum(CURRENT_SI, na.rm = TRUE)) %>% #sum up the total fire size for each month and pass to GGplot
  ggplot(aes(x = IGN_Month, y = sumSize)) + #make new GGPLOT with summary as data and month and total fire size as x and y
  geom_bar(stat = "identity", fill="#C0C0C0") + #make bar chart with the Y values from the data (identity)
  labs(title = "Monthly BC Wildfire Size Totals (ha) in 2019", x = "Month of Fire Ignition", y = "Total Fire Size (ha)") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) #set title to center and bold
barGraph
#, caption = "Figure 4: Total Monthly Fire Size (in hectares) for BC in 2019, from March 1 to September 18"
png("Output_BarGraph_GG.png")
barGraph
dev.off()

# Total Size by Month GGPLOT for Summer Months ########
df_2019_summer$IGN_Month_Summer <- month(df_2019_summer$IGNITION_D, label = TRUE, abbr = TRUE) #create new column with month of ignition
df_2019_summer$IGN_Month_Summer

barGraph_summer <- df_2019_summer %>% #store graph in bargraph variable and pass data frame as first argument in next line
  group_by(IGN_Month_Summer) %>% #use data frame and group by month and pass to first argument in next line
  summarise(sumSize = sum(CURRENT_SI, na.rm = TRUE)) %>% #sum up the total fire size for each month and pass to GGplot
  ggplot(aes(x = IGN_Month_Summer, y = sumSize)) + #make new GGPLOT with summary as data and month and total fire size as x and y
  geom_bar(stat = "identity", fill="#C0C0C0") + #make bar chart with the Y values from the data (identity)
  labs(title = "Monthly Summer Fire Size Totals (ha) for July & August 2019", x = "Month of Fire Ignition", y = "Total Fire Size (ha)") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) #set title to center and bold
barGraph_summer
# , caption = "Figure 5: Total Monthly Fire Size (in hectares) for BC in Summer 2019, from July 1 to August 31"
png("Output_BarGraph_GG_Summer.png")
barGraph_summer
dev.off()

################################################
################################################


################################################
################################################

################################################
################################################
# Mean Centre Map
################################################
# Mean Centre = mean of x coordinates & mean of y coordinates ########
##   LONGITUDE LATITUDE - order important ########

head(df_2019)

##########################################
meanLONG1 <- mean(df_2019$LONGITUDE)
meanLAT1 <- mean(df_2019$LATITUDE)
mean_centre_coord1 <- c(meanLONG1, meanLAT1)
mean_centre_coord1
# -121.88881   51.83355
##########################################
# TRY adding mean centre
##########################################
# Making Maps with tmap package ########
# Make spatial object (Spatial points dataframe) out of data ########
coords <- df_2019[, c("LONGITUDE", "LATITUDE")] #Store coordinates in new object
crs <- CRS("+init=epsg:4326") #store the coordinate system (CRS) in a new object
# 
firePoints <- SpatialPointsDataFrame(coords = coords, data = df_2019, proj4string = crs) #Make new spatial Points object using coordinates, data, and projection
# 

meanCentre <- data.frame(name = "Mean Centre", long = -121.88881, lat = 51.83355)

meanCentreCoords <- meanCentre[,c("long", "lat")]
meanCentreCoords
mcCRS <- CRS("+init=epsg:4326")

mcPoint <- SpatialPointsDataFrame(coords = meanCentreCoords, data = meanCentre, proj4string = crs)

map_TM_sc <- tm_shape(bc) + #make the main shape
  tm_fill(col = "gray60") +  #fill polygons
  tm_shape(firePoints) +
  tm_symbols(size=0.1, col = "red", alpha = 0.5) + # alpha is transparency lighter 0.3 vs 0.8 darker
  tm_shape(mcPoint) +
  tm_symbols(col = "#000000", alpha = 0.7, shape=7, size=0.8) +
  tm_add_legend(type = "symbol", labels = c("Wildfire Locations", "Mean Centre of all Wildfire Locations"), col = c(adjustcolor( "red", alpha.f = 0.6),adjustcolor( "#000000", alpha.f = 0.8)), shape = c(19,7)) +
  tm_layout(inner.margins = c(.03, .01, .02, .01), title = "BC Wildfire Locations 2019 \n(Mar 1-Sep 18)", title.position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(position = c("RIGHT", "BOTTOM")) +
  tm_compass(position=c("RIGHT", "TOP"))
# 
map_TM_sc
# 
png("TmMap_sc.png")
map_TM_sc
dev.off()
# 

################################################
################################################

################################################
################################################
# Making Maps with tmap package
################################################
# Making Maps with tmap package ########
# Make spatial object (Spatial points dataframe) out of data ########
coords <- df_2019[, c("LONGITUDE", "LATITUDE")] #Store coordinates in new object
crs <- CRS("+init=epsg:4326") #store the coordinate system (CRS) in a new object
# 
firePoints <- SpatialPointsDataFrame(coords = coords, data = df_2019, proj4string = crs) #Make new spatial Points object using coordinates, data, and projection
# 

map_TM_2 <- tm_shape(bc) + #make the main shape
  tm_fill(col = "gray60") +  #fill polygons
  tm_shape(firePoints) +
  tm_symbols(size=0.3, col = "red", alpha = 0.6) + # alpha is transparency lighter 0.3 vs 0.8 darker
  tm_layout(inner.margins = c(.06, .01, .01, .01), title = "BC Wildfire Locations 2019", title.position = c("LEFT", "BOTTOM")) + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position=c("right", "bottom"))
# 
map_TM_sc
# 
png("TmMap_2.png")
map_TM_2
dev.off()
# 

################################################
################################################

################################################
################################################
# different size firePoints Map
################################################
### try adding different size firePoints      ########
## how to scale size ???? ########
## https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html ########
# Store coordinates in new object ########
coords <- df_2019[, c("LONGITUDE", "LATITUDE")] 
# store the coordinate system (CRS) in a new object
crs <- CRS("+init=epsg:4326") 
# 
# Make new spatial Points object using coodinates, data, and projection
firePoints <- SpatialPointsDataFrame(coords = coords, data = df_2019, proj4string = crs) 
# 
map_TM_1 <- tm_shape(bc) + #make the main shape
  tm_fill(col = "gray60") +  #fill polygons
  tm_shape(firePoints) +
  tm_bubbles(size = "CURRENT_SI", col = "red") +
  tm_layout(inner.margins = c(.06, .01, .01, .01), title = "BC Wildfire Locations 2019", title.position = c("LEFT", "BOTTOM")) + 
  tm_legend(position=c("left", "bottom"), frame=FALSE) + #bg.color="gray80"
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position=c("right", "bottom"))

# 
map_TM_1
# 
png("TmMap_1.png")
map_TM_1
dev.off()
# 



##############

