library(tidync)
library(ncmeta)
library(tidyr)


#=============================================
#--- Prepare conversion table
# -> safran_classes.csv / safran_classes.txt
#=============================================

path_data_allslopes = "/Volumes/infogeo/Meteo_France/SAFRAN_montagne-CROCUS_2019/alp_allslopes"
path_data_allslopes = "/Volumes/ISA-RESEARCH/alp_allslopes"

# test if path is valid
length(list.files(path_data_allslopes)) > 0

y = 2017 #(max 2017)

pro_file = paste0(path_data_allslopes, "/reanalysis/pro/PRO_",as.character(y),"080106_",as.character(y+1),"080106.nc")

src <- tidync(pro_file)
print(src)

# tableau des 4471 indiv. et leurs attributs ZS, aspect, slope, massif_num, longitude, latitude
(safran_classes <- src %>% activate("D0") %>% hyper_tibble() )
write.table(data.frame(safran_classes), file = "safran_classes.txt", sep="\t", quote=FALSE, row.names = FALSE)

safran_classes[which(safran_classes$massif_num == 13), "Number_of_points"]

# test read the snow data with filters
data_y <- src %>% activate("D0,D3") %>% hyper_tibble(select_var = "DSN_T_ISBA")

head(data_y)

datsel = data_y[which(data_y$Number_of_points==2279),]

library(ggplot2)

ggplot(datsel, aes(x = time, y = DSN_T_ISBA)) +
  geom_bar(stat = "identity") +
  geom_smooth(method = "loess", color = "red", span = 0.1)



#=============================================
#--- Prepare raster zaa safran classes 25m
# -> tab_alti.csv, tab_slope.csv, tab_aspect.csv, tab_massif.csv
# -> safran_classes_complet.csv
# -> safran_raster_NoP.img
# -> safran_raster_stack.img (missing)
#=============================================
safran_classes <- read.table("/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_classes.txt", h=T, sep="\t")

# raster des 4471 indiv.
library(raster)
mnt25m <- raster("/Volumes/ISA-RESEARCH/_DATA/zaa_mnt/mntAlpes_25m.tif")
crocus_alti <- cut(mnt25m, breaks = c(0, seq(150, 5000, by = 300)) , include.lowest = TRUE)
tab_alti = data.frame(cat_raster_alti = 1:17, alti_max = c(0, seq(150, 5000, by = 300))[-1] )
tab_alti$alti_min = tab_alti$alti_max-300
tab_alti$alti_min[1] = 0
tab_alti$alti_av = (tab_alti$alti_min+tab_alti$alti_max)/2
tab_alti$safran_ZS = tab_alti$alti_av
tab_alti$safran_ZS[1] = 0

crocus_aspect_alps <- raster("/Volumes/ISA-RESEARCH/_DATA/zaa_CHABLI_DB/_spatialManips/aspect_trigo_alps.tif")
crocus_aspect <- cut(crocus_aspect_alps, breaks = c(-1,0,seq(22.5, 400, by = 45)) , include.lowest = TRUE)
crocus_aspect[which(crocus_aspect[]==10)]=2 # rassemble orientation nord
tab_aspect = data.frame(cat_raster_aspect = 1:9, angle_max = c(-1,0,seq(22.5, 360, by = 45)) [-1] )
tab_aspect$angle_min = tab_aspect$angle_max-45
tab_aspect$angle_av = (tab_aspect$angle_min+tab_aspect$angle_max)/2
tab_aspect[1,] = c(1,-1,-1,-1)
tab_aspect[2,"angle_min"] = 337.5
tab_aspect$safran_aspect = tab_aspect$angle_av

crocus_slope_alps <- raster("/Volumes/ISA-RESEARCH/_DATA/zaa_CHABLI_DB/_spatialManips/slopeAlps.tif")
crocus_slope <- cut(crocus_slope_alps, breaks = c(0, 10, 30, 90) , include.lowest = TRUE)
crocus_slope
tab_slope = data.frame(cat_raster_slope = 1:3, slope_min = c(0, 10, 30),slope_max = c(10, 30, 90) )
tab_slope$slope_av = (tab_slope$slope_min + tab_slope$slope_max)/2
tab_slope$safran_slope = c(0,20,40)

##====
# need to round/attribute aspect to -1 if slope is cat 1 (round to flat when no exposition)
##====
crocus_aspect[which(crocus_slope[]==1)]=1

##====

library(rgdal)
library(raster)
crocus_massif <- readOGR("/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/massifs_shapefiles")
# library(ggplot2)
# map <- ggplot() + geom_polygon(data = crocus_massif, aes(x = long, y = lat), colour = "black", fill = NA)
# shp_df <- broom::tidy(crocus_massif, region = "massif_num")
# cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
# map + geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 4) + theme_void()
# plot(crocus_massif)
crocus_massif_raster <- rasterize(crocus_massif, mnt25m, field="massif_num")
crocus_massif_raster
tab_massif = crocus_massif@data


write.table(tab_alti, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/tab_alti.csv", sep=";", row.names = FALSE, dec=",")
write.table(tab_aspect, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/tab_aspect.csv", sep=";", row.names = FALSE, dec=",")
write.table(tab_slope, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/tab_slope.csv", sep=";", row.names = FALSE, dec=",")
write.table(tab_massif, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/tab_massif.csv", sep=";", row.names = FALSE, dec=",")


crocus_stack <- stack(crocus_alti, crocus_slope, crocus_aspect, crocus_massif_raster)
names(crocus_stack) <- c("altitude", "slope", "aspect", "massif")
writeRaster(crocus_stack, file="/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_raster_stack.img", format = "HFA", overwrite=TRUE)

# crocus_stack = stack("safran_raster_stack.img")

sf2 = merge(safran_classes, tab_alti, by.x="ZS", by.y="safran_ZS")
sf3 = merge(sf2, tab_aspect, by.x="aspect", by.y="safran_aspect")
sf4 = merge(sf3, tab_slope, by.x="slope", by.y="safran_slope")

head(sf4)
dim(sf4)

sf4$NoP_stack = unlist(apply(sf4[,c("cat_raster_alti", "cat_raster_slope", "cat_raster_aspect", "massif_num")], 1, function(x){paste0(x[1:4], collapse="")}))

write.table(sf4, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_classes_complet.csv", sep=";", row.names = FALSE, dec=",")

# sf4 = read.table("safran_classes_complet.csv", sep=";",dec=",", h=T)

corresp <- function(x, na.rm){
  if(sum(is.na(x))==0){
  num_point = sf4[which(((sf4$cat_raster_alti == x[1] & sf4$cat_raster_aspect == x[3]) & sf4$cat_raster_slope==x[2]) & sf4$massif_num==x[4]), "Number_of_points"]
  }else num_point=NA
  if(length(num_point)==0) num_point=as.numeric(paste0(c(9999, x), collapse = ""))
  return(num_point)
}
#
library(doParallel)
beginCluster(n=4, type="SOCK")
cl = getCluster()
clusterExport(cl, "sf4", envir=environment())
NoP = clusterR(crocus_stack, stackApply, args = list(indices=rep(1, 4), fun = corresp, na.rm=FALSE))
endCluster()


writeRaster(NoP, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_raster_NoP.img", format = "HFA", overwrite=TRUE)

# collapse <- function(x, na.rm){as.numeric(paste0(x, collapse = ""))}
#
# library(doParallel)
# beginCluster(n=4, type="SOCK")
# cl = getCluster()
# clusterExport(cl, "sf4", envir=environment())
# NoP_stack = clusterR(crocus_stack, stackApply, args = list(indices=rep(1, 4), fun = collapse, na.rm=FALSE))
# endCluster()

# NoP_stack <- stackApply(crocus_stack, indices=rep(1, 4),fun = collapse, na.rm=TRUE)

# index = which(NoP[]==9999)
# values = extract(crocus_stack, index)

# writeRaster(NoP_stack, file = "/Volumes/ISA-RESEARCH/_DATA/fra_SAFRAN_CROCCUS/safran_raster_NoP_stack.img", format = "HFA", overwrite=TRUE)
