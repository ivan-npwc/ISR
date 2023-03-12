   setwd( "C:\\Users\\usato\\Documents\\YandexDisk\\CURRENT WORK\\ISR-SSLDIET")
	
	
	library(dplyr)
    library(sf)	
	library(raster)	
    library(ggmap)
	library(spatialEco)
	library(marmap)
	library(tidyr)
	library(ggsn)	
    library(rgdal) 
	
	crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
    Sys.setlocale("LC_ALL","Russian_Russia.20866")
	radius=15 # Buffer radius by  miles 
	cellsize= 1 # cellsize in miles, in fact=1 mile
	
	bufer=radius*2 * 1852# convert miles to meters 
	clsz= cellsize*2* 1852
    dpthLimit= -15
######################################
batyFilterGrid=function(){
    IDover=read.csv("IDover.csv")
	 SSLsite =read.csv("tblsIN/SSLsites.csv")
	 SSLsite=SSLsite[SSLsite$IsDiet==TRUE,]
     sites <- st_as_sf(SSLsite, coords = c("declong", "declat"), crs = 4326) %>% st_transform(3857)
     circles <- st_buffer(sites, dist = bufer)                                                                   # Buffer xxx miles circles around the sites
     ###########################################################################################                 # SpatialPolygonsDataFrame grid over circles for each site 
   grid = circles %>% st_make_grid(what = "polygons",cellsize = c(clsz,clsz)) 
   gridAll=as_Spatial(grid)
   gridAll =spTransform(gridAll,crs)
   gridAll$id=c(1:length(gridAll))
   
   PolOvrCrlOcn=gridAll[gridAll$id %in% IDover[,1],]

 # b = getNOAA.bathy(lon1 = 135, lon2 = 175, lat1 = 40, lat2 = 65, 
  #                resolution = 1)
#   a=fortify(b)
#    coords <- data.frame(lon=a$x,lat= a$y)      
 #    data   <- data.frame(z=a$z)   
 #    Points <- SpatialPointsDataFrame(coords = coords,
 #                                    data = data, 
 #                                    proj4string =  crs)
#	 baty =spTransform(Points,crs) 
	 #################################################
#	bt= point.in.poly(baty, gridAll)
#	bt1=data.frame(bt)
#	bt10 =bt1 %>% filter(is.na(id)==F) %>% group_by(id)  %>% summarize (depth=mean(z))  %>% filter (depth < dpthLimit)
#	polOverOcean=gridAll[gridAll$id %in% bt10$id,]
#	
 #
 #  crcls=as_Spatial(circles)
 #  crcls =spTransform(crcls,crs)
# 
#    GrdP = circles %>% st_make_grid(what = "centers",cellsize = c(clsz,clsz)) 
#	GrdP=as_Spatial(GrdP)
#	GrdP =spTransform(GrdP,crs)
#	grOverCrcl = point.in.poly(GrdP,crcls)
#		grOverCrcl1=data.frame(grOverCrcl)
#		pt.ids=unique(grOverCrcl1$pt.ids[is.na(grOverCrcl1$id)==F])
#		
#	    PolOvrCrlOcn=polOverOcean[polOverOcean$id %in% pt.ids,]
#	 	ID=PolOvrCrlOcn$id
#		write.csv(ID, "IDover.csv", row.names=F)
	
}

GetCellCtch=function(){	                                                                                        # catch insite cells
#	posOverGr=read.csv("tblsIN/PosOverGr.csv")
#    posOverGr$VesDate=paste0(posOverGr$id_ves,"_",substr(posOverGr$datetime,1,10))
#	pos=read.csv("tblsIN/pos_2000_2010.csv")
#	pos=pos %>% filter(latitude>0) %>% filter(longitude>0)
#	pos$VesDate=paste0(pos$id_ves,"_",pos$date)
#	pos$time=substr(pos$datetime,12,20)
#	pos=pos %>% filter(time !="12:00:00")# we do NOT use ssd position ->> it is not catch position 
	
	
#	catch=read.csv("tblsIN/catch_2000_2010_dpth150_ImportantFood.csv")  
#	catch$VesDate=catch$Ves_Date

	############################################## 
#	head(catch)
#	head(pos)
#	head(posOverGr)
	
#	ctch= catch %>% group_by(VesDate, fish) %>% summarize(catch=sum(catch_volume))                                     # sum tottal catch by vesel on 1 day
#	posFltr=pos %>% filter (VesDate %in% ctch$VesDate)  %>% filter (VesDate %in% posOverGr$VesDate)              #filter POS only with catch AND insite 15 mile area site (posOverGr)
#	posFltrCtch=left_join(posFltr,ctch,by="VesDate")                                                             #found CATCH for all points insite 15 miles area site  (some ships pass through 15 miles area=> also included)
#	
#	ps=pos %>% filter(VesDate %in% posFltrCtch$VesDate) %>% group_by(VesDate) %>% summarize(CountCoord=n())      # found TOTAL POS coord count 
#	PosCatch= left_join(posFltrCtch,ps,by="VesDate")                                                             # joint POS insite 15 mile area WITH   POS count coord                                                         
#	PosCatch$prCatch= PosCatch$catch/PosCatch$CountCoord                                                         # GET catch proportion  for every coords based on proportion pos coord
#	

PosCatch=read.csv("PosCatch.csv")

	 coordsC <- data.frame(lon=PosCatch$longitude,lat= PosCatch$latitude)      
     dataC   <- data.frame(VesDate=PosCatch$VesDate,prCatch=PosCatch$prCatch,fish=PosCatch$fish)   
     PointsC <- SpatialPointsDataFrame(coords = coordsC,
                                     data = dataC, 
                                     proj4string =  crs)
	 PointsCatch =spTransform(PointsC,crs)                                                                       # PointsCatch it is coord with catch on every coords
 ###############################################################################################  
     pip=point.in.poly(PointsCatch,PolOvrCrlOcn)                                                                     # FOUND CATCH POS INSITE GRIDS SITE
	 pip1=as_tibble(pip)
     pip2=pip1[is.na(pip1$id)==F,]
     pip3=pip2 %>% group_by(id) %>% summarize (catch=sum(prCatch)) 
  PolOvrCrlOcn$cacth=0
  for (i in 1:length(PolOvrCrlOcn)) {
  id=PolOvrCrlOcn$id[i]
  ctch=pip3$catch[pip3$id==id]
   if (length(ctch)==0){ctch=0}
  PolOvrCrlOcn$cacth[i]=ctch
  }



#ctch=CellCtch %>% group_by(cell,year, mnth,season,site,ID) %>% summarize(catch=sum(catch)) %>% filter (catch>1)
#frcell=  ctch %>% group_by(cell,ID,site) %>% summarize(n=n()) 

     
#	 for (i in 1:length(pip2$cell)) {
#	 pip2$ID[i]=strsplit(pip2$cell[i],"_")[[1]][1]
#      }
#	pip2$area[pip2$area=="WBS"]="KC"
#	pip2$area[pip2$area=="CI"]="KC"
#	pip2$area[pip2$area=="KAMCH"]="KC"
#	pip2$area[pip2$area=="KURIL"]="KUR"
#	pip2$area[pip2$area=="NPSO"]="OKH"

#    CellCtch <<- pip2  %>% group_by(area,site,cell,year,mnth,ID,season,fish)%>%  summarize (catch =sum(catch))                                                                           # CellCtch it is catch insite cells 
#    pip2=NULL
#	return(CellCtch)
	  }
#	  GetCellCtch()
INFO=function(){	
    
      limit=quantile(PolOvrCrlOcn$cacth, 0.975)
	  PolOvrCrlOcn$CtchNorm=PolOvrCrlOcn$cacth - limit
	  PolOvrCrlOcn1=PolOvrCrlOcn[ PolOvrCrlOcn$CtchNorm>0,]
	  
	  
	  hist(PolOvrCrlOcn1$CtchNorm)
	#  PolOvrCrlOcn1$logCtch=log(PolOvrCrlOcn1$CtchNorm)
	#  hist(PolOvrCrlOcn1$logCtch)
	  
	#PolOvrCrlOcn2=PolOvrCrlOcn1[PolOvrCrlOcn1$logCtch>median(PolOvrCrlOcn1$logCtch),]
	
	
	
	
	
	
	
	
	
	
	
	
	 ctch=CellCtch %>% group_by(cell) %>% summarize(catch=sum(catch))
	  length(ctch$cell) # the tottal unique cells with fishing 
	 ctch=CellCtch %>% group_by(cell,year, mnth) %>% summarize(catch=sum(catch))  
	  summary(ctch$catch)  # summary fising per month
	  IQR(ctch$catch)      # summary fising per month
     ctch=CellCtch %>% group_by(cell,year, mnth,season,site,ID) %>% summarize(catch=sum(catch)) %>% filter (catch>1)
	  length(unique(ctch$cell))  # number uniq cell with catch more 1
	  
	 ctch %>% group_by(cell) %>% summarize(n=n()) %>% summary(n)   # summary uniq cell with catch more 1 tonn per month
	 ctch %>% group_by(cell) %>% summarize(n=n()) %>% summarize(IQR=IQR(n)) # IQR
	 
	 frcell=  ctch %>% group_by(cell,ID,site) %>% summarize(n=n()) 
	 lmt= quantile(frcell$n,0.90)  #us 0.9 quantile for filter critical area
    b= frcell %>% group_by(ID,site) %>% summarize(n=n()) %>%arrange(desc(ID)) # the 1 collumn table 3
	a= frcell %>% group_by(ID,site) %>% filter (n>lmt) %>% summarize(n=n()) %>% arrange(desc(ID)) # the 3 collumn table 3 and et all
	    
      CellCtch %>% group_by(area) %>% summarize(catch=sum(catch))                                            # catch per AREA 
	   IQR(CellCtch$catch)
	   CellCtch %>% group_by(fish) %>% summarize(catch=sum(catch)) %>% arrange(desc(catch))
	 
	#####
	criticalCell= frcell$cell[frcell$n>lmt]  
	
ctch=CellCtch %>% filter (cell  %in% criticalCell) %>%  group_by(site,fish) %>% summarize(catch=sum(catch))  %>% arrange(desc(catch))
ctch1=ctch %>% spread(fish, catch)
write.csv(ctch1,"Critical_ctch1.csv", row.names=F)
	
 
	}

map=function(){

  BoarderKZ = readOGR("C:\\Users\\usato\\Documents\\YandexDisk\\CURRENT WORK\\ISR-SSLDIET\\map\\Maping boarder KZ\\KZ_border.shp")
  BoarderKZ=BoarderKZ[BoarderKZ$FID==1,]
  BoarderKZ=data.frame(coordinates(BoarderKZ))
  names(BoarderKZ)=c("lon","lat")
  BoarderKZ$group=1
KZ= BoarderKZ %>% st_as_sf(coords = c("lon", "lat"), agr = "constant", crs = 4326) %>%
      st_transform(3857) %>%
     group_by(group) %>%
      summarise(do_union = FALSE) %>%
      st_cast("LINESTRING")
###############################
 grd= st_as_sf(PolOvrCrlOcn1, coords = c("lat", "lon"), crs = 4326) %>% st_transform(3857)
 
 

 #  bbox = c(left = 150,  right =164 ,bottom =45, top =60) #legend
 # bbox = c(left = 141,  right =164 ,bottom =42, top =61) #KURILLY SIMUSHIR
  # bbox = c(left = 154.2,  right =157.2 ,bottom =49.24, top =50.9) #ONEKOTAN, ANZIFER
  bbox = c(left = 161,  right =165 ,bottom =58, top =59.5) #KARAGINSKY
 #  bbox = c(left = 158,  right =161 ,bottom =52, top =54) #KEKURNY
   # bbox = c(left = 160,  right =162 ,bottom =54, top =55) #KOZLOVA
  #  bbox = c(left = 149.8,  right =153 ,bottom =45.5, top =47.6) #SIMUSHIR
#	 bbox = c(left = 152.5,  right =154 ,bottom =47.5, top =48.5) # MATUA
  stamenmap=get_stamenmap(bbox, maptype = "terrain-background",crop = TRUE, zoom=9)
########################################################
	
	ggmap_bbox <- function(map) {
                 if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
                    
                       map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
                       bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
                      attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
                      attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
                      attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
                      attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
                      map
}
	
   map <- ggmap_bbox(stamenmap)	

	ggmap(map) + 
          coord_sf(crs = st_crs(3857))+
		   geom_sf(data = grd,color = "red",inherit.aes = F,alpha=0.1)+
		  geom_sf(data = circles,color = "green",inherit.aes = F,alpha=0.1)
		  
		  
		  
		
		  geom_sf(data = AreaFood,color = "red",inherit.aes = F)+
          geom_sf(data = circles,color = "green",inherit.aes = F,alpha=0.1)+
        geom_sf(data = grd,color = "green",inherit.aes = F,alpha=0.1)+		 
		  geom_sf(data = KZ,color = "red",inherit.aes = F,alpha=0.4,size=3)+		 
		   geom_sf(data = sites,color = "green",inherit.aes = F,size=3)+
		   xlab("Долгота")+ ylab("Широта")	
	
		  ggsave("AreaFood_Kozlova.png")
		  
	
		  
		 
		   }
###

