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
	
	require(maps)
    require(mapdata)
    require(sp)
    data("world2HiresMapEnv") 
	
	library(ggOceanMapsData)
    library(ggOceanMaps)
	
	library(geosphere)  
    library(spatstat)
	
	crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
    Sys.setlocale("LC_ALL","Russian_Russia.20866")
	radius=40 # diametr Buffer circles by  miles 
	dpthLimit= -0
	
	cellsize= 0.025*2  # cellsize in miles,	
	bufer=radius * 1852# convert miles to meters 
	clsz= cellsize* 1852

######################################
ctchInCircl=function(){

GetCetchInCrls=function(){

circles=readRDS("circles")
pos=readRDS("pos")

reg= read.csv("tblsIN/id_region-NameRegion.csv")
ctch=read.csv("tblsIN/catch_2000_2010_fishSqu.csv")
ctch=ctch[ctch$id_region %in% reg$id_region,]
pos=pos[pos$VesDate %in% ctch$Ves_Date,]
Years=unique(pos$year)


Pos20WithDublicate=NULL
 for (i in 1:length(circles$site)){ 
site=circles[i,]
  for (y in 1:length(Years)){
   posY=pos[pos$year==Years[y],]
 posIN= point.in.poly(posY,site)
 posIN=posIN[is.na(posIN$site)==F,]
  if (length(posIN$site)>1){
 VesDateIN=unique(posIN$VesDate)
 posOverSite=pos[pos$VesDate %in% VesDateIN,]
 posOverSite$site=site$site
 print(paste0(site$site,"   ", Years[y]))
if (i==1){Pos20WithDublicate=posOverSite}
else {Pos20WithDublicate=rbind(Pos20WithDublicate,posOverSite)}

 }
 }
 }
saveRDS(Pos20WithDublicate,"Pos20WithDublicate")
}
INFO=function(){

  pos=readRDS("Pos20WithDublicate")
  ctch=read.csv("tblsIN/catch_2000_2010_fishSqu.csv")
  ctch$VesDate=ctch$Ves_Date
  Ctchves=unique(ctch$ves)
  posC= pos [pos$id_ves %in% Ctchves,]
 
   ctch$mnth=substr(ctch$date,6,7)
   ctch$season[ctch$mnth %in% c("06","07","08")]="summer"
   ctch$season[!ctch$mnth %in% c("06","07","08")]="winter"
   ctch=as_tibble(ctch)
  
 #siteFlt=  c("KEKURNY CAPE","SHIPUNSKY CAPE","ZHELEZNAYA BAY","KOZLOV CAPE")
#  siteFlt=  c("CHIRINKOTAN","MATUA/POLOGY","CHIRPOY/UDUSHLIVY","ANTSIFEROV/ROOKERY","SHIASHKOTAN/KRASNY","SIMUSHIR/KRASNOVATAYA","BRAT CHIRPOYEV/ROOKERY","ONEKOTAN/KAMEN YASNOY POGODY","RAYKOKE","SIMUSHIR/ARONT","URUP/CHAYKA")
 # siteFlt=  c("ZAVYALOV","YAMSKY ISLS","IONY")
  siteFlt=  c("ARAGINSKY/KRASHENINNIKOV")
#    siteFlt=  c("CHIRINKOTAN","MATUA/POLOGY","CHIRPOY/UDUSHLIVY","ANTSIFEROV/ROOKERY","SHIASHKOTAN/KRASNY","SIMUSHIR/KRASNOVATAYA","BRAT CHIRPOYEV/ROOKERY","ONEKOTAN/KAMEN YASNOY POGODY","RAYKOKE","SIMUSHIR/ARONT","URUP/CHAYKA","KEKURNY CAPE","SHIPUNSKY CAPE","ZHELEZNAYA BAY","KOZLOV CAPE","ZAVYALOV","YAMSKY ISLS","IONY","ARAGINSKY/KRASHENINNIKOV")
   
#  fish=c("минтай","камбала","терпуги","треска","бычки") # kamchatka
#	   fish=c("минтай","терпуги","кальмары прочие") # kurill
#	 fish=c("минтай","сельдь") # okhotsk
     fish=c("минтай","камбала","треска") # Karaga
#   fish=c("минтай","камбала","терпуги","треска","бычки","кальмары прочие","сельдь")# ALL
  ###################################################################################################### 
  #####       table  Quantitative assessment of commercial fisheries in the water areas of rookeries.
  tmp=data.frame(posC[posC$site %in% siteFlt,])  

 #  tmp=posC
 #  tmp %>% group_by(id_ves,site)  %>% summarise(n=n()) %>% group_by(site)%>%summarise(n=n())  # ves per site
 #  tmp %>% group_by(VesDate,site)  %>% summarise(n=n()) %>% group_by(site)%>%summarise(n=n())  # vesDays per site
   
   VesIn= data.frame(VesDate=tmp$VesDate,site=tmp$site)
   VesIn=unique(VesIn)
  ctchIN=left_join(VesIn,ctch,by="VesDate")
  ctchIN= as_tibble(ctchIN[is.na(ctchIN$site)==F,])
   
  #ctchIN %>% group_by(site)  %>%  summarise (catch=sum (catch_volume))  # ctch per site
  

 ctchIN %>% group_by(site,VesDate) %>% summarise(ctch=sum(catch_volume)) %>% group_by(site) %>% summarise(med=median(ctch),qo25=quantile(ctch,0.25),qo75=quantile(ctch,0.75))  %>%
  group_by()%>%
 summarise(Me=median(med),qo25=median(qo25), qo75=median(qo75))
 
 

 

 #####
  VesD=unique(pos$VesDate[pos$site %in% siteFlt])
  ctchF= as_tibble(ctch[ctch$Ves_Date %in% VesD,])
  ctchF$fish[ctchF$fish=="треска ярусная"]="треска"
   fsh=ctchF %>% group_by(fish) %>% summarise (ctch=sum(catch_volume)) %>%arrange(desc(ctch))
   ctchRg=ctchF[ctchF$fish %in% fish,]
  
 
  ##########################################
  tmp=ctchRg %>% group_by(mnth,year,season,fish) %>% summarise (ctch=sum(catch_volume)) %>%arrange(desc(ctch))
 ####################################info
 
 tmp %>% filter(mnth   %in% c("06")) %>%
         group_by(mnth,year) %>% 
		 summarise(ctch=sum(ctch))  %>% 
		   group_by() %>% 
		 summarise(Me=median(ctch), q025=quantile(ctch,0.25),q075=quantile(ctch,0.75))
 


 #############################################################
 
   polock=tmp[tmp$fish=="минтай",]
   NOpolock=tmp[!tmp$fish=="минтай",]
   
    ggplot(tmp, aes(x=mnth, y=ctch, fill=fish)) + 
    geom_boxplot() +
	 theme(axis.text.x=element_text(angle = 0, hjust = 0,size=20))+ #-90
	 xlab("Месяц")+
	 ylab("Вылов на месяц, тонн")+
	# scale_y_continuous(limits=c(0,4200), breaks=seq(0,4200, by = 300))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			  ,legend.position="bottom" 
			   )
  ################################################################################################################################
ctchALL=ctchIN
 ctchALL$reg[ctchALL$site %in% c("KEKURNY CAPE","SHIPUNSKY CAPE","ZHELEZNAYA BAY","KOZLOV CAPE")]="KK"
  ctchALL$reg[ctchALL$site %in% c("ZAVYALOV","YAMSKY ISLS","IONY")]="OKH"
  ctchALL$reg[ctchALL$site %in% c("ARAGINSKY/KRASHENINNIKOV")]="KRG"
   ctchALL$reg[ctchALL$site %in%c("CHIRINKOTAN","MATUA/POLOGY","CHIRPOY/UDUSHLIVY","ANTSIFEROV/ROOKERY","SHIASHKOTAN/KRASNY","SIMUSHIR/KRASNOVATAYA","BRAT CHIRPOYEV/ROOKERY","ONEKOTAN/KAMEN YASNOY POGODY","RAYKOKE","SIMUSHIR/ARONT","URUP/CHAYKA")]="KUR"
 
 ctchALL$catch_depth=0-ctchALL$catch_depth
  ctchALL$catch_depth[ctchALL$catch_depth > -5]=NA
    OperBottom=c("снюрревод","сеть донная","трал р/гл", "трал донный","ярус донный","невод кошельковый")
    Bottom=ctchALL %>% filter (oper %in% OperBottom) 


    ggplot(Bottom, aes(x=season, y=catch_depth,fill=reg)) + 
    geom_boxplot(outlier.size=0.011) +
	 theme(axis.text.x=element_text(angle = 0, hjust = 0,size=20))+ #
	 xlab("Месяц")+
	 ylab("Глубина лова, метров")+
	 scale_y_continuous(limits=c(-600,0), breaks=seq(-600,0, by = 100))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			   
			   ,legend.position="bottom" 
			   )
   
   
 #####################
 
 ctchF1=ctchRg
 ctchF1$catch_depth=0-ctchF1$catch_depth
  ctchF1$catch_depth[ctchF1$catch_depth > -5]=NA
    OperBottom=c("снюрревод","сеть донная","трал р/гл", "трал донный","ярус донный","невод кошельковый")
    Bottom=ctchF1 %>% filter (oper %in% OperBottom) 
	
   ############info
   kruskal.test(catch_depth~season, data=Bottom)
    boxplot (catch_depth~season, data=Bottom)
   boxplot (catch_depth~season, data=Bottom,plot=F)


  Bottom %>% filter (mnth %in% c("06","07","08")) %>%
             group_by()%>%
			 summarise(Me=median(catch_depth,na.rm=T),Q025=quantile(catch_depth,  na.rm=T,0.25),Q075=quantile(catch_depth,  na.rm=T,0.75))






 
   
    ggplot(Bottom, aes(x=mnth, y=catch_depth, fill=fish)) + 
    geom_boxplot(outlier.size=0.011) +
	 theme(axis.text.x=element_text(angle = 0, hjust = 0,size=20))+ #
	 xlab("Месяц")+
	 ylab("Глубина лова, метров")+
	 scale_y_continuous(limits=c(-600,0), breaks=seq(-600,0, by = 100))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			   
			   ,legend.position="bottom" 
			   )
   
   
   geom_smooth(aes(x=as.integer(group),y=variable,color=treatment,fill=treatment),method=loess)

}

exlude=function(){


ctchMore1= ctchF %>% group_by(Ves_Date)  %>% summarise(ctch=sum(catch_volume)) %>%  filter (ctch>1)
write.csv(ctchMore1,"ctchMore1.csv", row.names=F)


pos=read.csv("tblsIN/pos_2000_2010.csv")

ctch=read.csv("tblsIN/catch_2000_2010_fishSqu.csv")



     coords <- data.frame(lon=pos$longitude,lat= pos$latitude)      
     data   <- data.frame(id_ves =pos$id_ves, datetime=pos$datetime,date =pos$date, year=pos$year,VesDate =paste0(pos$id_ves,"_",pos$date))   
     Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                    proj4string =  crs)
	 pos1 =spTransform(Points,crs) 
     saveRDS(pos1,"pos")
	 
	################## 
	 site=read.csv("tblsIN/SSLsites.csv")
     site=site[site$IsDiet==TRUE,]
	 site=site[is.na(site$IsDiet)==F,]
	 
	 coords <- data.frame(lon=site$declong,lat= site$declat)      
     data   <- data.frame(site =site$site, Area=site$Area)   
     Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                    proj4string =  crs)
	 site =spTransform(Points,crs) 
     saveRDS(site,"site")
	 
	 #####
	 
	DVCS=read.csv("tblsIN/DayVesCatchSite.csv")
	DVCS$Catch= DVCS$Catch/1000
	 
	 
	####		   
	ggplot(DVCS, aes(x=region , y=VesDayCount)) + 
    geom_boxplot() +
	 theme(axis.text.x=element_text(angle = 0, hjust = 0,size=20))+ #-90
	 xlab("Регион")+
	 ylab("Число судосуток")+
	# scale_y_continuous(limits=c(0,4200), breaks=seq(0,4200, by = 300))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			  ,legend.position="bottom" 
			   )   
	############# 		   

	 #######################
	 	ggplot(DVCS, aes(x=region , y=Catch)) + 
    geom_boxplot() +
	 theme(axis.text.x=element_text(angle = 0, hjust = 0,size=20))+ #-90
	 xlab("Регион")+
	 ylab("Вылов на лежбище, тысяч тонн")+
	# scale_y_continuous(limits=c(0,4200), breaks=seq(0,4200, by = 300))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			  ,legend.position="bottom" 
			   )   
			   ##########
			   	ggplot(DVCS, aes(x=region , y=CatchPerDayVes)) + 
    geom_boxplot() +
	 theme(axis.text.x=element_text(angle = 0, hjust = 0,size=20))+ #-90
	 xlab("Регион")+
	 ylab("Вылов на судосутки, тонн")+
	# scale_y_continuous(limits=c(0,4200), breaks=seq(0,4200, by = 300))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			  ,legend.position="bottom" 
			   )   

	 

}

}
######################################
batyFilterGrid=function(){
    
	 SSLsite =read.csv("tblsIN/SSLsites.csv")
	 SSLsite=SSLsite[SSLsite$IsDiet==TRUE,]
	 SSLsite=SSLsite[is.na(SSLsite$IsDiet)==F,]
	 
     sites <- st_as_sf(SSLsite, coords = c("declong", "declat"), crs = 4326) %>% st_transform(3857)
     circles <- st_buffer(sites, dist = bufer)    



	 # Buffer xxx miles circles around the sites
     ###########################################################################################                 # SpatialPolygonsDataFrame grid over circles for each site 
   b = getNOAA.bathy(lon1 = 135, lon2 = 175, lat1 = 40, lat2 = 65, 
                  resolution = 1)
   a=fortify(b)
    coords <- data.frame(lon=a$x,lat= a$y)      
     data   <- data.frame(z=a$z)   
     Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                    proj4string =  crs)
	 baty =spTransform(Points,crs) 
	 
	   crcls=as_Spatial(circles)
       crcls =spTransform(crcls,crs)
	 #################################################
	 FeedingArea=NULL
  for (i in 1:length(circles$Site)) {
   grid = circles[i,] %>% st_make_grid(what = "polygons",cellsize = c(clsz,clsz)) 
   gridAll=as_Spatial(grid)
   gridAll =spTransform(gridAll,crs)
   gridAll$id=c(1:length(gridAll)) 
	bt= point.in.poly(baty, gridAll)
	bt1=data.frame(bt)
	bt10 =bt1 %>% filter(is.na(id)==F) %>% group_by(id)  %>% summarize (depth=mean(z))  %>% filter (depth < dpthLimit)
	polOverOcean=gridAll[gridAll$id %in% bt10$id,]
	#########
    GrdP = circles[i,] %>% st_make_grid(what = "centers",cellsize = c(clsz,clsz)) 
	GrdP=as_Spatial(GrdP)
	GrdP =spTransform(GrdP,crs)
	grOverCrcl = point.in.poly(GrdP,crcls)
		grOverCrcl1=data.frame(grOverCrcl)
		pt.ids=unique(grOverCrcl1$pt.ids[is.na(grOverCrcl1$Site)==F])
		
	      grid1=polOverOcean[polOverOcean$id %in% pt.ids,]
		
		 site=rep.int(circles$Site[i],length(grid1))
         area=rep.int(circles$Region[i],length(grid1))
         cel=c(1:length(grid1))
         cell=paste0(circles$id[i],"_",cel)
		
		  grid1$area=area
          grid1$site=site
          grid1$cell= cell		  
	
		  
       if (i==1){FeedingArea=grid1}
       if (i !=1){FeedingArea=rbind(FeedingArea,grid1)}
	print(paste0(circles$Site[i]))
    }
   saveRDS(FeedingArea,"FeedingArea")
	
	}
###################################	
GetCellCtch=function(){	                                                                                        # catch insite cells
	gridAll=readRDS("FeedingArea")
	pos=read.csv("tblsIN/pos_2000_2010.csv")
    catch=read.csv("tblsIN/catch_2000_2010_fishSqu.csv")  #catch_2000_2010_dpth150_ImportantFood
	
	pos=pos %>% filter(latitude>0) %>% filter(longitude>0)
	pos$VesDate=paste0(pos$id_ves,"_",pos$date)
	pos$time=substr(pos$datetime,12,20)
	pos=pos %>% filter(time !="12:00:00")# we do NOT use ssd position ->> it is not catch position 
	catch$VesDate=catch$Ves_Date
		
	############################################## 
	head(catch)
	head(pos)
#	head(posOverGr)
	
#	ctch= catch %>% group_by(VesDate, fish) %>% summarize(catch=sum(catch_volume))                                     # sum tottal catch by vesel on 1 day
#	posFltr=pos %>% filter (VesDate %in% ctch$VesDate)  %>% filter (VesDate %in% pos$VesDate)              #filter POS only with catch AND insite 15 mile area site (posOverGr)
#	posFltrCtch=left_join(posFltr,ctch,by="VesDate")                                                             #found CATCH for all points insite 15 miles area site  (some ships pass through 15 miles area=> also included)
	
#	ps=pos %>% filter(VesDate %in% posFltrCtch$VesDate) %>% group_by(VesDate) %>% summarize(CountCoord=n())      # found TOTAL POS coord count 
#	PosCatch= left_join(posFltrCtch,ps,by="VesDate")                                                             # joint POS insite 15 mile area WITH   POS count coord                                                         
#	PosCatch$prCatch= PosCatch$catch/PosCatch$CountCoord                                                         # GET catch proportion  for every coords based on proportion pos coord
	
#	 coords <- data.frame(lon=PosCatch$longitude,lat= PosCatch$latitude)      
#     data   <- data.frame(VesDate=PosCatch$VesDate,prCatch=PosCatch$prCatch,fish=PosCatch$fish)   
#     Points <- SpatialPointsDataFrame(coords = coords,
 #                                    data = data, 
 #                                    proj4string =  crs)
#	 PointsCatch =spTransform(Points,crs)
 #    PointsCatch$Ctchid=c(1:length(PointsCatch))
#	 saveRDS(PointsCatch,"PointsCatch_ALL")
	 # PointsCatch it is coord with catch on every coords
	###########################################################################
	# SSLsite =read.csv("tblsIN/SSLsites.csv")
	# SSLsite=SSLsite[SSLsite$IsDiet==TRUE,]
    #sites <- st_as_sf(SSLsite, coords = c("declong", "declat"), crs = 4326) %>% st_transform(3857)
    # circles <- st_buffer(sites, dist = bufer)                                                                   # Buffer 15 miles circles around the sites
     ###########################################################################################                 # SpatialPolygonsDataFrame grid over circles for each site 
  #   gridAll=NULL
#	 for (i in 1:length(circles$id)) {
#   grid = circles[i,] %>% st_make_grid(what = "polygons",cellsize = c(clsz,clsz)) 
#   grid1=as_Spatial(grid)
#   site=rep.int(circles$site[i],length(grid1))
#   area=rep.int(circles$Area[i],length(grid1))
#   cel=c(1:length(grid1))
#   cell=paste0(circles$id[i],"_",cel)
#   grid1$area=area
#   grid1$site=site
#   grid1$cell= cell
#       if (i==1){gridAll=grid1}
#       if (i !=1){gridAll=rbind(grid1,gridAll)}
# }
 #plot(circles[1,] %>% st_make_grid(what = "polygons",cellsize = c(clsz,clsz)))
 #  gridAll =spTransform(gridAll,crs)
 ###############################################################################################  
 PointsCatch =readRDS("PointsCatch_ALL")
 gridAll=readRDS("FeedingArea")
 PointsCatch$year=substr(PointsCatch$VesDate,7,10)
 years=unique(PointsCatch$year)
 
  for (i in 1:length(years)) {
   year= years[i]
   PointsCatchYear=PointsCatch[PointsCatch$year==year,]
  
     pip=point.in.poly(PointsCatchYear,gridAll)   # FOUND CATCH POS INSITE GRIDS SITE
	 pip1=as_tibble(pip)
     pip1=pip1[is.na(pip1$area)==F,] # here only we have catch ID
	 pip1$year=substr(pip1$VesDate,7,10)
	 pip1$mnth=substr(pip1$VesDate,12,13)
     pip2=pip1 %>% group_by(area,site,cell,Ctchid,year,mnth,fish) %>% summarize (catch=sum(prCatch)) 
	# for (i in 1:length(pip2$cell)) {
	# pip2$ID[i]=strsplit(pip2$cell[i],"_")[[1]][1]
   #  }
	pip2$ID=1
	  pip2$area[pip2$area=="WBS"]="KC"
	pip2$area[pip2$area=="CI"]="KC"
	pip2$area[pip2$area=="KAMCH"]="KC"
	pip2$area[pip2$area=="KURIL"]="KUR"
	pip2$area[pip2$area=="NPSO"]="OKH"
#	pip2$season[pip2$mnth %in% c("04","05","06","07","08","09")]="summer"
#	pip2$season[pip2$mnth %in% c("10","11","12","01","02","03")]="winter"  
   CellCtch <- pip2  %>% group_by(Ctchid,area,site,cell,year,mnth,ID,fish)%>%  summarize (catch =sum(catch))  
    write.csv(CellCtch,paste0("CellCtch/",year,".csv", row.names=F))# CellCtch it is catch insite cells 
    pip2=NULL
	print(year)
	}
	
	
	fin=NULL	
  lstctch=list.files("CellCtch", full.names=T)	
	 for (i in 1:length(lstctch)) {
	 ctch=read.csv(lstctch[i])
	 fin=rbind(fin,ctch)
	 }
	 write.csv(fin,"CellCtch.csv", row.names=F)
	  }
	#######################################
shipunZeleznaya=function(){
     
	 SZFeedingArea=NULL
	 SSLsite =read.csv("tblsIN/SSLsites.csv")
	 SSLsite=SSLsite[SSLsite$IsDiet==TRUE,]
     sites <- st_as_sf(SSLsite, coords = c("declong", "declat"), crs = 4326) %>% st_transform(3857)
    SZ=sites[sites$site %in% c("SHIPUNSKY CAPE","ZHELEZNAYA BAY"),] 
     SZircles <- st_buffer(SZ, dist = bufer)  
	 
	 SZircles <- SZircles %>% 
     group_by(group = paste(st_intersects(SZircles, SZircles, sparse = T))) %>% 
     summarise(score = sum(id))
	 SZsquare=SZircles %>% st_make_grid(what = "polygons",cellsize = c(clsz,clsz)) 

	 ##############################
	   SZsquare=as_Spatial(SZsquare)
       SZsquare =spTransform(SZsquare,crs)
	   SZsquare$Sqid=c(1:length(SZsquare))
	   
	 ####################
#	  b = getNOAA.bathy(lon1 = 135, lon2 = 175, lat1 = 40, lat2 = 65, 
 #                 resolution = 1)
    a=fortify(b)
    coords <- data.frame(lon=a$x,lat= a$y)      
     data   <- data.frame(z=a$z)   
     Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                    proj4string =  crs)
	 baty =spTransform(Points,crs)
	 baty$idbt=c(1:length(baty))
	 #########################
	 
	bt= point.in.poly(baty, SZsquare)
	bt1=data.frame(bt)
	bt10 =bt1 %>% filter(is.na(Sqid )==F) %>% group_by(Sqid)  %>% summarize (depth=mean(z))  %>% filter (depth < dpthLimit)
	polOverOcean=SZsquare[SZsquare$Sqid %in% bt10$Sqid,]
	#########
       clls=as_Spatial(SZircles)
       clls =spTransform(clls,crs)
	   
    GrdP = SZircles %>% st_make_grid(what = "centers",cellsize = c(clsz,clsz)) 
	GrdP=as_Spatial(GrdP)
	GrdP$Sqid=c(1:length(GrdP))
	GrdP =spTransform(GrdP,crs)
	grOverCrcl = point.in.poly(GrdP,clls)
	
		grOverCrcl1=data.frame(grOverCrcl)
		Sqid=unique(grOverCrcl1$Sqid [is.na(grOverCrcl1$score)==F])
		
	      grid1=polOverOcean[polOverOcean$Sqid %in% Sqid,]
		AreaFoodShipunZeleznaya=length(grid1)

	}
	##################################
criticalCatch=function(){

    CriticalArea=readRDS("CriticalArea")
	CriticalArea= as_Spatial(CriticalArea)
    CriticalArea =spTransform(CriticalArea,crs)
	   
	gridAll=readRDS("FeedingArea")
	posOverGr=read.csv("tblsIN/PosOverGr.csv")
    posOverGr$VesDate=paste0(posOverGr$id_ves,"_",substr(posOverGr$datetime,1,10))
	pos=read.csv("tblsIN/pos_2000_2010.csv")
	pos=pos %>% filter(latitude>0) %>% filter(longitude>0)
	pos$VesDate=paste0(pos$id_ves,"_",pos$date)
	pos$time=substr(pos$datetime,12,20)
	pos=pos %>% filter(time !="12:00:00")# we do NOT use ssd position ->> it is not catch position 
	
	
	catch=read.csv("tblsIN/catch_2000_2010_dpth150_ImportantFood.csv")  #catch_2000_2010_dpth150_ImportantFood
	catch$VesDate=catch$Ves_Date
	
	############################################## 
	head(catch)
	head(pos)
	head(posOverGr)
	
	ctch= catch %>% group_by(VesDate, fish) %>% summarize(catch=sum(catch_volume))                                     # sum tottal catch by vesel on 1 day
	posFltr=pos %>% filter (VesDate %in% ctch$VesDate)  %>% filter (VesDate %in% posOverGr$VesDate)              #filter POS only with catch AND insite 15 mile area site (posOverGr)
	posFltrCtch=left_join(posFltr,ctch,by="VesDate")                                                             #found CATCH for all points insite 15 miles area site  (some ships pass through 15 miles area=> also included)
	
	ps=pos %>% filter(VesDate %in% posFltrCtch$VesDate) %>% group_by(VesDate) %>% summarize(CountCoord=n())      # found TOTAL POS coord count 
	PosCatch= left_join(posFltrCtch,ps,by="VesDate")                                                             # joint POS insite 15 mile area WITH   POS count coord                                                         
	PosCatch$prCatch= PosCatch$catch/PosCatch$CountCoord                                                         # GET catch proportion  for every coords based on proportion pos coord
	
	 coords <- data.frame(lon=PosCatch$longitude,lat= PosCatch$latitude)      
     data   <- data.frame(VesDate=PosCatch$VesDate,prCatch=PosCatch$prCatch,fish=PosCatch$fish)   
     Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  crs)
	 PointsCatch =spTransform(Points,crs)
    PointsCatch$Ctchid=c(1:length(PointsCatch))
	
	
 ###############################################################################################  
     pip=point.in.poly(PointsCatch,CriticalArea)   # FOUND CATCH POS INSITE GRIDS SITE
	 pip1=as_tibble(pip)
     pip1=pip1[is.na(pip1$area)==F,] # here only we have catch ID
	 pip1$year=substr(pip1$VesDate,7,10)
	 pip1$mnth=substr(pip1$VesDate,12,13)
	 
     pip2=pip1 %>% group_by(area,site,cell,Ctchid,year,mnth,fish) %>% summarize (catch=sum(prCatch)) 
	# for (i in 1:length(pip2$cell)) {
	# pip2$ID[i]=strsplit(pip2$cell[i],"_")[[1]][1]
    #  }
	pip2$ID=1
	  pip2$area[pip2$area=="WBS"]="KC"
	pip2$area[pip2$area=="CI"]="KC"
	pip2$area[pip2$area=="KAMCH"]="KC"
	pip2$area[pip2$area=="KURIL"]="KUR"
	pip2$area[pip2$area=="NPSO"]="OKH"
    CellCtch <- pip2  %>% group_by(Ctchid,area,site,cell,year,mnth,ID,fish)%>%  summarize (catch =sum(catch))  

    Ch = CellCtch[!CellCtch$site %in% c("SHIPUNSKY CAPE","ZHELEZNAYA BAY"),] 
    tottalctch= Ch %>% group_by(fish,site, Ctchid) %>% summarise(catch=mean(catch))  %>% group_by(fish,site) %>% summarise(catch=sum(catch)) 
    sum(tottalctch$catch)
     ctch1=tottalctch %>% spread(fish,  catch)
	 
	 ##########
   Ch = CellCtch[CellCtch$site %in% c("SHIPUNSKY CAPE","ZHELEZNAYA BAY"),] 
    tottalctch= Ch %>% group_by(fish, Ctchid) %>% summarise(catch=mean(catch))  %>% group_by(fish) %>% summarise(catch=sum(catch)) 
    sum(tottalctch$catch)
     ctch1=tottalctch %>% spread(fish,  catch)
}	
	#################################
CellCtchINFO=function(){	

     reg= read.csv("tblsIN/id_region-NameRegion.csv")
	 
	 sites=read.csv("tblsIN/site50.csv")
	 sites$site=sites$Site;sites$Site=NULL
	 
	# catch=as_tibble(read.csv("tblsIN/catch_2000_2010_fishSqu.csv"))
    # PointsCatch = readRDS("PointsCatch_ALL")	 
     CellCtch=as_tibble(read.csv("CellCtch.csv"))
	 catch=catch %>% filter (id_region %in% reg$id_region)
	 catch=left_join(catch,reg,by="id_region")
#	 pos=read.csv("tblsIN/pos_2000_2010.csv")
	
	CellCtch=left_join(CellCtch,sites, by="site")
	
	########################################## catch are dublicated for overlap area =>> catch is more then fact
	 tottalCatch= CellCtch %>% group_by(Ctchid,fish,area)  %>% summarize(catch=mean(catch)) ### tottal catch
	 sum(tottalCatch$catch) # catch IN Feeding area
	 sum(catch$catch_volume) # catch fish and square IN DVFO
	 
	 ctchbyreg= catch %>% group_by(name_region)  %>%  summarise (catch=sum(catch_volume))
	 
	 
	pointsCtchInArea= PointsCatch[PointsCatch$Ctchid %in% CellCtch$Ctchid,]   # found vesel day in feeding area
	pointsCtchInArea$vsl= substr(pointsCtchInArea$VesDate, 1,5)
	length(unique(pointsCtchInArea$vsl)) # vesel in feeding area
	length(unique(pointsCtchInArea$VesDate)) # vesel day in feeding area
	
	
	count_coord1 = catch %>% filter (count_coord==1) %>% filter (ID %in% CellCtch$Ctchid) %>% group_by(ves,date) %>% summarise(n=mean())
	count_coord=length(count_coord1$ves) # ves date with only ONE coords
	
	catch$VesDate=paste0(catch$ves,"_",catch$date)
	pos$VesDate=paste0(pos$id_ves,"_",pos$date)
	posInFeeding=pos %>% filter (VesDate  %in% catch$VesDate)

	
	
	#########################
	criticalfishcatch=     tottalCatch %>% group_by(fish) %>% summarize(catch=sum(catch)) %>% arrange(desc(catch))
	 write.csv(criticalfishcatch,"Critical_ctch1.csv", row.names=F)	
	#######################
	areaCatch=tottalCatch %>% group_by(area) %>% summarize(catch=sum(catch)) %>% arrange(desc(catch))
	SDsiteCatch = CellCtch %>% group_by(site,area) %>% summarize(catch=sum(catch)) %>% group_by(area) %>% summarize(sd=sd(catch))
	###########
	
	
	
	
	
    tottalCtch= CellCtch %>% group_by(Ctchid,fish,Region) %>%  summarise(ctch=mean(catch)) %>%  group_by(fish,Region) %>%  summarise(ctch=sum(ctch))  %>% arrange(desc(ctch)) # tottal catch
	tmp=tottalCtch %>% spread(Region, ctch)

	
	 ctch=CellCtch %>% group_by(cell) %>% summarize(catch=sum(catch))
	  length(ctch$cell) # the tottal unique cells with fishing 
	 PerMonth=CellCtch %>% group_by(cell,year, mnth) %>% summarize(catch=sum(catch))  
	  summary(PerMonth$catch)  # summary fising per month
	  IQR(PerMonth$catch)      # summary fising per month
	  medianCatch=median(PerMonth$catch)
	  
     ctchMoreMedian=CellCtch %>% group_by(cell,year, mnth,season,site) %>% summarize(catch=sum(catch)) %>% filter (catch>medianCatch)
	  length(unique(ctchMoreMedian$cell))  # number uniq cell with catch more median
	  
	 ctchMoreMedian %>% group_by(cell) %>% summarize(n=n()) %>% summary(n)   # summary uniq cell with catch more then medianCatch tonn per month
	 ctchMoreMedian %>% group_by(cell) %>% summarize(n=n()) %>% summarize(IQR=IQR(n)) # IQR
	 
	 frcell=  ctchMoreMedian %>% group_by(site,cell) %>% summarize(n=n()) # cell duplicate with catch more median
	 lmt= quantile(frcell$n,0.95)  #us 0.9 quantile for count cell duplicated
   # b= frcell %>% group_by(site) %>% summarize(n=n()) %>% arrange(desc(ID)) # the 1 collumn table 3
	a= frcell %>%  filter (n>lmt) %>% group_by(site) %>% summarize(n=n())  # the count of uniq cell with catch more mediam and duplicated more then 0.95 quantile 
	    
		
		#AreaFood[AreaFood$site != "ZHELEZNAYA BAY",],
		
   # CellCtch %>% group_by(area) %>% summarize(catch=sum(catch))                                            # catch per AREA 
	  # IQR(CellCtch$catch)
	
	#####
	criticalCell= frcell$cell[frcell$n>lmt]  
	
ctch=CellCtch %>% filter (cell  %in% criticalCell) %>%  group_by(site,fish) %>% summarize(catch=sum(catch))  %>% arrange(desc(catch))
ctch1=ctch %>% spread(fish, catch)

   FeedingArea=readRDS("FeedingArea")
	shipunZeleznayaArea=FeedingArea[FeedingArea$site %in% c("SHIPUNSKY CAPE","ZHELEZNAYA BAY"),]
 
	}
###################################################  SPATIAL FOOD STABILITY
map=function(){

 
  circles=readRDS("circles")
  sites=readRDS("sites")
  KZ=readRDS("BoarderKZ");KZ=st_transform(KZ, crs)
 # Pos=readRDS("Pos20WithDublicate")
 
  
  siteALL=readRDS("siteALL")
  siteALL=siteALL[siteALL$Area %in% sites$Area,]
  
   crcls=st_as_sf(circles)
  
  #siteFlt=  c("KEKURNY CAPE","SHIPUNSKY CAPE","ZHELEZNAYA BAY","KOZLOV CAPE")
#   siteFlt=  c("CHIRINKOTAN","MATUA/POLOGY","CHIRPOY/UDUSHLIVY","ANTSIFEROV/ROOKERY","SHIASHKOTAN/KRASNY","SIMUSHIR/KRASNOVATAYA","BRAT CHIRPOYEV/ROOKERY","ONEKOTAN/KAMEN YASNOY POGODY","RAYKOKE","SIMUSHIR/ARONT","URUP/CHAYKA")
 #  siteFlt=  c("ZAVYALOV","YAMSKY ISLS","IONY")
  # siteFlt=  c("ARAGINSKY/KRASHENINNIKOV")
   GeneralMap=function(){
    
  pos=read.csv("tblsIN/pos_2000_2010.csv")
  index=sample(1:length(pos[,1]))
  ps=pos[index,][1:1000000,]
  
    ps$LatTest=nchar(substr(ps$latitude,4,7))
    ps$LonTest=nchar(substr(ps$longitude,5,8))
    ps=ps %>% filter (LatTest !=0) %>% filter (LonTest !=0)
  
  
  ps1=ps[is.na(ps$latitude)==F,]
  ps2=st_as_sf(ps1, coords = c("longitude","latitude"), crs = 4326) %>% st_transform(crs)
   
  

    bbox = c(left = 140,  right =166 ,bottom =40, top =63) #legend
   
   bsm=          basemap(limits = bbox,bathymetry = T,rotate=T,land.col = "#eeeac4", bathy.style = "poly_greys", lon.interval =5,grid.col = NA)+ # land.border.col = NA
               theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			   ,legend.position="none" 
			   )
    bsm
   
   
   
   
   		   

 p=    bsm +
       annotation_scale(location = "br") + 
       annotation_north_arrow(location = "tr", pad_x=unit(4.5, "cm"),pad_y=unit(13.5, "cm")) +  #which_north = "true",

	   annotation_spatial(data = ps2,inherit.aes = F,alpha=0.01,size=0.00005, color = "red")+ 
	#   annotation_spatial(data = sites,inherit.aes = F,size=4,pch=13)+ #
	   annotation_spatial(data = crcls,inherit.aes = F,alpha=0.00001, size=1,color = "green")+
        xlab("Долгота")+ ylab("Широта")	
		
		p
		
		
	  
      p$layers[7]=p$layers[2] # land over 
	  p$layers[8]=p$layers[3] # scale over
	  p$layers[9]=p$layers[4] # north over
	
   
    p
   
   
   
   
   }
    
   
   
   subINOUT=function(){
  Pos$time=substr(Pos$datetime,11,20)
  Pos$time=as.POSIXct(strptime(Pos$time, format="%H:%M:%S"))
  TIMEVECTOR= c("12:00:00","00:00:00","00:01:00","23:59:00")
  TIMEVECTOR=as.POSIXct(strptime(TIMEVECTOR, format="%H:%M:%S"))
  Pos=Pos[!Pos$time %in% TIMEVECTOR,]

  PsFfltr=Pos[Pos$site %in% siteFlt,]
  

   ftr= point.in.poly(PsFfltr,circles[circles$site %in% siteFlt,])
   IN=data.frame(ftr[is.na(ftr$site.y)==F,])
   OUT=data.frame(ftr[is.na(ftr$site.y)==T,])
   
   IN= IN %>% group_by(VesDate,datetime) %>%  summarise(lat=mean(coords.x2),lon=mean(coords.x1))   # remove dublicate
   OUT= OUT %>% group_by(VesDate,datetime) %>%  summarise(lat=mean(coords.x2),lon=mean(coords.x1)) # remove dublicate
   
   IN=st_as_sf(IN, coords = c("lon","lat"), crs = 4326) %>% st_transform(crs)
   OUT=st_as_sf(OUT, coords = c("lon","lat"), crs = 4326) %>% st_transform(crs)
   }
   
  
  

exlude=function(){

NoFishingAvachaBay=readOGR("NoFishingAvachaBay.kml")
NoFishingAvachaBay=spTransform(NoFishingAvachaBay,crs)
 VesIN= data.frame(point.in.poly(PsFfltr,NoFishingAvachaBay))
 VesIN=VesIN[is.na(VesIN$Name)==FALSE,]
 IN= PsFfltr[PsFfltr$VesDate %in% VesIN$VesDate,]
 


 ctchMore1=read.csv("ctchMore1.csv")
 PsFfltr1=PsFfltr[PsFfltr$VesDate %in% ctchMore1$Ves_Date,]

 siteALL=read.csv("tblsIN/SSLsites.csv")
 siteALL=siteALL[siteALL$declong>0,]
 siteALL=siteALL[is.na(siteALL$declat)==F,]
  siteALL= st_as_sf( siteALL, coords = c("declong","declat"), crs = 4326) %>% st_transform(3857)
  siteALL=st_transform( siteALL, crs)
saveRDS(siteALL,"siteALL")
 # FeedingArea=readRDS("FeedingArea")
 # CellCtch=read.csv("CellCtch.csv")
 
  circles=readRDS("circles");  circles=st_as_sf(circles) ;  circles=st_transform(circles, crs)
  sites=readRDS("sites"); sites=st_as_sf(sites) ;  sites=st_transform(sites, crs)
  #KZ=readRDS("BoarderKZ ");KZ=st_transform(KZ, crs)
 
  Pos20WithDublicate=readRDS("Pos20WithDublicate"); Pos20WithDublicate=st_as_sf(Pos20WithDublicate) ;  Pos20WithDublicate=st_transform(Pos20WithDublicate, crs)

 saveRDS(Pos20WithDublicate,"Pos20WithDublicate")
 saveRDS(circles,"circles")
 saveRDS(sites,"sites")




#  PointsCatch =readRDS("PointsCatch_ALL"); PointsCatch=st_as_sf(PointsCatch)#, coords = c("declong", "declat"), crs = 4326) %>% st_transform(3857)
  #########
#reg=as_Spatial(reg)
#reg=spTransform(reg,crs)
#writeOGR(reg,"reg.kml", driver="KML", layer="id_region")

#reglist=list.files("reg", full.names=T)
#Myreg=NULL
# for (i in 1:length(reglist)) {
# rg=readOGR(reglist[i])
# if (i==1){Myreg=rg} 
# else { Myreg=rbind(Myreg,rg)}
# }
#Myreg=st_as_sf(Myreg);Myreg=st_transform(Myreg, crs)

############################### 


#ggOceanMapsenv <- new.env(); ggOceanMapsenv$datapath <-paste0("OceanMap")
#ctch=CellCtch %>% group_by(cell,year, mnth,season,site,ID) %>% summarize(catch=sum(catch)) %>% filter (catch>1)
#frcell=  ctch %>% group_by(cell,ID,site) %>% summarize(n=n()) 
# lmt= quantile(frcell$n,0.95)  #us 0.9 quantile for filter critical area
#crtArea=frcell[frcell$n>lmt,]    	  
 # CriticalArea=  FeedingArea[FeedingArea$cell %in% crtArea$cell,]
 # CriticalArea= st_as_sf(CriticalArea, coords = c("lat", "lon"), crs = 4326) %>% st_transform(3857)
 
 # FeedingArea= st_as_sf(FeedingArea, coords = c("lat", "lon"), crs = 4326) %>% st_transform(3857)
  ##########
#  SHIPUNSKYCAPE=CriticalArea[CriticalArea$site=="SHIPUNSKY CAPE",]
#  KEKURNYCAPE=CriticalArea[CriticalArea$site=="KEKURNY CAPE",]
#  cell=unique(KEKURNYCAPE$cell)

#  fin=NULL
#  for (i in 1:length(cell)) {
#  cellPol=KEKURNYCAPE[KEKURNYCAPE$cell==cell[i],]
#  site=cellPol$site
#  over1=data.frame(st_intersects(cellPol,CriticalArea[CriticalArea$site=="SHIPUNSKY CAPE",]))
#   if (length(over1[,1]) != 0) {celToRemove=data.frame(celToRemove=cell[i]); fin=rbind(celToRemove,fin) } 
#  }
 #  CriticalArea=CriticalArea[CriticalArea$site != "ZHELEZNAYA BAY",]
 #  CriticalArea=CriticalArea[!CriticalArea$cell %in% fin$celToRemove,]
#  SMA=as_tibble(AreaFood)
#   SMA %>% group_by (area,site) %>% summarize (n=n())
#	b = getNOAA.bathy(lon1 = bbox[1], lon2 = bbox[2], lat1 = bbox[3], lat2 = bbox[4],resolution = 1)
#    bf = fortify.bathy(b)

#	coords <- data.frame( lon=bf$x,lat= bf$y)      
#   data   <- data.frame(depth=bf$z)   
#  baty <- SpatialPointsDataFrame(coords = coords,
#                                  data = data, 
#                                 proj4string =  crs)
									 
#	baty =spTransform(baty,crs)
#	bt= st_as_sf(baty, coords = c("lat", "lon"), crs = 4326) %>% st_transform(3857)

#######
 # BoarderKZ = readOGR("C:\\Users\\usato\\Documents\\YandexDisk\\CURRENT WORK\\ISR-SSLDIET\\map\\Maping boarder KZ\\KZ_border.shp")
 # BoarderKZ=BoarderKZ[BoarderKZ$FID==1,]
 # BoarderKZ=data.frame(coordinates(BoarderKZ))
 # names(BoarderKZ)=c("lon","lat")
 # BoarderKZ$group=1
#KZ= BoarderKZ %>% st_as_sf(coords = c("lon", "lat"), agr = "constant", crs = 4326) %>%
#      st_transform(3857) %>%
 #    group_by(group) %>%
  #    summarise(do_union = FALSE) %>%
   #   st_cast("LINESTRING")
   #################
#  region= as_tibble(read.csv("tblsIN/id_region-NameRegion.csv")) 
#   dat = as_tibble(read.csv("tblsIN/fish_reg_polygons.csv")) 
# dat=dat %>% filter (id_region %in% region$id_region)
#dat=dat[is.na(dat$latitude)==F,] 
#dat$longitude[dat$longitude<0]=180#360+dat$longitude[dat$longitude<0]
#reg= dat %>% st_as_sf(coords = c("longitude", "latitude"), agr = "constant", crs = 4326) %>%
#      st_transform(3857) %>%
#     group_by(id_region) %>%
#      summarise(do_union = FALSE) %>%
#      st_cast("LINESTRING")
#	  reg=reg[reg$id_region != 261,]
#saveRDS	 (reg,"region") 


}
 # bbox = c(left = 140,  right =166 ,bottom =40, top =63) #legend
 # bbox = c(left = 141,  right =164 ,bottom =42, top =61) #KURILLY SIMUSHIR
  # bbox = c(left = 154.2,  right =157.2 ,bottom =49.24, top =50.9) #ONEKOTAN, ANZIFER
 
  #  bbox = c(left = 160,  right =162 ,bottom =54, top =55) #KOZLOVA
  #  bbox = c(left = 149.8,  right =153 ,bottom =45.5, top =47.6) #SIMUSHIR
#	 bbox = c(left = 152.5,  right =154 ,bottom =47.5, top =48.5) # MATUA
  
  
#  bbox = c(left = 157,  right =163.8 ,bottom =51, top =55.3) #KAMCHATKA
 # bbox = c(left = 149,  right =157 ,bottom =45, top =51.5) #KURILL

 #  bbox = c(left = 142,  right =156 ,bottom =54, top =60) #okhotsk
  bbox = c(left = 161,  right =165 ,bottom =57.5, top =59.5) #KARAGINSKY


bsm=          basemap(limits = bbox,bathymetry = T,rotate=T,land.col = "#eeeac4", bathy.style = "poly_greys", lon.interval =5,grid.col = NA)+ # land.border.col = NA
               theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			   ,legend.position="none" 
			   )
bsm
 #bsm+annotation_spatial(data = siteALL,inherit.aes = F,size=4,pch=1)
			   
			   
			   
			   
			   

 p=    bsm +
       annotation_scale(location = "br") + 
       annotation_north_arrow(location = "tr", which_north = "true") +  #
	  #  annotation_spatial(data = KZ,inherit.aes = F,size=1,color="gray25")+
	   annotation_spatial(data = OUT,inherit.aes = F,alpha=0.01,size=0.00005, color = "red")+ 
	   annotation_spatial(data = IN,inherit.aes = F,alpha=0.01, size=0.00005,color = "blue")+
	   annotation_spatial(data = siteALL,inherit.aes = F,size=4,pch=1)+
	   annotation_spatial(data = sites,inherit.aes = F,size=4,pch=13)+ #
	   annotation_spatial(data = crcls,inherit.aes = F,alpha=0.00001, size=0.7,color = "green")+
        xlab("Долгота")+ ylab("Широта")	
		
		p
		
		
	  
      p$layers[10]=p$layers[2] # land over 
	  p$layers[11]=p$layers[3] # scale over
	  p$layers[12]=p$layers[4] # north over
	  
	  p$layers[13]=p$layers[7] # sites over
	  p$layers[14]=p$layers[8] # sites over

	  
	  

p



}		  
############################
PosDistance1=function () { # no operaion table in analis

   reg=read.csv("tblsIN/id_region-NameRegion.csv")
   sites=readRDS("sites")
   sites=data.frame(site=sites$Site,id=sites$id)
   ctch=read.csv("tblsIN/catch_2000_2010.csv") 
  # pos=read.csv("tblsIN/pos_2000_2010.csv")
   pos=readRDS("PosWithCtch")
   pos$Ves_Date=paste0(pos$id_ves, "_", pos$date)
   circlsArea=readRDS("circles")
   circlsArea=as_Spatial(circlsArea);circlsArea=spTransform(circlsArea,crs)
   circlsArea=circlsArea[circlsArea$Region != "CI",]
   #Ves_DateInCrcl=read.csv("Ves_DateInCrcl.csv")
   ctch=ctch[ctch$id_region %in% reg$id_region,]
   #pos= pos[pos$Ves_Date %in% Ves_DateInCrcl$Ves_Date,]
   
subF=function(){
 
# count_coord=ctch %>% group_by(id_region,Ves_Date) %>%summarise(count_coord=mean(count_coord))
# count_coord1=left_join(count_coord,reg,by="id_region")

PosWithCtch=pos[pos$Ves_Date %in% ctch$Ves_Date,]
PosWithCtch$time=substr(PosWithCtch$datetime,12,20)
PosWithCtch=PosWithCtch[PosWithCtch$time != "12:00:00",]

    coords <- data.frame(lon=PosWithCtch$longitude,lat= PosWithCtch$latitude)   
   data   <- data.frame(id_ves=PosWithCtch$id_ves,  date=PosWithCtch$date)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                    data = data, 
                                     proj4string =  crs)
									 
	Points =spTransform(Points,crs)
	saveRDS(Points,"PosWithCtch")
	
#	circlsArea=readRDS("AreaFood31")
#	PosWithCtch=readRDS("PosWithCtch")
#	PosWithCtch$year=substr(PosWithCtch$date,1,4)
#	years=unique(PosWithCtch$year)
#	PosWithCtch$Ves_Date=paste0(PosWithCtch$id_ves,"_",  PosWithCtch$date)
#    circlsArea=as_Spatial(circlsArea)
#    circlsArea =spTransform(circlsArea,crs)
#	
#for (y in 1:length(years)) {
#    year=years[y]
#	posYear=PosWithCtch[PosWithCtch$year==year,]
#	index=over(posYear,circlsArea)
#	posYearOverCircl=posYear[is.na(index$site)==F,]
#	saveRDS(posYearOverCircl,paste0("posOver20Miles/",year))
#	print(year)
#	}
#
#Ves_date_fin=NULL
#listPos=list.files("posOver20Miles", full.names=T)
#	for (i in 1:length(listPos)){
#	pos=readRDS(listPos[i])
#	Ves_Date=data.frame(Ves_Date=pos$Ves_Date)
#	Ves_date_fin=rbind(Ves_Date,Ves_date_fin)
#
#	}
 #   write.csv(Ves_date_fin,"Ves_DateInCrcl.csv",row.names=F)
	}
##########

#PosDistance= NULL
PosDistance=read.csv("PosDistance.csv")

pos$year=substr(pos$date, 1,4)
years=unique(pos$year)
 
  for (i in 8:length(years)) {
   year= years[i]
   PointsCatchYear=pos[pos$year==year,]


 for (e in 1:length(circlsArea)){
   area=circlsArea[e,]
   ovr=point.in.poly(PointsCatchYear,area)
   ovr1= ovr[is.na(ovr$Site)==F,]
   ovr1$Ves_date=paste0(ovr1$id_ves,"_",ovr1$date)
   VsDat=unique(ovr1$Ves_date)
 for (i in 1:length(VsDat)) {
 VD=VsDat[i]
 ovr2=ovr1[ovr1$Ves_date==VD,]
  if (length(ovr2$id_ves) >1) { # only if positions more than 1

	for (y in 1:(length(ovr2)-1)) {
    mdist <- distm(ovr2[y:(y+1),])[,2][1]	
	dust=data.frame(VsDat=VD,mdist=mdist,Ccoord=length(ovr2$id_ves),site= area$Site, Area=area$Region,year=year)
    PosDistance=rbind(PosDistance,dust)
   }
 }
}
print(paste0("Done   ", year,"____",area$Site,"    ",Sys.time()))
}

}
  write.csv(PosDistance,"PosDistance.csv", row.names=F)
################################################################################  
  
  PosDistance %>% group_by(site)  %>% summarise (Ncoord=median(Ccoord), Distance=median(mdist))
  Pd=left_join(PosDistance,sites, by="site")
  Pd$id=as.factor(Pd$id)
  
    PosDistance$Area[PosDistance$Area=="WBS"]="KC"
	PosDistance$Area[PosDistance$Area=="CI"]="KC"
	PosDistance$Area[PosDistance$Area=="KAMCH"]="KC"
	PosDistance$Area[PosDistance$Area=="KURIL"]="KUR"
	PosDistance$Area[PosDistance$Area=="NPSO"]="OKH"
  
  
  ggplot(count_coord1, aes(x=name_region, y=count_coord)) + 
    geom_boxplot() +
	 theme(axis.text.x=element_text(angle = -90, hjust = 0,size=20))+
	 xlab("Site id")+
	 ylab("Number of coordinates per ship day")+
	# scale_y_continuous(limits=c(0,12), breaks=seq(1,12, by = 1))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			   #,legend.position="none" 
			   )

  ggsave("NumberCoord.png")
  
   ggplot(Pd, aes(x=id, y=mdist, fill=Area)) + 
    geom_boxplot() +
	 theme(axis.text.x=element_text(angle = -90, hjust = 0,size=20))+
	 xlab("Site id")+
	 ylab("Distance between two consecutive points")+
	 scale_y_continuous(limits=c(0,25000), breaks=seq(1,25000, by = 5000))+
	  theme(legend.key.size = unit(2, "cm"),
			   legend.text = element_text(size = 15),
			   axis.text=element_text(size=14),
			   axis.title=element_text(size=15),
			   legend.title=element_blank()
			   #,legend.position="none" 
			   )
	  ggsave("Distance coord.png")	   
 ############################################
 anova (glm(data=PosDistance,formula=mdist~Area,family = gaussian))
 
 
 
 
 
 
 
 PosDistance$mdist= PosDistance$mdist/1852
 PosDistance=PosDistance %>%  arrange(desc(mdist))
 
 PosDistance1=PosDistance %>% group_by(VsDat) %>% summarize (mdist=median(mdist)) %>%  arrange(desc(mdist))
 summary(PosDistance1$mdist)
 IQR(PosDistance1$mdist)
}
##############################################
cellsize=function (){
   VesDateInCriticalArea=read.csv("VesDateInCriticalArea.csv")
   pos=read.csv("tblsIN/pos_2000_2010.csv") 
   pos$VesDate=paste0(pos$id_ves,"_", pos$date) 
   pos$time=substr(pos$datetime,12,20)
   pos= pos[pos$time != "12:00:00",]
   pos= pos[pos$VesDate %in% VesDateInCriticalArea$VesDate,]
   head(pos)
   VesDateUniq=unique(pos$VesDate)
  
  pr=0
   cellsize=NULL
   for (i in 1:length(VesDateUniq)) {
   VS=VesDateUniq[i]
   TrackDay=pos[pos$VesDate == VS,]
   TD=data.frame(lon=TrackDay$longitude,lat=TrackDay$latitude,w=1)
   w= TD$w # value- ctch
  
  sigma<-(sd(TD$lon)+sd(TD$lat))/2
  iqr<-(IQR(TD$lon)+IQR(TD$lat))/2
  bandwidth <- 0.9*min(sigma, iqr)*TD$w^(-1/5)
  res=data.frame(id_ves=TrackDay$id_ves[1], date=TrackDay$date[1], x=bandwidth)
  cellsize=rbind(res,cellsize)
  percent= round(i/length(VesDateUniq)*100)
   if (percent != pr) {pr=percent; print(pr)}
   }
   
   cellsize1= cellsize %>% group_by(id_ves,date)%>% summarize (bandwidth=median(x))
   cellsize1$meters= cellsize1$bandwidth *(10000/90)*1000
    cellsize1$miles=cellsize1$meters/1852
   summary(cellsize1$miles)
   IQR(cellsize1$miles, na.rm=T)
   }
#############################################################
fishFilter=function(){
 
 reg= read.csv("tblsIN/id_region-NameRegion.csv")
 catch=read.csv("tblsIN/catch_2000_2010.csv")
 catch=catch[catch$id_region %in% reg$id_region,]
 
 length(unique(catch$fish))
 fsh=c("минтай",
       "треска",
       "терпуги",
       "палтус белокорый",
        "бычки",
        "скаты",
        "палтус",
        "окунь морской",
        "треска ярусная",
        "сельдь",
        "кальмары прочие",
        "навага",
        "ленок",
        "горбуша",
        "кета",
        "нерка",
        "кальмар тихоокеанский",
        "палтус стрелозубый",
        "кижуч",
        "голец",
        "ерш длинноперый",
        "корюшка",
        "осьминоги",
         "кальмар командорский",
         "кальмар Бартрама",
         "окунь-клювач",
         "шипощек",
        "шримсы-медвежата",
        "камбалы дальневосточные",
        "сельдь тихоокеанская",
        "палтус черный (синекорый)",
         "мойва",
        "сардина иваси",
       "акулы",
        "сайра",
       "лососевые т-о",
      "молодь лососевых",
	  "скумбрия", 
	  "зубатка",
	  "тресковые проч.",
	  "сельдь-черноспинка",
	  "хек(мерлуза)",
	  "корюшка азиатская зубастая",
	  "камбала",
	  "угольная рыба",
	  "осьминог Дофлейна гигантский",
	   "ставриды",
	   "чавыча",
	   "кунджа",
	   "песчанка",
	  "макруронус",
	  "сардина",
	  "осьминог песчаный",
	  "сайка",
	  "кальмар-илекс",
	  "серебрянка"
)
 
 
catch=catch[catch$fish %in% fsh,]
length(unique(catch$fish))
write.csv(catch,"tblsIN/catch_2000_2010_fishSqu.csv", row.names=F)
 
 fshSSLImportant=c("терпуги",
		                     "минтай",
							 "бычки",
							 "анчоусы",
		                    "треска",
							"треска ярусная",
							
							"корюшка азиатская зубастая",
							"корюшка малоротая морская",
							"корюшка малоротая",
							"корюшка",	
							
							"камбала",
							"камбала-ерш",
							 "камбалы дальневосточные",
							 
							 "сельдь тихоокеанская",
							 "сельдь",
							 
							 
							 "кальмар командорский",
							 "кальмар тихоокеанский",
							 "кальмар Бартрама",
							 "кальмары прочие",		
							 						 
							 "горбуша",
							 "кета",
							 "нерка",							 
							 "чавыча",
							 "кижуч",
							 "лососевые т-о")
  
  

}
#####################################################
budget_time=function() {


 
 
 
  pos=as_tibble(readRDS("Pos20WithDublicate"))
 # pos=as_tibble(read.csv("tblsIN/pos_2000_2010.csv"))
  bd=as_tibble(read.csv("tblsIN/Budget_time_2000_2010.csv"))
  ctch=as_tibble(read.csv("tblsIN/catch_2000_2010.csv"))
  
  
     siteFlt=  c("KEKURNY CAPE")
     pos=pos[pos$site==siteFlt,]
     pos$Vesdatetime=paste0(pos$id_ves ,"_", pos$datetime)
  

   bd$end_date=substr(bd$end_date_time,1,10)
   bd$use_time_v= bd$use_time_volume/1440
   bd$timeFising=times(bd$use_time_v)
   
   bd$Vesdatetime=paste0(bd$id_ves,"_",bd$end_date," ", bd$timeFising )


   bd= bd[bd$duration != 0,]
   bd= bd[bd$use_time_v != 24,]
   
   
  


#bd %>% filter (id_ves=="27860") %>% filter(end_date=="2010-04-29")
#ctch %>% filter (ves=="27860") %>% filter(date=="2010-04-29")
#pos %>% filter (id_ves=="27860") %>% filter(date=="2010-04-29")



  
   
   

   


   psbd=left_join(bd, pos, by="Vesdatetime")
   
   psbd=psbd[is.na(psbd$id_use_time )==F,]
   psbd=psbd[is.na(psbd$lat)==F,]
   
  
   
   
   
   
   
   
     coords <- data.frame(lon=psbd$lon,lat= psbd$lat)      
     data   <- data.frame(Vesdatetime=psbd$Vesdatetime)   
     Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                    proj4string =  crs)
	 psbd =spTransform(Points,crs) 
     psbd=st_as_sf(psbd )






   basemap(limits =  c(left = 157,  right =163.8 ,bottom =51, top =55.3),bathymetry = F,rotate=T)+
   annotation_spatial(data = psbd,inherit.aes = F,alpha=0.1,size=0.05, color = "red")




}



















   
   
   