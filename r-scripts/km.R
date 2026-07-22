km1=function(lat1,long1,lat2,long2) suppressWarnings(6371*acos(sin(lat1*pi/180)*sin(lat2*pi/180)+cos(lat1*pi/180)*cos(lat2*pi/180)*cos(abs(long1-long2)*pi/180)))
km=function(lat1,long1,lat2,long2){k=km1(lat1,long1,lat2,long2); if(any(is.na(k))){ru=runif(4,-1,1)/100; if(length(lat1)>1) a1=is.na(k) else a1=T;
if(length(lat2)>1) a2=is.na(k) else a2=T; k[is.na(k)]=km1(lat1[a1]+ru[1],long1[a1]+ru[2],lat2[a2]+ru[3],long2[a2]+ru[4])}; k}

gl=read.csv("http://charcoalcharts.com/data/greatlakes.csv")
plotlake=function(col=gray(.9)) for(p in unique(gl$poly)) polygon(gl$long[gl$poly==p],gl$lat[gl$poly==p],col=col,border=NA,xpd=T)

plotCircle <- function(LatDec, LonDec, Km, n=360, data=F, ...) {
    #LatDec = latitude in decimal degrees of the center of the circle
    #LonDec = longitude in decimal degrees
    #Km = radius of the circle in kilometers
    ER <- 6371 #Mean Earth radius in kilometers. Change this to 3959 and you will have your function working in miles.
    AngDeg <- seq(1:n) #angles in degrees 
    Lat1Rad <- LatDec*(pi/180)#Latitude of the center of the circle in radians
    Lon1Rad <- LonDec*(pi/180)#Longitude of the center of the circle in radians
    AngRad <- AngDeg*(pi/180)#angles in radians
    Lat2Rad <-asin(sin(Lat1Rad)*cos(Km/ER)+cos(Lat1Rad)*sin(Km/ER)*cos(AngRad)) #Latitude of each point of the circle rearding to angle in radians
    Lon2Rad <- Lon1Rad+atan2(sin(AngRad)*sin(Km/ER)*cos(Lat1Rad),cos(Km/ER)-sin(Lat1Rad)*sin(Lat2Rad))#Longitude of each point of the circle rearding to angle in radians
    Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
    Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
    if(!data) polygon(Lon2Deg,Lat2Deg,...)
    if(data) data.frame(lat=Lat2Deg,long=Lon2Deg)
}

longAdj=function(lat){
a=6378137 #semimajor axis meters
e=0.00669438 #eccentricity
b=a*sqrt(1-e**2) #semiminor axis
m=function(lat) atan(b*tan(pi*lat/180)/a)
circ=function(lat) 2*pi*a*cos(m(lat))
circ(lat)/circ(0)}

fillCircle <- function(LatDec, LonDec, Km, n = 360, ...) {
  
  ER <- 6371
  
  AngDeg <- seq(0, 359, length.out = n)
  AngRad <- AngDeg*pi/180
  
  Lat1Rad <- LatDec*pi/180
  Lon1Rad <- LonDec*pi/180
  d <- Km/ER
  
  Lat2Rad <- asin(
    sin(Lat1Rad)*cos(d) +
      cos(Lat1Rad)*sin(d)*cos(AngRad)
  )
  
  Lon2Rad <- Lon1Rad +
    atan2(
      sin(AngRad)*sin(d)*cos(Lat1Rad),
      cos(d)-sin(Lat1Rad)*sin(Lat2Rad)
    )
  
  Lat2Deg <- Lat2Rad*180/pi
  Lon2Deg <- (Lon2Rad*180/pi + 180) %% 360 - 180
  
  ####################################################
  ## Does the boundary wrap around the world?
  ####################################################
  
  dateline <- diff(range(Lon2Deg)) > 350
  
  ddeg <- d*180/pi
  
  north <- (LatDec + ddeg > 90)
  south <- (LatDec - ddeg < -90)
  

  if((north & !south) | (north & dateline)){
    
    i1 <- which.min(Lon2Deg+1000*(Lat2Deg<median(Lat2Deg)))
    i2 <- which.max(Lon2Deg+1000*(Lat2Deg>median(Lat2Deg)))
    
    lat1 <- Lat2Deg[i1]
    lat2 <- Lat2Deg[i2]
    
    if(i1 < i2){
      
      Lon2Deg <- append(
        Lon2Deg,
        c(-180,-180,180,180),
        after=i1)
      
      Lat2Deg <- append(
        Lat2Deg,
        c(lat1,90,90,lat2),
        after=i1)
      
    } else {
      
      Lon2Deg <- append(
        Lon2Deg,
        c(180,180,-180,-180),
        after=i2)
      
      Lat2Deg <- append(
        Lat2Deg,
        c(lat2,90,90,lat1),
        after=i2)
      
    }
  }
  
  if((!north & south) | (south & dateline)){
    
    i1 <- which.min(Lon2Deg+1000*(Lat2Deg>median(Lat2Deg)))
    i2 <- which.max(Lon2Deg+1000*(Lat2Deg<median(Lat2Deg)))
    
    lat1 <- Lat2Deg[i1]
    lat2 <- Lat2Deg[i2]
    
    if(i1 < i2){
      
      Lon2Deg <- append(
        Lon2Deg,
        c(-180,-180,180,180),
        after=i1)
      
      Lat2Deg <- append(
        Lat2Deg,
        c(lat1,-90,-90,lat2),
        after=i1)
      
    } else {
      
      Lon2Deg <- append(
        Lon2Deg,
        c(180,180,-180,-180),
        after=i2)
      
      Lat2Deg <- append(
        Lat2Deg,
        c(lat2,-90,-90,lat1),
        after=i2)
      
    }
  }
  
  if(north & south & !dateline){
    
    i0 <- which.min(Lat2Deg)
    
    lat0 <- Lat2Deg[i0]
    lon0 <- Lon2Deg[i0]
    
    Lon2Deg <- append(
      Lon2Deg,
      c(lon0,lon0,-180,-180,180,180,lon0,lon0),
      after=i0)
    
    Lat2Deg <- append(
      Lat2Deg,
      c(lat0,-90,-90,90,90,-90,-90,lat0),
      after=i0)
    
  }
  
  polygon(Lon2Deg, Lat2Deg, fill=T, ...)
}
