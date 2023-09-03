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
