km1=function(lat1,long1,lat2,long2) suppressWarnings(6371*acos(sin(lat1*pi/180)*sin(lat2*pi/180)+cos(lat1*pi/180)*cos(lat2*pi/180)*cos(abs(long1-long2)*pi/180)))
km=function(lat1,long1,lat2,long2){k=km1(lat1,long1,lat2,long2); if(any(is.na(k))){ru=runif(4,-1,1)/100; if(length(lat1)>1) a1=is.na(k) else a1=T;
if(length(lat2)>1) a2=is.na(k) else a2=T; k[is.na(k)]=km1(lat1[a1]+ru[1],long1[a1]+ru[2],lat2[a2]+ru[3],long2[a2]+ru[4])}; k}

gl=read.csv("http://charcoalcharts.com/data/greatlakes.csv")
plotlake=function(col=gray(.9)) for(p in unique(gl$poly)) polygon(gl$long[gl$poly==p],gl$lat[gl$poly==p],col=col,border=NA)
plotlakes=function(col=gray(.9)) plotlake()
