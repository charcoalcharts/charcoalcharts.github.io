install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("httr")
install.if("geosphere")

library(httr)
library(geosphere)

blines=function(x,y,rcol="black",fcol="blue",rlwd=1,flwd=2,drive=T,data=F,...){
  loc=paste(paste(x[1],y[1],sep=","),paste(x[2],y[2],sep=","),sep=";")
  url=paste0("http://router.project-osrm.org/route/v1/driving/",loc,"?geometries=geojson&overview=full")
  r0=GET(url); r0=content(r0); if(r0$code=="Ok" & drive){rx=unlist(r0$routes[[1]]$geometry$coordinates); n=length(rx)/2; sn=seq(1,n,by=10)
  long=rx[1:n*2-1][sn]; lat=rx[1:n*2][sn]
  if(data) data.frame(lat,long) else lines(long,lat,col=rcol,lwd=rlwd,...)}
if(r0$code!="Ok" | !drive){
path=gcIntermediate(cbind(x[1],y[1]),cbind(x[2],y[2]),n=101,addStartEnd=T,breakAtDateLine=T)
if(length(path)==2) path=rbind(path[[1]],c(NA,NA),path[[2]])
if(data) data.frame(lat=path[,2],long=path[,1]) else lines(path[,1],path[,2],col=fcol,lwd=flwd,...)}}

rlines=function(x,y,data=F,...){
  loc=paste(paste(x[1],y[1],sep=","),paste(x[2],y[2],sep=","),sep=";")
  url=paste0("http://router.project-osrm.org/route/v1/driving/",loc,"?geometries=geojson&overview=full")
  r0=GET(url); r0=content(r0); if(r0$code=="Ok"){rx=unlist(r0$routes[[1]]$geometry$coordinates); n=length(rx)/2; sn=seq(1,n,by=10)
  long=rx[1:n*2-1][sn]; lat=rx[1:n*2][sn]
  if(data) data.frame(lat,long) else lines(long,lat,...)}}

glines=function(x,y,data=F,...){
path=gcIntermediate(cbind(x[1],y[1]),cbind(x[2],y[2]),n=101,addStartEnd=T,breakAtDateLine=T)
if(length(path)==2) path=rbind(path[[1]],c(NA,NA),path[[2]])
if(data) data.frame(lat=path[,2],long=path[,1]) else lines(path[,1],path[,2],...)}

