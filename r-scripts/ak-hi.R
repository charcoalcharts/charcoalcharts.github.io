install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("maps")

alaska=function(...){
m=map("world","USA:Alaska",plot=F)
mx=c(); for(j in 1:length(m$names)) mx[j]=max(map("world",m$names[j],plot=F)$x,na.rm=T)
map("world",c(m$names[mx<0],"USA:Alaska"),exact=T,...)}
hawaii=function(...) map("world","USA:Hawaii",...)

uscan=function(...){
m=map("world",c("USA","Canada"),plot=F)
mx=c(); for(j in 1:length(m$names)) mx[j]=max(map("world",m$names[j],plot=F)$x,na.rm=T)
map("world",c(m$names[mx<0],"USA","USA:Alaska"),exact=T,...)}
