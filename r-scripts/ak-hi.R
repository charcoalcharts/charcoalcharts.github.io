install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("maps")
library(maps)

alaska=function(...){
m=map("world","USA:Alaska",plot=F)
mx=c(); for(j in 1:length(m$names)) mx[j]=max(map("world",m$names[j],plot=F)$x,na.rm=T)
map("world",c(m$names[mx<0],"USA:Alaska"),exact=T,...)}
hawaii=function(...) map("world","USA:Hawaii",...)

uscan=function(...){
m=map("world",c("USA","Canada"),plot=F)
mx=c(); for(j in 1:length(m$names)) mx[j]=max(map("world",m$names[j],plot=F)$x,na.rm=T)
map("world",c(m$names[mx<0],"USA","USA:Alaska"),exact=T,...)}

#par(fig=c(0,1,0,1)); map("usa",xpd=T)
add.alaska=function(fig=c(0,0,.3,.3),...){par(new=T,fig=fig[c(1,3,2,4)],mar=rep(0,4)); plot(alaska(plot=F),type="n",bty="n",xaxt="n",yaxt="n"); alaska(add=T,xpd=T,...)}
add.hawaii=function(fig=c(.25,.05,.45,.25),...){par(new=T,fig=fig[c(1,3,2,4)],mar=rep(0,4)); plot(hawaii(plot=F),type="n",bty="n",xaxt="n",yaxt="n"); hawaii(add=T,xpd=T,...)}
