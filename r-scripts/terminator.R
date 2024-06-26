# ADDS A TERMINATOR TO A MAP TO SHOW DAYTIME / NIGHTTIME REGIONS
# Returns a dataframe of latitude and longitude for the line that separates illuminated day and dark night for any given time
# This is just a port of the Javascript Leaflet.Terminator plugin (https://github.com/joergdietrich/Leaflet.Terminator/blob/master/L.Terminator.js)

install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("plyr")
install.if("dplyr")

library(plyr)
library(dplyr)

rad2deg <- function(rad) {
  (rad * 180) / (pi)
}

deg2rad <- function(deg) {
  (deg * pi) / (180)
}

getJulian <- function(time) {
  # get Julian day (number of days since noon on January 1, 4713 BC; 2440587.5 is number of days between Julian epoch and UNIX epoch)
  (as.integer(time) / 86400) + 2440587.5
}

getGMST <- function(jDay) {
  # calculate Greenwich Mean Sidereal Time
  d <- jDay - 2451545.0
  (18.697374558 + 24.06570982441908 * d) %% 24
}

sunEclipticPosition <- function(jDay) {
  # compute the position of the Sun in ecliptic coordinates
  # days since start of J2000.0
  n <- jDay - 2451545.0
  # mean longitude of the Sun
  L <- 280.460 + 0.9856474 * n
  L = L %% 360
  # mean anomaly of the Sun
  g <- 357.528 + 0.9856003 * n
  g = g %% 360
  # ecliptic longitude of Sun
  lambda <- L + 1.915 * sin(deg2rad(g)) + 0.02 * sin(2 * deg2rad(g))
  # distance from Sun in AU
  R <- 1.00014 - 0.01671 * cos(deg2rad(g)) - 0.0014 * cos(2 * deg2rad(g))
  
  data.frame(lambda, R)
}

eclipticObliquity <- function(jDay) {
  # compute ecliptic obliquity
  n <- jDay - 2451545.0
  # Julian centuries since J2000.0
  T <- n / 36525
  # compute epsilon
  23.43929111 -
    T * (46.836769 / 3600
         - T * (0.0001831 / 3600
                + T * (0.00200340 / 3600
                       - T * (0.576e-6 / 3600
                              - T * 4.34e-8 / 3600))))
}

sunEquatorialPosition <- function(sunEclLng, eclObliq) {
  # compute the Sun's equatorial position from its ecliptic position
  alpha <- rad2deg(atan(cos(deg2rad(eclObliq)) *
                          tan(deg2rad(sunEclLng))))
  delta <- rad2deg(asin(sin(deg2rad(eclObliq)) 
                        * sin(deg2rad(sunEclLng))))
  
  lQuadrant  = floor(sunEclLng / 90) * 90
  raQuadrant = floor(alpha / 90) * 90
  alpha = alpha + (lQuadrant - raQuadrant)
  
  data.frame(alpha, delta)
}

hourAngle <- function(lng, sunPos, gst) {
  # compute the hour angle of the sun for a longitude on Earth
  lst <- gst + lng / 15
  lst * 15 - sunPos$alpha
}

longitude <- function(ha, sunPos) {
  # for a given hour angle and sun position, compute the latitude of the terminator
  rad2deg(atan(-cos(deg2rad(ha)) / tan(deg2rad(sunPos$delta))))
}

terminator <- function(time, from = -180-360, to = 180+360, by = 1) {
  # calculate latitude and longitude of terminator within specified range using time (in POSIXct format, e.g. `Sys.time()`)
  jDay = getJulian(time)
  gst = getGMST(jDay)
  
  sunEclPos = sunEclipticPosition(jDay)
  eclObliq = eclipticObliquity(jDay)
  sunEqPos = sunEquatorialPosition(sunEclPos$lambda, eclObliq)
  
  df=lapply(seq(from, to, by), function(i) {
    ha = hourAngle(i, sunEqPos, gst)
    lon = longitude(ha, sunEqPos)
    data.frame(long = i, lat=lon)
  }) %>%
    plyr::rbind.fill()

bo=as.POSIXct(as.numeric(time),origin="1970-1-1"); attr(bo,"tzone")="UTC"
bo=as.POSIXct(format(bo,"2000-%m-%d %H:00:00"))
df$night=ifelse(bo>"2000-03-20 21:00:00" & bo<"2000-09-23 07:00:00",-1,1)
df
}


shade=function(time,from=-180-360,to=180+360,by=1,day=F,plot=T,...){
  a=terminator(time,from,to,by)
if(!day) b=data.frame(long=c(a$long,to,from,a$long[1]),lat=c(a$lat,c(150,150)*a$night[1],a$lat[1]))
 if(day) b=data.frame(long=c(a$long,to,from,a$long[1]),lat=c(a$lat,c(-150,-150)*a$night[1],a$lat[1]))
if(plot) polygon(b$long,b$lat,border=NA,xpd=T,...) else return(b)}


