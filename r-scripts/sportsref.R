install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("httr")
install.if("rvest")

library(httr)
library(rvest)

read_html2=function(url){Sys.sleep(runif(1,3,7)); ua=paste(c(sample(LETTERS,10,T),sample(10000,10,T)),collapse=""); GET(url,user_agent(ua)) %>% read_html}
read_sportsref=function(url){Sys.sleep(runif(1,3,7)); ua=paste(c(sample(LETTERS,10,T),sample(10000,10,T)),collapse=""); GET(url,user_agent(ua)) %>% read_html}
