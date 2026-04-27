install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("xml2")
install.if("rsvg")
install.if("magick")
install.if("dplyr")

library(xml2)
library(rsvg)
library(magick)
library(dplyr)

color_add=function(pal,n=1){
  pal0=pal[pal!="#FFFFFF" & pal!="#000000"]; z=length(pal0); p=c()
  if(z>0) p=colorRampPalette(c("white",pal0[1],"black"))(5)[2]
  if(z>1) p=c(p,colorRampPalette(c("white",pal0[2],"black"))(5)[2])
  if(z>0) p=c(p,colorRampPalette(c("white",pal0[1],"black"))(5)[4])
  if(z>1) p=c(p,colorRampPalette(c("white",pal0[2],"black"))(5)[4])
  c(pal,p,sample(gray(3:7/10)))[1:(length(pal)+n)]
}

default_fill <- function(doc, fill = "#000000") {nodes <- xml_find_all(doc, "//*[not(@fill)]");xml_set_attr(nodes, "fill", fill); return(doc)}
list_colors=function(doc){regmatches(as.character(doc),gregexpr("#[0-9a-fA-F]{3,6}",doc)) %>% unlist %>% unique %>% toupper}

color_swap=function(svg_file,new_palette,old_palette=NULL,save_file_as="image_recolored.svg"){
  doc0 <- read_xml(svg_file); doc=doc0 %>% default_fill
  if(is.null(old_palette)){
    colors <- list_colors(doc)
    img <- image_read_svg(svg_file, width = 1000, height = 1000)
    ix=c(); for(j in 1:length(colors)){dat=img %>% image_transparent(colors[j],fuzz=5) %>% .[[1]] %>% as.numeric; ix[j]=sum(dat[,,4]==0) }
    old_palette=colors[order(-ix)]
  }
  old_n=length(old_palette); new_n=length(new_palette)
  if(old_n>new_n) new_palette=color_add(new_palette,old_n-new_n)
  if(old_n<new_n) new_palette=new_palette[1:old_n]
  for(u in 1:length(old_palette)) doc=gsub(old_palette[u],new_palette[u],doc,ignore.case=T)
  
  doc %>% as_xml_document %>% write_xml(save_file_as)
}
