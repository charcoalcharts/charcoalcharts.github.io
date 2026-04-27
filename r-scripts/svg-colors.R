install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("xml2")
install.if("rsvg")
install.if("magick")
install.if("dplyr")
install.if("stringr")
install.if("purrr")

library(xml2)
library(rsvg)
library(magick)
library(dplyr)
library(stringr)
library(purrr)

get_svg_pallet=function(svg_file){
  doc <- read_xml(svg_file)
  nodes <- xml_find_all(doc, "//*")
    get_attr_safe <- function(node, attr) {
      val <- xml_attr(node, attr)
      ifelse(is.na(val),"#000000", val)
    }
    fills   <- map_chr(nodes, get_attr_safe, "fill")
    strokes <- map_chr(nodes, get_attr_safe, "stroke")
    colors <- c(fills, strokes) %>% unique %>% toupper
    img <- image_read_svg(svg_file, width = 1000, height = 1000)
    ix=c(); for(j in 1:length(colors)){dat=img %>% image_transparent(colors[j],fuzz=5) %>% .[[1]] %>% as.numeric; ix[j]=sum(dat[,,4]==0) }
    colors[order(ix)]
}

color_swap=function(svg_file,new_palette,old_palette=NULL,save_file_as="image_recolored.svg"){
doc <- read_xml(svg_file)
nodes <- xml_find_all(doc, "//*")
if(is.null(old_palette)){
  get_attr_safe <- function(node, attr) {
    val <- xml_attr(node, attr)
    ifelse(is.na(val),"#000000", val)
  }
  fills   <- map_chr(nodes, get_attr_safe, "fill")
  strokes <- map_chr(nodes, get_attr_safe, "stroke")
  colors <- c(fills, strokes) %>% unique %>% toupper
  img <- image_read_svg(svg_file, width = 1000, height = 1000)
  ix=c(); for(j in 1:length(colors)){dat=img %>% image_transparent(colors[j],fuzz=5) %>% .[[1]] %>% as.numeric; ix[j]=sum(dat[,,4]==0) }
  old_palette=colors[order(ix)]
}
old_palette=old_palette %>% tolower
#old_palette <- c("#3EC1CC", "#000000", "#AA182C") %>% tolower
#new_palette <- c("#FF0990", "#00FF00", "#0000FF") %>% tolower
new_palette=new_palette %>% tolower

color_map <- setNames(new_palette, old_palette)

replace_color <- function(val, attr = c("fill", "stroke")) {
  attr <- match.arg(attr)
  
  # Normalize input
  val_norm <- tolower(trimws(val))
  
  # Handle implicit fill = black
  if (is.na(val_norm) && attr == "fill") {
    val_norm <- "#000000"
  }
  
  # Skip NA stroke
  if (is.na(val_norm)) return(val)
  
  # Skip "none"
  if (val_norm == "none") return(val)
  
  # Replace if mapped
  if (val_norm %in% names(color_map)) {
    return(color_map[[val_norm]])
  }
  
  return(val)
}


for (node in nodes) {
  fill_val <- xml_attr(node, "fill")
  stroke_val <- xml_attr(node, "stroke")
  
  # --- FILL (always evaluate to catch implicit black) ---
  new_fill <- replace_color(fill_val, "fill")
  
  # Write fill if:
  # - it existed OR
  # - it was implicit black and got changed
  if (!is.na(fill_val) || (!is.na(new_fill) && new_fill != "#000000")) {
    xml_set_attr(node, "fill", new_fill)
  }
  
  # --- STROKE (only if present) ---
  if (!is.na(stroke_val)) {
    xml_set_attr(node, "stroke", replace_color(stroke_val, "stroke"))
  }
}

write_xml(doc,save_file_as)
}
