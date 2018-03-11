#' Source code for the project
#' @import dplyr
#' @import readr
#' @import stringr
#' @import ggplot2
#' @import grid
#' @import leaflet


#'
#' @name eq_location_clean
#' @title clean location in raw data
#' @description  This function is used in order to change name of Location in database to more convinient format: it keeps only Country name in Titlecase
#' @param location column of dataframe or simply a vector with names of locations to be changed
#' @return the vector of Country names in title case is returned
#' @examples \dontrun{eq_location_clean<-(location=database$COUNTRY)}
#' @export
eq_location_clean <- function(location){cleaned_location<-paste0(stringr::str_to_title(location), ":")
                                          cleaned_location}
#'

#'
#' @name eq_clean_data
#' @title clean entire dataset
#' @description  This function "cleans" the initial dataset so it can be further used in geoms: creates date column in POSTIXct format with given Year, Month and Day,
#' as well as convert LONGITUDE and LATITUDE to numeric and clean location name using previous function [func(eq_location_clean)]. Because the cleaning task is
#' quite specific to the current database, there is no option to choose columns, related to dates and coordinates. Instead, all of them are nested in the body of function
#' thus, the only arument is the name of \code{data.frame} in which base is stored
#' @param data a data frame that contains the imported base
#' @return cleaned dataframe is returned with country names in title case and coordinates as numeric insread of character
#' @examples \dontrun{eq_clean_data(database)->cleaned_base}
#' @export
      eq_clean_data<-function(data){
  data$DATE<-as.POSIXct(strptime("1994-02-03", format="%Y-%m-%d"))
  data$DATE[data$YEAR<0]<-as.POSIXct(strptime(paste(data$YEAR[data$YEAR<0], data$MONTH[data$YEAR<0],data$DAY[data$YEAR<0],sep="-"),"-%Y-%m-%d"))
  data$DATE[data$YEAR>=0]<-as.POSIXct(strptime(paste(data$YEAR[data$YEAR>=0], data$MONTH[data$YEAR>=0],data$DAY[data$YEAR>=0],sep="-"),"%Y-%m-%d"))

  data$LONGITUDE<-as.numeric(gsub(" ", "", data$LONGITUDE))
  data$LATITUDE<-as.numeric(gsub(" ", "", data$LATITUDE))
  data$LOCATION_NAME<-eq_location_clean(data$COUNTRY)
  data$DEATHS<-as.numeric(gsub(" ", "", data$DEATHS))
  data
}




##############################Module2 Part one#################################

#'
#' @name draw_key_timeline
#' @title function to identify legend
#' @description This is function that is used in ggproto to make custom legend type in new geom_*.
#'  @inheritParams ggplot2::draw_key_polygon
#' @export
    draw_key_timline<-function(data, params, size){
  lwd <- min(data$size, min(size)/4)
  elem2<-circleGrob(x=0.6, y=0.5, r=data$size/10, gp=gpar(col=data$colour, fill=alpha(data$fill, data$alpha)))
  result <- gTree(children = gList(elem2))
  result

}



#'
#' @name GeomTimeline
#' @title function to build ggproto for geom
#' @description Here will be the usage of two functions, required to create custom geom: ggplot2:ggproto and geom_* (in this case geom_hurricane) in order to
#' create new geom_timeline, which shows magnitude and year of hurricane occured
#' @inheritParams ggplot2::ggproto
#' @examples \dontrun{this function needed to geom_timeline works. Is not called by user directly}
#' @export
    GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                                  required_aes = c("x"),
                                  optional_aes=c("size"),
                                  non_missing_aes = c("fill", "colour", "y"),
                                  default_aes = aes(shape = 19, colour = "black", fill = "black", size = 0.15,
                                                    linetype = 1, alpha = 0.45, fontsize = 1, y=0.5),
                                  draw_key = draw_key_timline,
                                  draw_panel = function(data, panel_scales, coord) {
                                  coords<-coord$transform(data, panel_scales)
                                  coords$size<-coords$size/50
                                  coords<-coords[!is.na(coords$size),]
                                  grid::pointsGrob(x=coords$x, y=coords$y, pch = coords$shape,size=unit(coords$size, "native"),
                                                   gp = grid::gpar(col = coords$colour, fill = coords$fill, alpha=coords$alpha))
                                  }
                                  )

#'
#' @name geom_timeline
#' @title custom geom to plot data
#' @description Geom to draw timeline
#' @inheritParams ggplot2::geom_point
#'@examples \dontrun{cleaned_base%>%filter(YEAR>=2000 & YEAR<=2010 & !is.na(EQ_MAG_ML))%>%ggplot()+
#'    geom_timeline(aes(fill=DEATHS, colour=DEATHS, x=YEAR, size=EQ_MAG_ML))+theme_timeline}
#' @export
    geom_timeline<- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = FALSE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#ggplot(test_df)+geom_timeline(aes(x=Year, y=Country, size=ML))+theme_classic()





##############################Module2 Part two#################################


#'
#' @name theme_timeline
#' @title modification of existing geom classic theme
#' @description Here is slight modification of existing classic theme: everything is the same except default position of the legend: it is at the bottom, instead of right side
#' @inheritParams ggplot2::theme_classic
#' @export
    theme_timeline<-theme_classic() %+replace% theme(legend.position="bottom")


#'
#' @name GeomTimeline_label
#' @title ggproto for custom geom
#' @description It is modification of previous \code{geom_timeline} that can add captions to hurricanes plotted by date occured. Captions can be applyed to all observation,
#' or to first n observations, having maximum value of specified metric (for example, first 5 with the highest magnitude level)
#' @inheritParams ggplot2::ggproto
#' @examples \dontrun{This function is needed to make geom_timeline_label works. Is not called by user directly}
#' @export
    GeomTimeline_label <- ggplot2::ggproto("GeomTimeline_label", Geom,
                                 required_aes = c("x"),
                                 optional_aes=c("size", "n_max", "caption"),
                                 non_missing_aes = c("fill", "colour", "y"),
                                 default_aes = aes(shape = 19, colour = "black", fill = "black", size = 0.1,
                                                   linetype = 1, alpha = 0.25, fontsize = 1, y=0.5),
                                 draw_key = draw_key_timline,
                                 draw_panel = function(data, panel_scales, coord) {
                                   coords<-na.omit(coord$transform(data, panel_scales))
                                   if(max(coords$size)>=1){coords$size<-((coords$size-min(coords$size))/(max(coords$size)-min(coords$size)))*0.1}
                                   rank<-coords$size
                                   rank[!is.na(rank)]<-rank(coords$size[!is.na(coords$size)], ties.method="min")
                                   rank<-max(rank, na.rm=TRUE)+min(rank, na.rm=TRUE)-rank
                                   points<-grid::pointsGrob(x=coords$x, y=coords$y, pch = 21,size=unit(coords$size, "native"),
                                                    gp = grid::gpar(col = coords$colour, fill = coords$fill, alpha=coords$alpha, fontsize=coords$fontsize))
                                   coords<-coords[(!is.na(rank)&rank<=coords$n_max[1]) ,]
                                   print(coords)
                                   if(!is.null(coords$caption) | !is.null(coords$size)){
                                                ticks<-grid::segmentsGrob(x0=unit(coords$x, "npc"), x1=unit(coords$x, "npc"),
                                                                          y0=unit(coords$y, "npc"), y1=unit(coords$y+0.2, "npc"));
                                                text<-grid::textGrob(label=coords$caption, x=unit(coords$x, "npc"), y=unit(coords$y+0.23, "npc"), rot=45)
                                                } else {ticks<-NULL;text<-NULL}
                                   result <- gTree(children = gList(points, ticks, text))
                                   result

                                 }
)


#'
#' @name geom_timeline_label
#' @title custom geom with labels
#' @description Geom to draw timeline label
#' @inheritParams ggplot2::geom_point
#' @examples \dontrun{cleaned_base%>%filter(YEAR>=2000 & YEAR<=2004 & !is.na(EQ_MAG_ML))%>%ggplot()+
#'    geom_timeline_label(aes(fill=DEATHS, colour=DEATHS, x=YEAR, y=COUNTRY, caption=COUNTRY, size=EQ_MAG_ML, n_max=2))+
#'      theme_timeline}
#' @export
    geom_timeline_label<- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = FALSE, ...) {
  ggplot2::layer(
    geom = GeomTimeline_label, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#ggplot(test_df)+geom_timeline_label(fill="blue",
#                                    aes(fill="blue",x=Year, y=Country, caption=Country, size=Intensity, n_max=5))+theme_timeline

#ggplot(test_df)+
#  geom_timeline_label(aes(fill=Deaths, x=Year, y=Country, caption=Country, size=Intensity, n_max=2))+
#  theme_timeline+
#  scale_colour_gradient(low = "#132B43", high = "#56B1F7")

#############################################Module 3#######################

#'
#' @name eq_create_label
#' @title Function to generate leaflet popup text
#' @description Function to automate creating popup labels in leaflet using html tags
#'@param data data used
#'@param location location
#'@param magnitude intensity
#'@param death numbet of deaths occured
#'@examples \dontrun{cleaned_base$popup_text<-eq_create_label(cleaned_base, "LOCATION", "EQ_MAG_ML", "DEATHS")}
#'@export
    eq_create_label<-function(data, location, magnitude, death){
  data<-as.data.frame(data)

  xxx<-function(x, argument){
    if(is.na(x)){x=""} else {x=paste0("<b>",argument, "</b> ",x, "<br>")}
    x
  }

  Location_caption<-sapply(data[,colnames(data)==location], xxx, argument="Location")
  Magnitude_caption<-sapply(data[,colnames(data)==magnitude], xxx, argument="Magnitude")
  Death_caption<-sapply(data[,colnames(data)==death], xxx, argument="Deaths")

  Whole_popup<-paste0(Location_caption,Magnitude_caption,Death_caption)

  Whole_popup}

#'
#' @name eq_map
#' @title create interactive plots based on leaflet
#' @description Function to plot dots of hurricanes on ineractive maps with popus containing basic information
#' @param data data used
#' @param longitude x-coord
#' @param latitude y-cood
#' @param annot_col column to extract popup caption from
#' @examples \dontrun{eq_map(cleaned_base, "LONGITUDE", "LATITUDE", "popup_text")}
#' @export
eq_map<-function(data, longitude, latitude, annot_col){
    keep<-c(longitude, latitude, annot_col)
    data<-data[,colnames(data) %in% keep]
    data<-as.data.frame(na.omit(data))
    print(head(data))
    caption<-eq_create_label(data, "Country", "Intensity", "Deaths")
    m = leaflet() %>% addTiles()%>%
    addCircleMarkers(lng=data[,colnames(data)==longitude], lat=data[,colnames(data)==latitude])%>%
    addPopups(lng=data[,colnames(data)==longitude], lat=data[,colnames(data)==latitude], popup=data[,colnames(data)==annot_col] )
    m
}

#eq_map(test_df, longitude = "Longitude", latitude="Latitude")

#eq_create_label(test_df, "Country", "Intensity", "Deaths")

#test_df%>%dplyr::mutate(popup_text = eq_create_label(.,location="Country", magnitude="Intensity", death="Deaths"))%>%
#           eq_map(longitude="Longitude", latitude="Latitude", annot_col = "popup_text")
