#' Simple theme to be used with FNIDR (TTR)
#'
#' Simple theme for TTR pictures with common look
#' @param base_size Base size for the fonts
#' @param base_family Base font family
#' @param base_col_num Colour number (from thlcolbynum)
#' @param other_col_num Colour used in effects (from thlcolbynum)
#' @param debug Should debugging information be included in the graphs
#' @export
theme_ttr2015 <- function(base_size = 11, base_family = "",base_col_num=29,other_col_num=2,debug=FALSE,axis.text=1,xaxis.mid=FALSE)  {
  half_line <- base_size/2
  local_grey<-thlcolbynum(base_col_num)
  local_other<-thlcolbynum(other_col_num)
  theme(line = element_line(colour = local_other, size = 0.5, linetype = 1,
                            lineend = "butt"),
        rect = element_rect(fill = "white",colour=local_grey, size = 0.5, linetype = 1),
        text = element_text(family = base_family, face = "plain", colour = local_grey,
                            size = base_size, lineheight = 0.9,
                            hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
        axis.line = element_blank(),
        axis.text = element_text(size = rel(axis.text)),
        axis.ticks.length = unit(half_line/2, "pt"),
        axis.title.x = element_text(face="bold", # hjust=1,vjust=1,
                                    margin = margin(t = 0.8*half_line, b = 0.8 * half_line/2),
                                    debug=debug),
        axis.title.y = element_text(face="bold", angle=90,# angle = 0,hjust=-1,vjust=1,
                                    margin = margin(r = 0.8 * half_line,l = 0.8 * half_line/2),
                                    debug=debug),        
        legend.background = element_rect(colour = NA),
        legend.margin = margin(0.2,0.2,0.2,0.2, "cm"),
        legend.key = element_rect(fill = "white",colour = "white"),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(1.2)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0),
        legend.title.align = NULL,
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = NULL,
        panel.background = element_rect(fill = NA, colour = NA),
        panel.border = element_rect(fill=NA,colour=NA),#local_grey),
        panel.grid.major = element_line(colour = local_grey,size=.5),
        panel.grid.major.x = if( xaxis.mid) element_line(colour=NA) else element_line(colour = local_grey,size=.5),
        panel.grid.minor = element_line(colour = NA),#, size = 0.25),
        panel.grid.minor.x = if(!xaxis.mid) element_line(colour=NA) else element_line(colour = local_grey,size=.5),
        panel.spacing = unit(1*half_line, unit="pt"),
#        panel.margin.x = NULL,
#        panel.margin.y = NULL,
        panel.ontop = FALSE,
        strip.background = element_rect(fill = "white",colour = local_grey),
        strip.text = element_text(colour = local_grey,size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line,b = half_line)),
        strip.text.y = element_text(angle = -90,margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_rect(colour = NA,fill= NA),
        plot.title = element_text(size = rel(1.5),colour = local_other,face="bold",hjust=0,
                                  margin = margin(b = half_line *1.2)),
        plot.margin = margin(half_line, half_line,half_line, half_line),
        complete = TRUE)
}
#' @describeIn theme_ttr2015 Theme for maps
#' @export
theme_ttr2015map<-function (legend=TRUE,base_size = 11, base_family = "",base_col_num=29,other_col_num=2,debug=FALSE)  {
  half_line <- base_size/2
  local_grey<-thlcolbynum(base_col_num)
  local_other<-thlcolbynum(other_col_num)
  if (legend) {
      return(theme(text = element_text(family = base_family, face = "plain", colour = local_grey,
                                       size = base_size, lineheight = 0.9,
                                       hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),                   
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.line = element_blank(),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   legend.background = element_rect(colour = NA),
                   legend.margin = margin(0.2,0.2,0.2,0.2, "cm"),
                   legend.key = element_rect(fill = "white",colour = "white"),
                   legend.key.size = unit(1.2, "lines"),
                   legend.key.height = NULL,
                   legend.key.width = NULL,
                   legend.text = element_text(size = rel(1.2)),
                   legend.text.align = 1,
                   legend.title = element_text(hjust = 0),
                   legend.title.align = NULL,
                   legend.position = c(0.33,.4),
                   legend.direction = NULL,
                   legend.justification = "right",
                   legend.box = NULL,
                   axis.ticks.length = unit(0, "cm"),
                   panel.spacing = unit(0, "lines"),
                   plot.title = element_text(size = rel(1.5),colour = local_other,face="bold",hjust=0,
                                             margin = margin(b = half_line *1.2)),
                   plot.margin = margin(0, 0, 0, 0, "lines"),
                   complete = TRUE))
    }
    else {
        return(theme(text = element_text(family = base_family, face = "plain", colour = local_grey,
                                       size = base_size, lineheight = 0.9,
                                       hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),                   
                     line = element_blank(),
                     rect = element_blank(), 
                     text = element_blank(),
                     axis.ticks.length = unit(0, "cm"),
                     legend.position = "none",
                     panel.spacing = unit(0, "lines"),
                     plot.margin = margin(0, 0, 0, 0, "lines"), 
                     plot.title = element_text(size = rel(1.5),colour = local_other,face="bold",hjust=0,
                                               margin = margin(b = half_line *1.2)),
                     complete = TRUE))
    }
}
