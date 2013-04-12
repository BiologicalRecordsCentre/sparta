#' Plot GIS shape files
#' 
#' This function can be used to plot gis data that has been loaded from shape files
#' using the \code{readShapePoly()} function contained in the 'maptools' R package.
#'
#' @param gis_data GIS object, or list of objects, to be plotted. Alternativly the name
#'        of the country (or list of names) to be plotted.
#' @param main Text variable controlling the main title placed on the plot - value
#'        needs to be a text string (default == '', title is blank)
#' @param xlab Text variable controlling the label for the x-axis - value needs
#'        to be a text string (default = "", blank)
#' @param ylab Text variable controlling the label for the y-axis - value needs
#'        to be a text string (default = "", blank) 
#' @param xlim Numberical variable setting the lower and upper limits for the
#'        x-axis. xlim can be rounded to nearest grid divison (controlled by grid.div and round.grid) that
#'        allows the specified coordinates to be plotted (default = \code{NULL}, fixs
#'        the limits to the nearest grid line (see grid.divs) that allows all GIS
#'        shapes to be plotted)
#' @param ylim Numberical variable setting the lower and upper limits for the y-axis.
#'        ylim can be rounded to the nearest grid divison (controlled by grid.div and round.grid) that allows the
#'        specified coordinates to be plotted (default = \code{NULL}, fixs the limits to the
#'        nearest grid line (see grid.divs) that allows all gis shapes to be plotted)
#' @param show.axis Logical variable controlling whether axis are plotted
#' @param show.grid Logical variable determining whether to overlay gridlines ontop on
#'        the plot
#' @param grid.div Numerical variable determining the interval between axis ticks and also
#'        the gridlines. Note: when show.grid = \code{TRUE} this value still determines the axes tick
#'        intervals (default = 1)
#' @param round.grid Logical, if TRUE the bounding box is enlarged so that that entire grid
#'        cells are shown (default = FALSE)         
#' @param grid.col Variable determining the colour of the lines used for the overlaid grid.
#'        Value can be text string of a named colour, numerical value, etc, see help for
#'        function 'par' for more details (default = \code{"grey"}).
#' @param fill.col Variable determining the colour used to fill the landmasses plotted.
#'        Value can be text string of a named colour, numerical value, etc, see help for
#'        function 'par' for more details (default = \code{NA}, meaning no fill)
#' @param line.col Variable determining the colour of the line used to plot the landmasses.
#'        Value can be text string of a named colour, numerical value, etc, see help for
#'        function 'par' for more details (default = \code{NULL}, which results in black lines)
#' @param bg.col Variable determining the colour of the background of the plot area. Value
#'        can be text string of a named colour, numerical value, etc, see help for function
#'        'par' for more details (default = \code{"white"})
#' @param box.col Variable determining the colour of the border surrounding the plot area
#'        (default = \code{NA}, no border is added to plot)
#' @param new.window Logical variable determining whether plot should create and plot in
#'        a new window or plot within an existing window/device. NOTE: when new.window =
#'        \code{TRUE} the window dimensions are determined based upon the xlim and ylim
#'        in order to keep the correct aspect ratio and preserve the map shape, if changing
#'        new.window to \code{FALSE} the sizing of the device/window is down the the
#'        user/previous plot (default \code{TRUE})
#' @param no.margin Logical variable determining whether plot should include a margin
#'        NOTE - main title, x-labels and y-labels are written in margin so do not remove
#'        margin if titles/labels are required (default = \code{FALSE}, plot includes a margin). 
#' @param max.dimen Numberical variable determining the maximum window dimension. Variable
#'        determines height or width depend on whether xlim range is greater than ylim range 
#'        (default = \code{13})      
#' @param cex.main Numberical variable determining the relative sizing of the main plot title
#'        (default = \code{1.2})
#' @param cex.lab Numberical variable determining the relative sizing of the axis labels
#'        (default = \code{1})
#' @param cex.axis Numberical variable determining the relative sizing of the axis values
#'        (default = \code{0.8})
#' @param blank.plot Logical variable determining whether function should finish once blank
#'        plot window created or if it should continue further with the plotting (default = 
#'        \code{FALSE}, plotting continues)
#' @param plot.shape Logical variable determining whether to plot lines from shape file
#'        or whether to only setup the background of the plot (i.e grid lines, axis) to
#'        which outline can be added later, useful where outline is to cover plotting 
#'        symbols/colours (default = \code{TRUE})
#' @param additions Logical variable determining whether plot is to be created from scratch
#'        or whether components (i.e. axes, gridlines, outline) are to be added to an existing
#'        plot (default = \code{FALSE})
#' @param additions Logical variable determining whether country polygons (if more than one
#'        is selected) should be dissolved. If they are the boundaries between them are not
#'        shown. Default is \code{FALSE}.
#' @return A data.frame giving the dimensions of the plot area.
#' @keywords GIS, mapping, shapefile
#' @examples
#' \dontrun{
#' # load example shapefile that was created using readShapePoly() from library 'maptools'
#' data(world)
#' 
#' # Example 1
#' # plot with defaults, main plot title set to 'World Map', grid line intervals at units of 10
#' plot.GIS(world, main = "World Map", grid.div = 10)   
#' 
#' # Example 2
#' # define a region to plot using xlim and ylim, colour plot background, and fill landmass
#' plot.GIS(world, main = "World Map", grid.div = 10, xlim = c(-10,20), ylim = c(30,60),
#'          fill.col = "lightgreen", bg.col = "lightblue")
#' 
#' # Example 3
#' # plot with finer scale, black, grid lines, and labelled axes
#' plot.GIS(world, main = "World Map", grid.div = 5, grid.col = 'black', xlab = 'Longitude',
#'          ylab = 'Latitude', xlim = c(-10,20), ylim = c(30,60), fill.col = "lightgreen",
#'          bg.col = "lightblue")
#' 
#' # Example 4
#' # plot Africa without gridlines, axes labels or margins
#' plot.GIS(world, xlab="", ylab="", show.axis = FALSE, show.grid = FALSE, fill.col = "lightgreen",
#' no.margin = TRUE, xlim = c(-20,55), ylim = c(-40,40))
#' 
#' # Example 5
#' # Plot UK with fill colour and background colour but no grid, then add points and labels
#' # highlighting the locations of the capital cities
#' plot.GIS(world, main="UK Capital Cities", ylim = c(48, 60), xlim = c(-10,3), show.grid = FALSE,fill.col = "lightgreen", bg.col = "lightblue")
#' city.x = c(0.1062, -3.2200, -3.1771, -6.2661, -5.9167)
#' city.y = c(51.5171, 55.9500, 51.4780, 53.3428, 54.6000)
#' points(city.x,city.y,pch=16, col="red")    
#' text(city.x[1:2], city.y[1:2], labels = c("London", "Edinburgh"), col="black", pos=1)
#' text(city.x[3], city.y[3], labels = "Cardiff", col="black", pos=3)
#' text(city.x[4:5], city.y[4:5], labels = c("Dublin", "Belfast"), col="black", pos=2)
#' 
#' # Example 6
#' # This plot highlights that when round.grid=TRUE the real xlim and ylim for the plot are determined
#' # by taking user specified values and rounding to the nearest grid division allowing given
#' # coordinates to be plotted 
#' plot.GIS(world, main = "This is Sparta!", xlim=c(15,25), ylim = c(33,43), grid.div = 10,
#' fill.col = "lightgreen", bg.col = "lightblue", round.grid=TRUE)
#' rect(15,33,25,43, border ="red", lty=2)
#' points(x = 22.4303, y = 37.0765, col="red", pch=16, cex = 1.7)
#' text(x = 22.4303, y = 37.0765, labels = "Sparta", pos = 2, cex = 1.5)
#' 
#' # Example 7
#' # Plot the distribution records of Falco subbuteo in the Netherlands using data extracted from the 
#' # Global Biodiversity Information Facility (GBIF).
#' # i) Install and load the rgbif and ropensci packages from github
#' install_github('rgbif', 'ropensci') 
#' require(rgbif)
#' library(rgbif)
#'
#' # ii) Extract Falco subbuteo records from GBIF (use ?occurrencelist for further details)
#' spp<-occurrencelist(scientificname="Falco subbuteo", coordinatestatus = TRUE, originisocountrycode="NL", maxresults=100)
#' }

plot.GIS <-
function(
	gis_data, main = "", 
	xlab = "", 
	ylab = "", 
	xlim = NULL, 
	ylim = NULL, 
	show.axis = TRUE, 
	show.grid = TRUE, 
	grid.div = 1,
  round.grid = FALSE,
	grid.col = "grey",  
	fill.col = NA, 
	line.col = NULL, 
	bg.col = "white", 
	box.col = NA, 
	new.window = TRUE, 
	no.margin = FALSE, 
	max.dimen = 13, 
	cex.main = 1.2, 
	cex.lab = 1, 
	cex.axis = 0.8,
	blank.plot = FALSE,
	plot.shape = TRUE,
	additions = FALSE,
	return.dimen = TRUE,
  dissolve=FALSE
){
  
  required.packages <- c('maptools')
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){
    install.packages(new.packages,dependencies=TRUE)
  } 
    
  if(class(gis_data)=='character'){
    if(!exists('world')) data(world)
    missing<-gis_data[!tolower(gis_data) %in% tolower(world$name)]
    if(length(missing)!=0) stop(paste(missing,'not found in world map'))
    if(length(gis_data!=1) & dissolve==TRUE){
      library(maptools)
      polygons<-world[tolower(world$name) %in% tolower(gis_data),] 
      gis_data<-unionSpatialPolygons(polygons,rep(1, length(polygons)))
    } else {
      gis_data<-world[tolower(world$name) %in% tolower(gis_data),]                                                            
    }                                                  
  }
  
  # Determine dimesions of plot
    if(is.null(xlim)){
      if(is.list(gis_data)){
        for(i in 1:length(gis_data)){
          temp = attributes(gis_data[[i]])$bbox[1,]
          if(i == 1){
            xlim = temp
          } else {
            xlim = c(min(xlim[1], temp[1]), max(xlim[2], temp[2])) 
          }
        }
      } else {
        xlim = attributes(gis_data)$bbox[1,]
      }
      
    }
    if(round.grid) xlim = c(floor(xlim[1]/grid.div) * grid.div, ceiling(xlim[2]/grid.div) * grid.div ) # Round xlim to nearest grid divisions that include xlims specified
    
    if(is.null(ylim)){
      if(is.list(gis_data)){
        for(i in 1:length(gis_data)){
          temp = attributes(gis_data[[i]])$bbox[2,]
          if(i == 1){
            ylim = temp
          } else {
            ylim = c(min(ylim[1], temp[1]), max(ylim[2], temp[2])) 
          }
        }
      } else {
        ylim = attributes(gis_data)$bbox[2,]
      }
      
    }
    if(round.grid) ylim = c(floor(ylim[1]/grid.div) * grid.div, ceiling(ylim[2]/grid.div) * grid.div ) # Round xlim to nearest grid divisions that include xlims specified
  
    fill_col <- fill.col
    line_col <- line.col
    
	x.rat = abs(xlim[1] - xlim[2])
	y.rat = abs(ylim[1] - ylim[2])
	aspect.ratio = x.rat / y.rat
	if(aspect.ratio <= 1){
	  y.part = max.dimen - y.rat
	  x.part = x.rat + (y.part * aspect.ratio)
	  #dev.new(height = max.dimen, width = x.part)
	  plot.dimen = data.frame(height = max.dimen, width = x.part)
	} else {
	  aspect.ratio = 1/aspect.ratio
	  x.part = max.dimen - x.rat
	  y.part = y.rat + (x.part * aspect.ratio)
	  #dev.new(height = y.part, width = max.dimen)
	  plot.dimen = data.frame(height = y.part, width = max.dimen)
	}
		
	# Open new window if new.window == TRUE
    if(new.window | dev.cur() == 1){
		dev.new(height = plot.dimen$height, width = plot.dimen$width)
    }
    
	# If function to be run in additions mode then do not run commands to create blank plot
	if(additions == FALSE){
		if(no.margin){
		  par(mar = c(0,0,0,0))
		}
		 
		# Set up blank plot
		plot(0, 0, xlim=xlim, ylim=ylim, xaxs='i', yaxs='i', xaxt='n', yaxt='n', xlab="",ylab="", type='n', bty="n", asp=1) # Plot covering all of uk
		rect(xlim[1],ylim[1], xlim[2], ylim[2],col=bg.col, border = NA)
	}
    
	# If blank.plot == FALSE then continue and plot gridlines/shape data
	if(blank.plot == FALSE){
	
		# Plot data from shape file(s) if plot.shape == TRUE
		if(plot.shape){
			# Plot GIS data onto plot
			if(is.list(gis_data)){
			  for(i in 1:length(gis_data)){
				plot(gis_data[[i]], add = TRUE, col = fill.col, border = line.col)
			  }
			} else {
			  plot(gis_data, add=TRUE, col = fill.col, border = line.col)
			}
		}
		
	  
		
		# Add Axis and gridlines to plot
		if(show.grid){
		  #abline(h = seq(ylim[1],ylim[2], by = grid.div), v = seq(xlim[1],xlim[2], by=grid.div), col= grid.col)
		  # Horizontal lines
		  segments(x0 = xlim[1], y0 = seq(ylim[1],ylim[2], by = grid.div), x1 = xlim[2], col=grid.col)
		  # Vertical lines
		  segments(x0 = seq(xlim[1], xlim[2], by = grid.div), y0 = ylim[1], y1 = ylim[2], col=grid.col)
		}
		if(show.axis){
		  axis(1, at=seq(xlim[1],xlim[2], by=grid.div), labels = signif(seq(xlim[1],xlim[2],by=grid.div),4), cex.axis = cex.axis)
		  axis(2, at=seq(ylim[1],ylim[2], by=grid.div), labels = signif(seq(ylim[1],ylim[2],by=grid.div),4), cex.axis= cex.axis)
		}
		title(main=main, xlab=xlab, ylab=ylab, cex.main= cex.main, cex.lab= cex.lab)
		
		if(is.na(box.col) == FALSE){
		  box(col = box.col)
		}
	}
	
	if(return.dimen){
		invisible(plot.dimen)
	}
  
  }
