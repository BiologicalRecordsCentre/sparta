#' Plot GIS shape files
#' 
#' This function can be used to plot gis data that has been loaded from shape files
#' using the \code{readShapePoly()} function contained in the 'maptools' R package.
#'
#' @param gis_data GIS object, or list of objects, to be plotted.
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
#' @param set.margin Logical variable. If \code{TRUE} (default), the margins are set to be of
#'        equal size. If \code{FALSE} the margins are used as set in the R environment.
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
#' @param return.dimen logical, if \code{TRUE} the plot dimensions are returned
#' @return A data.frame giving the dimensions of the plot area.
#' @keywords GIS, mapping, shapefile


plot_GIS <-
function(
	gis_data=NULL, main = "", 
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
	set.margin = TRUE,
	max.dimen = 13, 
	cex.main = 1.2, 
	cex.lab = 1, 
	cex.axis = 0.8,
	blank.plot = FALSE,
	plot.shape = TRUE,
	additions = FALSE,
	return.dimen = TRUE
  ){
    
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
	  x.part = max.dimen - x.rat
	  y.part = y.rat + (x.part * (1/aspect.ratio))
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
		} else if(set.margin){
		    if(aspect.ratio >= 1){
  		    par(mar = c(4,4*aspect.ratio,4,4*aspect.ratio))
  		  } else {
  		    par(mar = c(4*1/aspect.ratio,4,4*1/aspect.ratio,4))
  		  }
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
