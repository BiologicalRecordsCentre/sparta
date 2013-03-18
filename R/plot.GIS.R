plot.GIS <-
function(
	gis_data, main = "", 
	xlab = "Easting (km)", 
	ylab="Northing (km)", 
	xlim = NULL, 
	ylim = NULL, 
	show.axis = TRUE, 
	show.grid = TRUE, 
	grid.div = 100000, 
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
	return.dimen = TRUE
){
  
  if(!exists('UK')) data(UK)
  
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
    xlim = c(floor(xlim[1]/grid.div) * grid.div, ceiling(xlim[2]/grid.div) * grid.div ) # Round xlim to nearest grid divisions that include xlims specified
    
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
    ylim = c(floor(ylim[1]/grid.div) * grid.div, ceiling(ylim[2]/grid.div) * grid.div ) # Round xlim to nearest grid divisions that include xlims specified
  
    fill_col <- fill.col
    line_col <- line.col
    
	x.rat = abs(xlim[1] - xlim[2])/100000
	y.rat = abs(ylim[1] - ylim[2])/100000
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
		  axis(1, at=seq(xlim[1],xlim[2], by=grid.div), labels = seq(xlim[1]/1000,xlim[2]/1000,by=grid.div/1000), cex.axis = cex.axis)
		  axis(2, at=seq(ylim[1],ylim[2], by=grid.div), labels = seq(ylim[1]/1000,ylim[2]/1000,by=grid.div/1000), cex.axis= cex.axis)
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
