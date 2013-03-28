plot.frescalo<-function(x){
  
  options(device = "windows") #this is fix for windows for Rstudio's 1 device rule
  
  if('lm_stats' %in% names(x)) lm_stats<-x$lm_stats
  stats<-x$stat
  
  dev.new()
  
  # Setup outer margin area
  par(oma = c(1,1,1,1))
  
  # Subdivide device into 4 figures
  layout(matrix(c(1,2,3,4), ncol = 2, nrow = 2, byrow = TRUE))
    
  # Setup margins
  par(mar = c(2,0,0,0))
  
  # No. Species
  map_data_R(gridrefs = stats$Location, attribute = stats$No_spp, breaks = cat_breaks(stats$No_spp, n_cat = 10, whole_breaks = TRUE), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft", leg_cex = 0.7, sq_border = NA, bg.col = NULL,shape_data = UK, xlab='',ylab='')
  mtext("(a) No. Species", adj = 0.05, font = 1, cex = 0.7)
  
  # Rescaled No. Species
  map_data_R(gridrefs = stats$Location, attribute = stats$Spnum_out, breaks = cat_breaks(stats$Spnum_out, n_cat = 10, whole_breaks = TRUE), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft",leg_cex = 0.7,  sq_border = NA, bg.col = NULL,shape_data = UK, xlab='',ylab='')
  mtext("(b) Rescaled No. Species", adj = 0.05, font = 1, cex = 0.7)
  
  # Alpha (Derivied Breaks)
  
  map_data_R(gridrefs = stats$Location, attribute = stats$Alpha, breaks = cat_breaks(stats$Alpha, n_cat = 10, whole_breaks = FALSE, rnd_digits = 2), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft", leg_cex = 0.7,  sq_border = NA, bg.col = NULL,shape_data = UK, xlab='',ylab='')
  mtext("(c) Alpha (Derived Breaks)", adj = 0.05, font = 1, cex = 0.7)
  
  par(mar=c(4,4,0,0))
  
  # histogram of trends
  if('lm_stats' %in% names(x)){
    density_line<-density(lm_stats$b)
    hist(lm_stats$b,main='',xlab='Trend',ylab='Frequency',ylim=c(0,max(density_line$y)),cex.axis=0.7,prob=TRUE)
    abline(v=0,col='blue')
    lines(density_line,col='red')     
    mtext("(d) Histogram of species trends", adj = 0.05, font = 1, cex= 0.7)  
  }
}