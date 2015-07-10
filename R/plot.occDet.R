#' @import ggplot2
#' @method plot occDet
#' @export

plot.occDet <- function(x, y = NULL, ...){
    
  # gets summary output from the BUGS files 
  spp_data <- as.data.frame(x$BUGSoutput$summary)
  
  # get rows we are interested in
  ### take psi.fs rows - these are the yearly proportion of occupied cells ###
  spp_data$X <- row.names(spp_data)
  new_data <- spp_data[grepl("psi.fs",spp_data$X),]
  new_data$year <- (Year = (x$min_year - 1) + as.numeric(gsub("psi.fs", "", gsub("\\[|\\]","", row.names(new_data)))))
  
  # rename columns, otherwise ggplot doesn't work properly    
  names(new_data) <- gsub("2.5%","quant_025", names(new_data))
  names(new_data) <- gsub("97.5%","quant_975", names(new_data))
  
  ### plot the yearly predicted proportion of occupied sites ###
  # plot with error bars based on 95CI
  ggplot(new_data, aes_string(x = "year", y = "mean"), ...) + 
    theme_bw() +
    geom_ribbon(data = new_data,
                aes_string(group = 1, ymin = "quant_025", ymax = "quant_975"),
                alpha = 0.2) +
    geom_line(size=1, col="red") +
    geom_point(size=2, col="red") +
    theme(legend.position = "none") +
    ylab("Occupancy") +
    xlab("Year") +
    scale_y_continuous(limits = c(0, 1)) +
    ggtitle(x$SPP_NAME) + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
}