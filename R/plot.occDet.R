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
  
  # Add rhat T/F column
  new_data$rhat_threshold[new_data$Rhat < 1.1] <- 'Good (<1.1)'
  new_data$rhat_threshold[new_data$Rhat > 1.1] <- 'Bad (>1.1)'
  
  ### plot the yearly predicted proportion of occupied sites ###
  # plot with error bars based on 95CI
  ggplot(new_data, aes_string(x = "year", y = "mean"))+#, ...) + 
    theme_bw() +
    geom_ribbon(data = new_data,
                aes_string(group = 1, ymin = "quant_025", ymax = "quant_975"),
                alpha = 0.2) +
    geom_line(size = 1, col = "black") +
    geom_point(size = 2, aes(col = rhat_threshold)) +
    scale_color_manual(name = 'Rhat', values = c('red','blue')) +
    ylab("Occupancy") +
    xlab("Year") +
    scale_y_continuous(limits = c(0, 1)) +
    ggtitle(x$SPP_NAME) + 
    theme(plot.title = element_text(lineheight = .8, face = "bold"),
          legend.position = 'bottom')
}