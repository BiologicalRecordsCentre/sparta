#' Plot occDet Objects
#' 
#' @param x An object of class occDet
#' @param y not used
#' @param main The plot title, defaults to the species name
#' @param reg_agg The name of a region or region aggregate to plot.
#' If '' (default) then the overall occupancy estimates are plotted
#' @param ... Additional arguments passed to ggplot

#' @import ggplot2
#' @method plot occDet
#' @export

plot.occDet <- function(x, y = NULL, main = x$SPP_NAME, reg_agg = '', ...){
    
  # gets summary output from the BUGS files 
  spp_data <- as.data.frame(x$BUGSoutput$summary)
  
  if(reg_agg != '') reg_agg <- paste0('.r_', reg_agg)
  
  # get rows we are interested in
  ### take psi.fs rows - these are the yearly proportion of occupied cells ###
  spp_data$X <- row.names(spp_data)
  new_data <- spp_data[grepl(paste0("^psi.fs", reg_agg, "\\["),spp_data$X),]
  new_data$year <- (Year = (x$min_year - 1) + as.numeric(gsub(paste0("psi.fs", reg_agg), "", gsub("\\[|\\]","", row.names(new_data)))))
  
  # rename columns, otherwise ggplot doesn't work properly    
  names(new_data) <- gsub("2.5%","quant_025", names(new_data))
  names(new_data) <- gsub("97.5%","quant_975", names(new_data))
  
  # Add rhat T/F column
  new_data$rhat_threshold[new_data$Rhat < 1.1] <- 'Good (<1.1)'
  new_data$rhat_threshold[new_data$Rhat > 1.1] <- 'Bad (>1.1)'
  
  ### plot the yearly predicted proportion of occupied sites ###
  # plot with error bars based on 95CI
  ggplot(new_data, aes_string(x = "year", y = "mean"), ...) + 
    theme_bw() +
    geom_ribbon(data = new_data,
                aes_string(group = 1, ymin = "quant_025", ymax = "quant_975"),
                alpha = 0.2) +
    geom_line(size = 1, col = "black") +
    geom_point(size = 4, aes(col = rhat_threshold)) +
    scale_color_manual(name = 'Rhat', values = c('Bad (>1.1)' = 'red','Good (<1.1)' = 'blue')) +
    ylab("Occupancy") +
    xlab("Year") +
    scale_y_continuous(limits = c(0, 1)) +
    ggtitle(main) + 
    theme(plot.title = element_text(lineheight = .8, face = "bold"),
          legend.position = 'bottom')
}