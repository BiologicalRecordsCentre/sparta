fres_zvalues <-
function(f_path){

# Read majority of data (skip 1st line)
trend <- read.fwf(f_path, widths=c(9,12,8,7,7,7,7,7,7), header=FALSE, stringsAsFactors = FALSE, strip.white = TRUE, skip = 1, na.strings = c("NA", "*******", "********", "1.#QO"), comment.char = "")
# Read in column names from 1st line
names(trend) <- read.table(f_path, skip = 0, nrows = 1, stringsAsFactors = FALSE)[1,]

x<-z_value(trend=trend)

return(x)
}
