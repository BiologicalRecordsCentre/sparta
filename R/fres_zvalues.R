fres_zvalues <-
function(f_path){

# Read majority of data (skip 1st line)
trend <- read.table(f_path, header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", "*******", "********", "1.#QO"), comment.char = "", sep=',')

# Run the Z-test
x<-z_value(trend=trend)

return(x)
}
