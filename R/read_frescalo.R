read_frescalo <-
function(stat_path, trend_path, freq_path,
         initial_dir = "~/",
         sep = NULL){

		# Remove any / or \\ at end of inital_dir
			initial_dir = gsub("([/]|(\\\\))$", "", initial_dir)
		
			if(is.null(sep)){
					# Read majority of data (skip 1st line)
					stats <- read.table(stat_path, header = TRUE,
					                    stringsAsFactors = FALSE,
					                    strip.white = TRUE,
					                    na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$"), comment.char = "", sep=',')
				} else {
					stats <- read.table(stat_path, sep = sep, header = TRUE,
					                    stringsAsFactors = FALSE, strip.white = TRUE,
					                    na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$"), comment.char = "")
				}
		
			# Read file
				if(is.null(sep)){
					# Read majority of data (skip 1st line)
					trend <- read.table(trend_path, header = TRUE,
					                    stringsAsFactors = FALSE, strip.white = TRUE,
					                    na.strings = c("NA", "-1.#IO", "1.#R", "*******", "********", "1.#QO", "-1.$"), comment.char = "", sep = ',')
				} else {
					trend <- read.table(trend_path, sep = sep, header = TRUE,
					                    stringsAsFactors = FALSE, strip.white = TRUE,
					                    na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$", "1.#QO"), comment.char = "")
				}
		
			# Read file
				if(is.null(sep)){
					# Read majority of data (skip 1st line)
					freq <- read.table(freq_path, header = TRUE,
					                   stringsAsFactors = FALSE, strip.white = TRUE,
					                   na.strings = c("NA", "*******", "1.#INF", "1.#QNB", "********"), comment.char = "", sep=',')
				} else {
					freq <- read.table(freq_path, sep = sep, header = TRUE,
					                   stringsAsFactors = FALSE, strip.white = TRUE,
					                   na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$", "1.#INF", "1.#QNB", "********"), comment.char = "")
				}
		
        return(list(stats = stats,
                    trend = trend,
                    freq = freq))
			}
