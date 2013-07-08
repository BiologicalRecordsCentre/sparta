read_frescalo <-
function(stat_path = NULL, trend_path = NULL, freq_path = NULL, initial_dir = "~/", sep = NULL){
		# Remove any / or \\ at end of inital_dir
			initial_dir = gsub("([/]|(\\\\))$", "", initial_dir)
		# Frescalo Stats file
			# Print header
				cat("\nFRESCALO STATS FILE\n")
			# Flush Console 
				flush.console()
			# Pop-up dialog box to choose file
				if(is.null(stat_path)){
					f_path = choose.files(default = file.path(initial_dir,"*.*"), caption = "Choose Frescalo Stats File:", multi = FALSE)
				} else {
					f_path = stat_path
				}
			# Return file path
				cat("\t",f_path, "\n", sep="")
			# Read file
				if(is.null(sep)){
					# Read majority of data (skip 1st line)
					stats <<- read.fwf(f_path, widths = c(10,7,7,7,7,8,8,10,10,5), header=FALSE, stringsAsFactors = FALSE, strip.white = TRUE, skip = 1, na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$"), comment.char = "")
					# Read in column names from 1st line
					names(stats) <<- read.table(f_path, skip = 0, nrows = 1, stringsAsFactors = FALSE)[1,]
				} else {
					stats <<- read.table(f_path, sep=sep, header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$"), comment.char = "")
				}
			# Return statements that file has been read
				cat("\tFile \"", basename(f_path),"\" read: ", nrow(stats), " rows and ", ncol(stats), " columns loaded into variable \"stats\"\n", sep="")
			# Show preview of data
				cat("\n\tPreview:\n\n")
				print(head(stats, 10))
				# Add extra line break
				cat("\n")
			# Flush Console 
				flush.console()
				
		# Frescalo Trend file
			# Print header
				cat("FRESCALO TREND FILE\n")
				#cat("Choose Frescalo Trend File: \n")
			# Flush Console 
				flush.console()
			# Pop-up dialog box to choose file
				if(is.null(trend_path)){
					f_path = choose.files(default = file.path(dirname(f_path),"*.*"), caption = "Choose Frescalo Trend File:", multi = FALSE)
				} else {
					f_path = trend_path
				}
			# Return file path
				cat("\t",f_path, "\n", sep="")
			# Read file
				if(is.null(sep)){
					# Read majority of data (skip 1st line)
					trend <<- read.fwf(f_path, widths=c(9,12,8,7,7,7,7,7,7), header=FALSE, stringsAsFactors = FALSE, strip.white = TRUE, skip = 1, na.strings = c("NA", "*******", "********", "1.#QO"), comment.char = "")
					# Read in column names from 1st line
					names(trend) <<- read.table(f_path, skip = 0, nrows = 1, stringsAsFactors = FALSE)[1,]
				} else {
					trend <<- read.table(f_path, sep=sep, header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$", "1.#QO"), comment.char = "")
				}
			# Return statements that file has been read
				cat("\tFile \"", basename(f_path),"\" read: ", nrow(trend), " rows and ",ncol(trend)," columns loaded into variable \"trend\"\n", sep="")
			# Show preview of data
				cat("\n\tPreview:\n\n")
				print(head(trend, 10))
				# Add extra line break
				cat("\n")
			# Flush Console 
				flush.console()
				
		# Frescalo Freq file
			# Print header
				cat("FRESCALO FREQUENCY FILE\n")
				#cat("Choose Frescalo Frequency File: \n")
			# Flush Console 
				flush.console()
			# Pop-up dialog box to choose file
				if(is.null(freq_path)){
					f_path = choose.files(default = file.path(dirname(f_path),"*.*"), caption = "Choose Frescalo Frequency File:", multi = FALSE)
				} else {
					f_path = freq_path
				}
			# Return file path
				cat("\t",f_path, "\n", sep="")
			# Read file
				if(is.null(sep)){
					# Read majority of data (skip 1st line)
					freq <<- read.fwf(f_path, widths=c(10,10,6,8,8,8,6,8), header=FALSE, stringsAsFactors = FALSE, strip.white = TRUE, skip = 1, na.strings = c("NA", "*******", "1.#INF", "1.#QNB", "********"), comment.char = "")
					# Read in column names from 1st line
					names(freq) <<- read.table(f_path, skip = 0, nrows = 1, stringsAsFactors = FALSE)[1,]
				} else {
					freq <<- read.table(f_path, sep=sep, header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", "*******", "-1.#IO","1.#R", "-1.$", "1.#INF", "1.#QNB", "********"), comment.char = "")
				}
			# Return statements that file has been read
				cat("\tFile \"", basename(f_path),"\" read: ", nrow(freq), " rows and ",ncol(freq)," columns loaded into variable \"freq\"\n", sep="")
			# Show preview of data
				cat("\n\tPreview:\n\n")
				print(head(freq, 10))
				# Add extra line break
				cat("\n")
			# Flush Console 
				flush.console()
		
		invisible(NULL)
	}
