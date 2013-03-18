report <-
function(filename, info) cat(file=filename, append=T, paste(timenow(), info, '\n', sep='\t'))
