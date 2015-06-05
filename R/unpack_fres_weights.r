unpack_fres_weights <- function(Fres_weights, frespath){
  
  fixedtoDF <- function(DF){
    
    DF <- as.matrix(DF)
    
    byrow <- function(x){
      
      split <- unlist(strsplit(as.character(x), split = ' '))
      return(split[split != ''])
      
    }
    
    df_format <- t(apply(DF, MARGIN = 1, FUN = byrow))
    
    return(df_format)
    
  }
  
  # unpack weights file if needed
  if(class(Fres_weights)=='character'){
    if(Fres_weights=='LCUK'){
      Fres_weights_name<-'UK_LC_Wts.txt'
      if(!file.exists(paste(dirname(frespath),'/UK_LC_Wts.txt',sep=''))){
        data(UK_LC_Wts)
        write.table(UK_LC_Wts,file=paste(dirname(frespath),'/UK_LC_Wts.txt',sep=''),row.names=FALSE,col.names=FALSE,quote=FALSE) 
        Fres_weights_out <- fixedtoDF(UK_LC_Wts)
      } else {
        data(UK_LC_Wts)
        Fres_weights_out <- fixedtoDF(UK_LC_Wts)
      }
    }      
    if(Fres_weights=='LCGB'){
      Fres_weights_name<-'GB_LC_Wts.txt'
      if(!file.exists(paste(dirname(frespath),'/GB_LC_Wts.txt',sep=''))){
        data(GB_LC_Wts)
        write.table(GB_LC_Wts, file = paste(dirname(frespath), '/GB_LC_Wts.txt', sep = ''), row.names=FALSE,col.names=FALSE,quote=FALSE) 
        Fres_weights_out <- fixedtoDF(GB_LC_Wts)
      } else {
        data(GB_LC_Wts)
        Fres_weights_out <- fixedtoDF(GB_LC_Wts)
      } 
    }
    if(Fres_weights=='LCNI'){
      Fres_weights_name<-'NI_LC_Wts.txt'
      if(!file.exists(paste(dirname(frespath),'/NI_LC_Wts.txt',sep=''))){
        data(NI_LC_Wts)
        write.table(NI_LC_Wts,file=paste(dirname(frespath),'/NI_LC_Wts.txt',sep=''),row.names=FALSE,col.names=FALSE,quote=FALSE) 
        Fres_weights_out <- fixedtoDF(NI_LC_Wts)
      } else {
        data(NI_LC_Wts)
        Fres_weights_out <- fixedtoDF(NI_LC_Wts)
      }
    }
    if(Fres_weights == 'VP'){
      Fres_weights_name <- 'Wts.txt' 
      if(!file.exists(paste(dirname(frespath), '/Wts.txt', sep = ''))){
        data(Wts)
        write.table(Wts, file = paste(dirname(frespath),'/Wts.txt', sep = ''), row.names = FALSE,col.names=FALSE,quote=FALSE)             
        Fres_weights_out <- fixedtoDF(Wts)
      } else {
        data(Wts)
        Fres_weights_out <- fixedtoDF(Wts)
      }
    }
  }
  
  if(is.data.frame(Fres_weights)){
    if(length(Fres_weights) != 3) stop('Fres_weights data.frame must have three columns: target, neighbour, weight')
    if(!is.numeric(Fres_weights[,3])){
      warning('Weights column in Fres_weights should be numeric, conversion attempted')
      if(is.factor(Fres_weights[,3])){
        Fres_weights[,3] <- as.numeric(as.character(Fres_weights[,3]))
      } else if(is.character(Fres_weights[,3])){
        Fres_weights[,3] <- as.numeric(Fres_weights[,3])
      } else{
        stop('Conversion failed')
      }
    } 
    if(max(Fres_weights[,3]) > 1 | min(Fres_weights[,3]) < 0) stop('Weight in Fres_weights cannot be greater than 1 or less than 0')
    
    # Weights file rounded to 4 decimal places as frescalo takes a fixed width format
    Fres_weights[,3] <- round(Fres_weights[,3],4)
    
    # Write the weights file
    write.fwf(Fres_weights, colnames = FALSE, rownames = FALSE, width = c(9,9,7),
              file = paste(dirname(frespath), '/Custom_Wts.txt', sep = ''))
    Fres_weights_name <- 'Custom_Wts.txt'   
    Fres_weights_out <- Fres_weights
  }
  
  return(list(Fres_weights_name = Fres_weights_name,
              Fres_weights_out = Fres_weights_out,
              site_names = unique(Fres_weights_out[,1])))
  
}