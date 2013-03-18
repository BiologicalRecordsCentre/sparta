is.gridcell.wellsampled <-
function(CellID, n=3){
  # modified version of filter.gridcells()
  #takes the dataframe and returns the rownumbers of the dataset identifying well-sampled the gridcells
  nyrs <- table(CellID)
  return(CellID %in% names(nyrs)[nyrs >= n])
}
