format_recs <-
function(records,taxon){
  colnames(records)[grep('YEAR',colnames(records))]<-'year'
  colnames(records)[grep('TO_ENDDATE',colnames(records))]<-'Date'
  records$site <- reformat_gr(records$TO_GRIDREF, prec_out = 10000)
  records$kmsq <- reformat_gr(records$TO_GRIDREF, prec_out = 1000)
  records$taxon <- taxon
  return(records)
}
