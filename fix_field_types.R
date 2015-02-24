fix_field_types <- function(x){
  study <- x$task == 'Study'
  toInteger <- c('subject','session','round','trial','itemnumber',paste('TP',1:5,sep=''),paste('RP',1:5,sep=''),'Acc')
  x[,toInteger] <- as.data.frame(lapply(x[,toInteger],as.integer))
  
  toNumeric <- c('Time','CumPCorr')
  x[,toNumeric] <- as.data.frame(lapply(x[,toNumeric],as.numeric))
  
  toLogical <- c(paste('O',1:7,sep=''))
  x[,toLogical] <- as.data.frame(lapply(x[,toLogical],as.logical))
  
  toFactor <- c("TCl","TCd","RCl","RCd")
  x[,toFactor] <- as.data.frame(lapply(x[,toFactor],as.factor))
  
  x$task=factor(x$task,levels=c('Study','Missing1','Missing2'),labels=c('study','train','test'))
  return(x)
}