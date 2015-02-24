fix_study_data <- function(x) {
  study <- x$task=='Study'
  endtrial <- grepl('^[A-Z]',x$TP1)
  
  # Shift values in "endtrial" rows over by 1 column starting at column 7
  x[study & endtrial,8:ncol(x)] <- x[study & endtrial,7:(ncol(x)-1)]
  x[study & endtrial,7] <- NA
  
  # Relabel whatever is in "item number" as the round (seems to correspond to
  # build attempts)
  x[study,"round"] <- x[study,"itemnumber"]
  
  # Relabel whatever is in "TP1" as the item number. Seems consistent throughout
  # trial, but not monotonically increasing across trials.
  x[study,"itemnumber"] <- x[study,"TP1"]
  
  # Overwrite the now useless TP1 field with NAs
  x[study,"TP1"] <- NA
  
  # Shift all study data after column 7 to the right 4 columns.
  x[study,11:ncol(x)]<- x[study,7:(ncol(x)-4)]
  TP <- paste('TP',1:5,sep='')
  RP <- paste('RP',1:5,sep='')
  OP <- paste('O',1:5,sep='')
  x[study,TP] <- x[study,RP]
  x[study,RP] <- x[study,OP]
  x[study,OP] <- NA
  
  # Move time information to the Time field.
  x[study,'Time'] <- x[study,'O6']
  x[study,'O6'] <- NA
  return(x)
} 