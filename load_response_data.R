load_response_data <- function(round, STUDY_INFO) {
  require(stringr)
  datadir = file.path(STUDY_INFO$datadir,sprintf('round%02d',round))
  filelist = list.files(datadir)
  ResponseData <- data.frame(
    subject=numeric(),
    session=numeric(),
    task=factor(NULL,levels=1:3,labels=c('study','train','test')),
    round=numeric(),
    trial=numeric(),
    itemnumber=numeric(),
    TP1=logical(),
    TP2=logical(),
    TP3=logical(),
    TP4=logical(),
    TP5=logical(),
    TCl=logical(),
    TCd=logical(),
    RP1=logical(),
    RP2=logical(),
    RP3=logical(),
    RP4=logical(),
    RP5=logical(),
    RCl=logical(),
    RCd=logical(),
    O1=logical(),
    O2=logical(),
    O3=logical(),
    O4=logical(),
    O5=logical(),
    O6=logical(),
    O7=logical(),
    Time=numeric(),
    Acc=numeric(),
    CumPCorr=numeric()
  )
  for (f in filelist) {
    fileparts <- strsplit(f,'_')[[1]]
    subjectnumber <- as.numeric(str_extract(fileparts[1],'[0-9]+'))
    filetype <- factor(fileparts[2],levels=c('data.txt','session.txt'),labels=c('data','session'))
    fpath <- file.path(datadir,f)
    if (filetype == "data") {
      print(f)
      DATA <- read.table(fpath,header=FALSE,skip=2,sep=" ")
      names(DATA) <- names(ResponseData)
      ResponseData <- rbind(ResponseData,DATA)
    }
  }
  return(ResponseData)
}