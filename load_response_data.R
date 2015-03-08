load_response_data <- function(round, STUDY_INFO) {
  require(stringr)
  datadir = file.path(STUDY_INFO$datadir,sprintf('round%02d',round))
  filelist = list.files(datadir)
  ResponseData <- data.frame(
    subject=numeric(),
    session=numeric(),
    task=factor(NULL,levels=c('Study','Missing1','Missing2'),labels=c('study','train','test')),
    round=numeric(),
    trial=numeric(),
    itemnumber=numeric(),
    TP1=numeric(),
    TP2=numeric(),
    TP3=numeric(),
    TP4=numeric(),
    TP5=numeric(),
    TCl=numeric(),
    TCd=numeric(),
    RP1=numeric(),
    RP2=numeric(),
    RP3=numeric(),
    RP4=numeric(),
    RP5=numeric(),
    RCl=numeric(),
    RCd=numeric(),
    O1=numeric(),
    O2=numeric(),
    O3=numeric(),
    O4=numeric(),
    O5=numeric(),
    O6=numeric(),
    O7=numeric(),
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
      DATA <- read.table(fpath,header=TRUE,fill=NA,stringsAsFactors=FALSE,strip.white=TRUE)[-1,]
      names(DATA) <- names(ResponseData)
      DATA$subject <- as.numeric(subjectnumber)
      DATA <- fix_study_data(DATA)
      DATA <- fix_field_types(DATA)
      ResponseData <- rbind(ResponseData,DATA)
    }
  }
  return(ResponseData)
}