load_session_data <- function(round,STUDY_INFO) {
  require(stringr)
  datadir = file.path(STUDY_INFO$datadir,sprintf('round%02d',round))
  filelist = list.files(datadir)
  MetaData <- list()
  SessionData <- data.frame(subject=numeric(),session=numeric(),task=factor(NULL,levels=1:3,labels=c('study','train','test')),start=as.Date(character()),end=as.Date(character()),accuracy=numeric())
  for (f in filelist) {
    fileparts <- strsplit(f,'_')[[1]]
    subjectnumber <- as.numeric(str_extract(fileparts[1],'[0-9]+'))
    filetype <- factor(fileparts[2],levels=c('data.txt','session.txt'),labels=c('data','session'))
    fpath <- file.path(datadir,f)
    if (filetype == "session") {
      SESSION <- scan(fpath,what=character(),sep='\n')
      
      ## Count sessions completed
      sesix <- grep('Session number:',SESSION)
      nsessions <- length(sesix)
      SESSIONLIST <- list()
      sesix <- append(sesix,length(SESSION)+1)
      for (i in 1:nsessions) {
        SESSIONLIST[[i]] <- SESSION[sesix[i]:sesix[i+1]-1]
      }
      
      ## Extract Bindings
      ix <- match('Binding matrix:',SESSION)
      Binding<-SESSION[(ix+1):(ix+6)]
      BindingListStr <- str_extract_all(Binding,'[0-9]+')
      BindingMatrix <- t(sapply(BindingListStr,as.numeric))[,-1]
      ix <- grep('Code name bindings',SESSION)[1]
      CodeNameBindings <- as.numeric(str_extract_all(SESSION[ix],'[0-9]+')[[1]])
      
      ## SESSION 1
      # Extract Study Round start/stop times
      ix <- grep('Start study round',SESSIONLIST[[1]])[1] # Round 0
      DateTimeString <- str_trim(str_split(SESSIONLIST[[1]][ix], pattern=':', n=2)[[1]][2])
      StudyDateTime <- strptime(DateTimeString,'%m/%d/%Y %I:%M:%S %p')

      # Extract Training (one-missing) start times
      ix <- grep('Start inference with 1 parts missing',SESSIONLIST[[1]])
      temp <- str_split(SESSIONLIST[[1]][ix], pattern=' at ', n=2)
      DateTimeStrings <- sapply(temp, str_trim)[2,]
      TrainingDateTimes_start <- strptime(DateTimeStrings,'%m/%d/%Y %I:%M:%S %p')
      
      # Extract Training (one-missing) stop times
      ix <- grep('Completed  [0-9]+ trials with 1 feature/s missing',SESSIONLIST[[1]])
      temp <- str_split(SESSIONLIST[[1]][ix], pattern=' at ', n=2)
      DateTimeStrings <- sapply(temp, str_trim)[2,]
      DateTimeStrings <- str_replace(DateTimeStrings,'.[0-9]+$','')
      TrainingDateTimes_end <- strptime(DateTimeStrings,'%d-%b-%Y %H:%M:%S')
      nrounds <- length(TrainingDateTimes_end)
      
      # Extract Training (one-missing) proportion correct
      ProportionCorrectTraining <- c(NA,sapply(str_extract_all(SESSIONLIST[[1]][ix+1],'[0-1].[0-9]+'),as.numeric))
      
      X <- data.frame(
        subject=subjectnumber,
        session=1,
        task=factor(rep(1:2,c(1,nrounds)),levels=1:3,labels=c('study','train','test')),
        start=c(StudyDateTime,TrainingDateTimes_start),
        end=c(TrainingDateTimes_start[1],TrainingDateTimes_end),
        accuracy=as.numeric(ProportionCorrectTraining)
      )
      SessionData <- rbind(SessionData,X)
      
      ## SESSIONS 2 & 3 (possibly more or less in case of errors)
      # Extract Testing (two-missing) start times
      for (i in 2:nsessions) {
        ix <- grep('Start inference with 2 parts missing',SESSIONLIST[[i]])
        if (length(ix) == 0) {next}
        temp <- str_split(SESSIONLIST[[i]][ix], pattern=' at ', n=2)
        DateTimeStrings <- sapply(temp, str_trim)[2,]
        TestDateTimes_start <- strptime(DateTimeStrings,'%m/%d/%Y %I:%M:%S %p')
        
        # Extract Training (two-missing) stop times
        ix <- grep('Completed  [0-9]+ trials with 2 feature/s missing',SESSIONLIST[[i]])
        temp <- str_split(SESSIONLIST[[i]][ix], pattern=' at ', n=2)
        DateTimeStrings <- sapply(temp, str_trim)[2,]
        DateTimeStrings <- str_replace(DateTimeStrings,'.[0-9]+$','')
        TestDateTimes_end <- strptime(DateTimeStrings,'%d-%b-%Y %H:%M:%S')
        ntests <- length(ix)
        # Extract Training (two-missing) proportion correct
        ProportionCorrectTest <- sapply(str_extract_all(SESSIONLIST[[i]][ix+1],'[0-1].[0-9]+'),as.numeric)
        
        X <- data.frame(
          subject=subjectnumber,
          session=i,
          task=factor(3,levels=1:3,labels=c('study','train','test')),
          start=TestDateTimes_start,
          end=TestDateTimes_end,
          accuracy=ProportionCorrectTest
        )
        SessionData <- rbind(SessionData,X)
      }
    }
  }
  return(SessionData)
}
