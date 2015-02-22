loaddata <- function(round,STUDY_INFO=STUDY_INFO) {
  require(stringr)
  datadir = file.path(STUDY_INFO$datadir,sprintf('round%02d',round))
  filelist = list.files(datadir)
  for (f in filelist) {
    print(f)
    fileparts <- strsplit(f,'_')[[1]]
    subjectnumber <- as.numeric(str_extract(fileparts[1],'[0-9]+'))
    filetype <- factor(fileparts[2],levels=c('data.txt','session.txt'),labels=c('data','session'))
    fpath <- file.path(datadir,f)
    if (filetype == "session") {
      SESSION <- scan(fpath,what=character(),sep='\n')
      ix <- match('Binding matrix:',SESSION)
      Binding<-SESSION[(ix+1):(ix+6)]
      BindingListStr <- str_extract_all(Binding,'[0-9]+')
      BindingMatrix <- t(sapply(BindingListStr,as.numeric))[,-1]
      ix <- grep('Code name bindings',SESSION)[1]
    }
  }
}
