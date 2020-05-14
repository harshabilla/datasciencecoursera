pmean <- function(directory, pollutant, id=1:332){
  pollutants=c()
  names=list.files(directory)
  for(i in id){
    filepath=paste(directory,"/",names[i],sep="")
    data=read.csv(filepath,header=T)
    pollutants=c(pollutants,data[,pollutant])
  }
  pmean=mean(pollutants, na.rm=T)
  pmean
}
complete <- function(directory, id= 1:332){
  ids=c()
  nobss=c()
  filenames=list.files(directory)
for(i in id){
  filepath=paste(directory,"/",filenames[i],sep="")
  data=read.csv(filepath,header=T)
  completeCases=data[complete.cases(data), ]
  ids=c(ids,i)
  nobss=c(nobss,nrow(completeCases))
}
  data.frame(id=ids,nobs=nobss)
}
corr <- function(directory, threshold = 0){
  completes = complete(directory, 1:332)
  completes_above_threshold = subset(completes, nobs > threshold )
  correlations <- vector()
  filenames = list.files(directory)
  for(i in completes_above_threshold$id){
    filepath=paste(directory,"/" ,filenames[i], sep="")
    data = read.csv(filepath, header = TRUE)
    completeCases = data[complete.cases(data),]
    count = nrow(completeCases)
    if( count >= threshold ) {
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  correlations
}