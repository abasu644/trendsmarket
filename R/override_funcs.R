
rmdflag=1
rmdstr=""
finalfile="default.html"
#base_path="C:/Users/abasu/Desktop/rpackages/"
base_path="/home/devtest/"


print.data.frame.orig <- getS3method("print", "data.frame")
print.default.orig <- getS3method("print", "default")
print.ggplot.orig <- getS3method("print", "ggplot")

print.data.frame <- function(x)
{
  if(rmdflag==0)
  {
    print.data.frame.orig(x)
    return;
  }
  else
  {
    print.data.frame.knit(x)
  }
}

print.default <- function(x)
{
  if(rmdflag==0)
  {
    print.default.orig(x)
    return;
  }
  else
  {
    print.default.knit(x)
  }
}

print.ggplot <- function(x)
{
  if(rmdflag==0)
  {
    print.ggplot.orig(x)
    return;
  }
  else
  {
    print.ggplot.knit(x)
  }
}


print.data.frame.knit<- function(x)
{
  library(xtable)
  cnames=colnames(x)
  x=t(x)
  x=apply(x,2,as.character)
  x=as.data.frame(x)
  x=t(x)
  x=as.data.frame(x)
  cnames->colnames(x)
  options(width=120);
  #cat(capture.output(x),sep="\n")
  print.data.frame.orig(x,include.rownames=FALSE,quote=F)
  #cat("\n")
  
}


print.default.knit<- function(x)
{
  cat(x)
  cat("\n")
  # knit2html(paste(system.file(package="trendsmarket"),"rmd/printdefault.Rmd",sep="/"),fragment.only = TRUE)
  #   s1 <- paste(readLines("printdefault.html"), collapse="\n")
  #   #rmdstr<-paste(readLines(finalfile), collapse="\n")
  #   rmdstr<-paste(rmdstr,s1)
  #ff <- file(finalfile,open="at")
  #write(rmdstr,file=ff)
  #close(ff)
}
print.ggplot.knit<- function(x)
{
  print.ggplot.orig(x)
  #knit2html(paste(system.file(package="trendsmarket"),"rmd/ggplot.Rmd",sep="/"),fragment.only = TRUE)
  #   s1 <- paste(readLines("ggplot.html"), collapse="\n")
  #   #rmdstr<-paste(readLines(finalfile), collapse="\n")
  #   rmdstr<-paste(rmdstr,s1)
  #ff <- file(finalfile,open="at")
  #write(rmdstr,file=ff)
  #sclose(ff)
}
#
getmetadata<-function()
{
  meta_file="dataset_metadata.conf"
  flp=paste(base_path,meta_file,sep="")
  read.csv(flp)
  
}