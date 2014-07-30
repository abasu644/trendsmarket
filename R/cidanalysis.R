
viewcidresults <- function(cid=NULL,rf_code="gfx")
{
  
  cid_file=paste(base_path,"cid_",rf_code,".tsv",sep="")
  rmddata=paste(system.file(package="trendsmarket"),"rmd/cidanalysis.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n cid_file='",
                      cid_file,
                      "'\n",
                      " cid='",
                      cid,
                      "' \n ```",
                      " \n",
                      rmddata,
                      sep="")
  
  write(finalfiledata,file="viewcidresults_temp.Rmd")
  knit2html("viewcidresults_temp.Rmd",output="viewcidresults_temp.html",fragment.only = TRUE)
  
}

getALLCIDS <- function(rf_code="gfx")
{
  
  cid_file=paste(base_path,"cid_",rf_code,".tsv",sep="")
  dt=read.csv(cid_file,sep="\t")
  colnames(dt)=c("cid","dte","count","netrev","clicks")
  allcids=unique(dt$cid)
  return(allcids)
}