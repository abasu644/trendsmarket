groupmktidanalysis <- function(mktid=-9999,rf_code="gfx")
{
  rmddata=paste(system.file(package="trendsmarket"),"rmd/analysis.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n mktid=",mktid,
                      " \n base_path='", 
                      base_path, "' \n ",
                      " ```",
                      "\n",
                      rmddata,
                      sep="");
  write(finalfiledata,file="groupmktidanalysis_temp.Rmd")
  knit2html("groupmktidanalysis_temp.Rmd",output="groupmktidanalysis_temp.html",fragment.only = TRUE)
}