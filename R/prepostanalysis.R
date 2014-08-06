prepostanalysis <- function(mktid=-9999,rf_code="gfx")
{
  rmddata=paste(system.file(package="trendsmarket"),"rmd/prepostanalysis.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n mktid=",mktid,
                      " \n base_path='", 
                      base_path, "' \n ",
                      " ```",
                      "\n",
                      rmddata,
                      sep="");
  write(finalfiledata,file="prepostanalysis_temp.Rmd")
  knit2html("prepostanalysis_temp.Rmd",output="prepostanalysis_temp.html",fragment.only = TRUE)
}