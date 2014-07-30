
dataanalysis <- function(mktid=-9999,rf_code="gfx")
{
   mktdatafile=paste("data_analysis_mktid_rule_1-5_",rf_code,".tsv",sep="")
   
   rmddata=paste(system.file(package="trendsmarket"),"rmd/dataanalysis.Rmd",sep="/")
   rmddata=paste (readLines(rmddata),collapse="\n")
   finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n mktid=",
                       mktid,
                       " \n base_path='", 
                       base_path, 
                       "' \n rf_code='",
                       rf_code,
                       "' \n mktdatafile='", 
                       mktdatafile, 
                       "' \n ```", 
                       "\n",
                       rmddata,sep="");
   write(finalfiledata,file="dataanalysis_temp.Rmd")
   knit2html("dataanalysis_temp.Rmd",output="dataanalysis_temp.html",fragment.only = TRUE)
}



