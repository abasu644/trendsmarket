
viewabtest <- function(rf_code="gfx")
{
  ab_file=paste(base_path,"test_control_results_",rf_code,".tsv",sep="")
  rmddata=paste(system.file(package="trendsmarket"),"rmd/abtest.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n ab_file='",
                      ab_file,
                      "'\n",
                       "```",
                      " \n",
                      rmddata,
                      sep="")
                    
  write(finalfiledata,file="viewabtest_temp.Rmd")
  knit2html("viewabtest_temp.Rmd",output="viewabtest_temp.html",fragment.only = TRUE)
  
}

#dt=read.csv("C:/Users/abasu/Desktop/rpackages/us_flag11_training_data_weights.txt",sep="\t")

getcidddata <- function()
{
dt=read.csv("C:/Users/abasu/Desktop/rpackages/cidffdata.tsv",sep="\t")
colnames(dt)=c("dt","cidAvail","total_count","netrev","clicks")
ml=melt(dt, id = c("dt","cidAvail"))
ml$value=round(as.numeric(ml$value),2)        
mln=ml[ml$variable=='total_count',]
pl<-ggplot()+geom_line(data=mln,aes(dt, value,group=cidAvail,colour=cidAvail))
#print(pl)
}