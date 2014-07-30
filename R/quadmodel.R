

knitquadratic <- function(mktid=-9999,rf_code="gfx")
{
  run_file=paste("run_data_",rf_code,".tsv",sep="")
  mktdatafile=paste("data_analysis_mktid_rule_1-5_",rf_code,".tsv",sep="")
  rmddata=paste(system.file(package="trendsmarket"),"rmd/quadratic_model7.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n mktid=",mktid,
                      " \n base_path='", 
                      base_path, "' \n ",
                      " \n run_file='", 
                      run_file, "' \n ",
                      " \n mktdatafile='", 
                      mktdatafile, "' \n ",
                      " ```",
                      "\n",
                      rmddata,
                      sep="");
  write(finalfiledata,file="trent.Rmd")
  knit2html("trent.Rmd",output="trent.html",fragment.only = TRUE)
  
}


getquadraticdata<-function(mktid,rf_code="gfx")
{
  run.date = as.Date("3015-06-02")
  run_file=paste("run_data_",rf_code,".tsv",sep="")
  cnm=colnames(read.csv(paste(base_path,"data.csv",sep=""),sep="\t"))
  a=system(paste("grep ",  mktid, paste(" ",base_path,run_file,sep=""),sep=""),intern=T,ignore.stderr=T)
  alltraffic.df=as.data.frame( t(sapply(1:length(a), function(x) strsplit(a, "\t")[[x]])) ) 
  colnames(alltraffic.df)=cnm
  alltraffic.df$DTE=as.Date(as.character(alltraffic.df$DTE))
  alltraffic.df=alltraffic.df[alltraffic.df$DTE!='2014-05-27',]
  alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))]=apply(alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))],2,as.numeric)
  audits.csv = paste("_", run.date,".csv", sep="")
  mkt.id = mktid
  return(alltraffic.df)
}

callquadratic<-function(mktid,base_path,run_file)
{
  run.date = as.Date("3015-06-02")
  cnm=colnames(read.csv(paste(base_path,"data.csv",sep=""),sep="\t"))
  a=system(paste("grep ",  mktid, paste(" ",base_path,run_file,sep=""),sep=""),intern=T,ignore.stderr=T)
  alltraffic.df=as.data.frame( t(sapply(1:length(a), function(x) strsplit(a, "\t")[[x]])) ) 
  colnames(alltraffic.df)=cnm
  alltraffic.df$DTE=as.Date(as.character(alltraffic.df$DTE))
  alltraffic.df=alltraffic.df[alltraffic.df$DTE!='2014-05-27',]
  alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))]=apply(alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))],2,as.numeric)
  audits.csv = paste("_", run.date,".csv", sep="")
  mkt.id = mktid
  rf_code ="gfx"
  return(quadratic.fn(mkt.id, rf_code, run.date,alltraffic.df))
}
checknetrev <- function(mktid)
{
  rf_code="gfx"
   cnm=colnames(read.csv(paste(base_path,"data.csv",sep=""),sep="\t"))
  a=system(paste("grep ",  mktid, paste(" ",base_path,"run_data.tsv",sep=""),sep=""),intern=T,ignore.stderr=T)
  alltraffic.df=as.data.frame( t(sapply(1:length(a), function(x) strsplit(a, "\t")[[x]])) ) 
  colnames(alltraffic.df)=cnm
  alltraffic.df$DTE=as.Date(as.character(alltraffic.df$DTE))
  alltraffic.df=alltraffic.df[alltraffic.df$DTE!='2014-05-27',]
  alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))]=apply(alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))],2,as.numeric)
  max_dt=max(alltraffic.df$DTE)
  latest_netrev=alltraffic.df[alltraffic.df$DTE==max_dt,"TRREVENUE"] -alltraffic.df[alltraffic.df$DTE==max_dt,"COST"]
  latest_maxcpc=alltraffic.df[alltraffic.df$DTE==max_dt,"MAX_CPC"]
  return(data.frame(latest_date=max_dt,latest_netrev=latest_netrev,latest_maxcpc=latest_maxcpc))
}



plotFandV <- function(traffic.df)
{
  traffic.df=traffic.df[order(traffic.df$DTE),]
  ml=melt(traffic.df, id = "DTE")
  mlf1=ml[ml$variable %in% c("REVENUE","COST","NETREV"),]
  mlf2=ml[ml$variable %in% c("CLICKS","IMPR"),]
  mlf1$set="Finance"
  mlf2$set="Volume"
  mlf=rbind(mlf1,mlf2)
  pl<-ggplot()+geom_line(data=subset(mlf,set=="Volume"),aes(DTE, value,group=variable,colour=variable))+
  geom_line(data=subset(mlf,set=="Finance"),aes(DTE, value,group=variable,colour=variable))+facet_grid(set ~ .     ,scales="free_y")
  print(pl)
}