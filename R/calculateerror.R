#' Read CSV file
#' 
#' Simple wrapper for read.csv
#' 
#' @export
#' @param file a csv file.
#' @param ... arguments passed to read.csv
calculateerror <- function(filenm, headero=0,sepo=",",description="",rf_code="gfx")
{
  rmddata=paste(system.file(package="trendsmarket"),"rmd/calculateerror.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n filenm='",
                      filenm,
                      "'\n",
                      "df=read.csv(filenm,header=",headero,",sep='",sepo,"');",
                      " \n",
                       "```",
                      " \n",
                      rmddata,
                      sep="")
                    
  write(finalfiledata,file="calculateerror_temp.Rmd")
  knit2html("calculateerror_temp.Rmd",output="calculateerror_temp.html",fragment.only = TRUE)
  erdf=read.csv(filenm,header=ifelse(headero==0,F,T),sep=sepo)
  saveToErrorDatabase(erdf,description,rf_code,headero)
  
}

saveToErrorDatabase<- function(di,description="",rf_code="gfx",headero=1)
{
  library(RMySQL)
  con <- dbConnect(MySQL(), user="ds", host="10.101.64.136",password="ds",dbname ="lemonade")
  query <- function(...) dbGetQuery(con, ...)
  clength=dim(di)[2]
  y_string=colnames(di)[1]
  if(headero==0)
  {
    colnames(di)[1]="actual"
    y_string="actual"
  }
  maxId <- dbGetQuery(con, "SELECT MAX(eid) FROM lemonade.error_metrics")[1,1] 
  maxId=ifelse(is.na(maxId),0,maxId)
  eid=maxId+1
  tryCatch({
    
  em=data.frame(eid=eid,rf_code=rf_code,description=description,create_date=Sys.Date())
  
  dbWriteTable(con,"error_metrics",em,overwrite=F,append=T,row.names=F)
  
  colmn=colnames(di)
  lc=length(colmn)
  emc=data.frame(eid=rep(eid,lc),colmn=colmn)
  dbWriteTable(con,"error_metrics_colmn",emc,overwrite=F,append=T,row.names=F)
  
  di=cbind(eid=eid,di)
  colnames(di)=c("eid","colmn1","colmn2","colmn3")
  dbWriteTable(con,"error_metrics_data",di,overwrite=F,append=T,row.names=F)
  
 },error=function(x){print(x)},
  finally ={
             dbDisconnect(con)
          })
  
}

listerrors<- function(searchtext="gfx")
{
  rmddata=paste(system.file(package="trendsmarket"),"rmd/listerrors.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=FALSE,echo=F,message=FALSE,comment=NA} \n ",
                      " searchtext='",searchtext,
                      "' \n",
                      "```",
                      " \n",
                      rmddata,
                      sep="")  
  write(finalfiledata,file="listerrors_temp.Rmd")
  knit2html("listerrors_temp.Rmd",output="listerrors_temp.html",fragment.only = TRUE)
  
}

retrieveerror<- function(eid=1)
{
  rmddata=paste(system.file(package="trendsmarket"),"rmd/retrieveerror.Rmd",sep="/")
  rmddata=paste (readLines(rmddata),collapse="\n")
  finalfiledata=paste("```{r  warning=FALSE,error=TRUE,echo=F,message=FALSE,comment=NA} \n ",
                      " eid=",eid,
                      " \n",
                      "```",
                      " \n",
                      rmddata,
                      sep="")
  
  write(finalfiledata,file="retrieveerror_temp.Rmd")
  knit2html("retrieveerror_temp.Rmd",output="retrieveerror_temp.html",fragment.only = TRUE)
  
}

errorfun <-function (actual, predicted, q = seq(0, 1, 0.1))
{
  error <- predicted - actual
  rmse <- round(sqrt(crossprod(error)/length(error)),2)
  rmset <- round(mean(error^2, trim = 0.01),2)
  mape <- abs(error)/actual
  bias <- error/actual
  sumup <- data.frame(med = median(error), rmse = rmse, range = max(error) -
               min(error), rmset = rmset)
  
  return(sumup);
}

