groupmktidanalysis <- function(mktid=-9999,rf_code="gfx")
{
  rmddata=paste(system.file(package="trendsmarket"),"rmd/analysis.Rmd",sep="/")
  knit2html(rmddata,output="groupmktidanalysis_temp.html",fragment.only = TRUE)
}