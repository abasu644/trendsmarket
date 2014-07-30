
adj.df = data.frame(FACTOR=c(2.017963, 1.690003, 1.756423, 1.949394, 1.761040, 2.083924, 2.352540), 
                    row.names=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


perturb.fn = function(maxcpc) {
  pert.vec = seq(-0.01, 0.01, by=0.01)
  return(maxcpc + sample(pert.vec, 1))
}

cap.fn = function(maxcpc, maxcpc.cap,maxcpc.delta = 0.08) {  
  if (abs(maxcpc - maxcpc.cap) > maxcpc.delta) {
    if (maxcpc > maxcpc.cap) return(maxcpc.cap + maxcpc.delta) else return(maxcpc.cap - maxcpc.delta)
  } else {
    return(maxcpc)
  }
}

quadratic.fn = function(mkt.id, rf_code, run.date) {
  
  traffic.df = alltraffic.df[alltraffic.df$MKT_ID == mkt.id & alltraffic.df$RF_CODE == rf_code,]
  traffic.df$IMPR[(traffic.df$IMPR<1) | is.na(traffic.df$IMPR)] <- 1
  traffic.df = traffic.df[!duplicated(traffic.df),]
  print(traffic.df)
  traffic.df = traffic.df[is.finite(traffic.df$MAX_CPC) & traffic.df$MAX_CPC > 0 &
    !is.na(traffic.df$DTE) & 
    !is.na(traffic.df$REVENUE) & 
    !is.na(traffic.df$COST) &
    !is.na(traffic.df$CLICKS),]
  
  cid = traffic.df$CID[1]
  #rf_code = traffic.df$RF_CODE[1]
  if (nrow(traffic.df) > 0) { #10: Only non-penny bids
    maxcpc.max = max(traffic.df$MAX_CPC)
    last.date = max(traffic.df[!is.na(traffic.df$MAX_CPC), "DTE"])
    maxcpc.cap = max(traffic.df[traffic.df$DTE == last.date, "MAX_CPC"])
    
	traffic.df$TRREVENUE <- apply(traffic.df[,c("REVENUE","TRREVENUE")],1,max)	
	traffic.df$TRNETREV = traffic.df$TRREVENUE - traffic.df$COST
	traffic.df$NETREV = traffic.df$REVENUE - traffic.df$COST
	traffic.df$WDAY = weekdays(as.Date(traffic.df$DTE))
	traffic.df$MAX_CPC1 <- ifelse(traffic.df$CLICKS==0,traffic.df$MAX_CPC,round(traffic.df$COST/traffic.df$CLICKS/0.9,2)) 
	
    if (sum(traffic.df$CLICKS) > 0) {
      #traffic.df = traffic.df[, c("MKT_ID", "DTE", "MAX_CPC", "NETREV", "CLICKS", "WDAY", "RF_CODE", "REVENUE", "COST")]
      maxcpc.impr = max(traffic.df$MAX_CPC[(traffic.df$MAX_CPC < min(traffic.df$MAX_CPC[traffic.df$CLICKS>0]))]) # Maximum bid where we just got impressions and no clicks
      maxcpc.impr = ifelse(maxcpc.impr>0,maxcpc.impr,0)
      #print("printing maxcpc.impr")
      #print(maxcpc.impr)
      
	  tr.df.clk = traffic.df
      traffic.df = traffic.df[traffic.df$CLICKS > 0,]
      print(traffic.df)
      maxcpc.clkltst = max(traffic.df$MAX_CPC[traffic.df$DTE == max(traffic.df$DTE)])
      print("max.cpc.clicklatest : ")
      print(maxcpc.clkltst)
      
      ########## Week day adjustment ######################################
      run.wday = weekdays(run.date)
      if (FALSE) {
        for (i in 1:nrow(traffic.df)) {
          if (run.wday != traffic.df$WDAY[i]) {
            adj = adj.df[run.wday, "FACTOR"]/adj.df[traffic.df$WDAY[i], "FACTOR"]
            if (traffic.df$NETREV[i] >= 0) {
              traffic.df$NETREV[i] = traffic.df$NETREV[i] * adj
            } else {
              traffic.df$NETREV[i] = traffic.df$NETREV[i]/adj
            }
          }
        }
      }
      
      ############## Median smoothing #####################################
	  netRevCalc <- function(traffic.df){
	  netrev.df = data.frame(do.call("rbind", as.list(by(traffic.df, traffic.df$MAX_CPC, FUN=function(x) {
        #print(x)
        y = x[x$CLICKS>0,]
        return(c(MAX_CPC=x$MAX_CPC[1], NETREV_MED=ifelse(nrow(y) == 0, 0, median(rep(y$NETREV, y$CLICKS))), CLICKS_MED=median(x$CLICKS), COUNT=nrow(x)))
      }))))
      
      for (i in 1:nrow(netrev.df)) {
        traffic.df[traffic.df$MAX_CPC == netrev.df$MAX_CPC[i], "NETREV"] = netrev.df$NETREV_MED[i]
        traffic.df[traffic.df$MAX_CPC == netrev.df$MAX_CPC[i], "CLICKS"] = netrev.df$CLICKS_MED[i]
      }
	  return(traffic.df)
	  }
	  trbck <- netRevCalc(traffic.df)
	  
	  ## If its going to rule 5 then claculate netrevenue using the Actual TRREVENUE
	  if (nrow(trbck[trbck$NETREV > 0,]) > 0) {
		USE_TRREV = 0
		traffic.df <- trbck
		
		}else{
		traffic.df$NETREV <- traffic.df$TRNETREV
		traffic.df <- netRevCalc(traffic.df)
		USE_TRREV = 1
		}
		rm(trbck)
      print(traffic.df)
      
      ############ Determine A and B here ##################################
      
      #print("Fixing maxcpc.A & maxcpc.B...")
      if (nrow(traffic.df[traffic.df$NETREV > 0,]) > 0) { #6: Atleast some rows where netrev > 0
        nzmaxcpc.B =  max(traffic.df[traffic.df$NETREV > 0, "MAX_CPC"])
        
        if (nrow(traffic.df[traffic.df$MAX_CPC > nzmaxcpc.B & traffic.df$NETREV == 0,]) > 0) {
          zmaxcpc.B = min(traffic.df[traffic.df$MAX_CPC > nzmaxcpc.B & traffic.df$NETREV == 0, "MAX_CPC"])
        } else {
          zmaxcpc.B = NA
        } 
        maxcpc.B = max(nzmaxcpc.B, zmaxcpc.B, na.rm = T)
        print(paste("nzmaxcpc.B=", nzmaxcpc.B, ", zmaxcpc.B=", zmaxcpc.B, ", maxcpc.B=", maxcpc.B))
        
        if (nrow(traffic.df[traffic.df$MAX_CPC < nzmaxcpc.B & traffic.df$NETREV > 0,]) > 0) {
          nzmaxcpc.A = min(traffic.df[traffic.df$MAX_CPC < nzmaxcpc.B & traffic.df$NETREV > 0, "MAX_CPC"])
        } else {
          nzmaxcpc.A = nzmaxcpc.B
        }
        
        if (nrow(traffic.df[traffic.df$MAX_CPC < nzmaxcpc.A & traffic.df$NETREV == 0,]) > 0) {
          zmaxcpc.A = max(traffic.df[traffic.df$MAX_CPC < nzmaxcpc.A & traffic.df$NETREV == 0, "MAX_CPC"])
        } else {
          zmaxcpc.A = NA
        }
        maxcpc.A = min(nzmaxcpc.A, zmaxcpc.A, na.rm=T)
        print(paste("nzmaxcpc.A=", nzmaxcpc.A, ", zmaxcpc.A=", zmaxcpc.A, ", maxcpc.A=", maxcpc.A))
        
        ################### Subset the traffic for CLICKS > 0 #######################
        print("Using traffic subset for model...")

		tr.df.clk$NETREV <- tr.df.clk[,if(USE_TRREV==1) "TRNETREV" else "NETREV"]
		tr <- data.frame(do.call("rbind", as.list(by(tr.df.clk, tr.df.clk$MAX_CPC1, FUN=function(x) {
			return(c(MAX_CPC1=x$MAX_CPC1[1],IMPR=mean(x$IMPR,na.rm=T), CLICKS = median(x$CLICKS[x$CLICKS>0],na.rm=T),NETREV=median(rep(x$NETREV[x$CLICKS>0],x$CLICKS[x$CLICKS>0]),na.rm=T)))
		}))))
		if(nrow(tr[!is.na(tr$NETREV),])>1){
		tr$NETREV <- approx(tr$MAX_CPC1,tr$NETREV,tr$MAX_CPC1,rule=2)$y}
		else{tr$NETREV[is.na(tr$NETREV)]<-0}		
		tr$CLICKS[is.na(tr$CLICKS)] <- 1
        k  = sum(tr.df.clk$NETREV)/sum(tr.df.clk$IMPR)/5
		tr$NEWMETRIC <- tr$NETREV + k*tr$IMPR
		traffic.df <- merge(tr,tr.df.clk,by="MAX_CPC1")
		colnames(traffic.df) <- c("MAX_CPC", "IMPR", "CLICKS", "NETREV_new", "NETREV", "DTE", "MKT_ID", "RF_CODE", "CID", "MAX_CPC1", "CLICKS_old", "COST", "REVENUE", "IMPR_old", "TRREVENUE","TRNETREV","NETREV_old", "WDAY")		
		print(traffic.df)
        
        if (length(unique(traffic.df$MAX_CPC)) >=2) {
          print(unique(traffic.df$MAX_CPC))
          maxcpc.mean = mean(unique(traffic.df$MAX_CPC))
          print(paste("maxcpc.mean =", maxcpc.mean)) 
          
          modelFn <- function(traffic.df)
          {
          weights.df = traffic.df$CLICKS
          netrev.mdl = lm(NETREV ~ I(MAX_CPC - maxcpc.mean) + I((MAX_CPC - maxcpc.mean)^2), data = traffic.df)#, weights = weights.df)
          netrev.summ = summary(netrev.mdl)
          print(netrev.summ)
          coeff = coef(netrev.summ)
          
          if (nrow(coeff) >= 1) { 
            maxcpc.int = coeff[1,1]; pval.int = coeff[1,4]
          } else {
            maxcpc.int = NA; pval.int = NA
          }
          if (nrow(coeff) >= 2) {
            maxcpc.coeff = coeff[2,1]; pval.coeff = coeff[2,4] 
          } else {
            maxcpc.coeff = NA; pval.coeff = NA
          }
          if (nrow(coeff) >= 3) {
            maxcpc.sq.coeff = coeff[3,1]; pval.sq.coeff = coeff[3,4] 
          } else {
            maxcpc.sq.coeff = NA; pval.sq.coeff = NA
          }
          rsq = netrev.summ$r.sq
          arsq = netrev.summ$adj.r.sq
          return(list(netrev.mdl = netrev.mdl,netrev.summ = netrev.summ,coeff = coeff,maxcpc.int = maxcpc.int,pval.int=pval.int, maxcpc.coeff = maxcpc.coeff,pval.coeff = pval.coeff,maxcpc.sq.coeff = maxcpc.sq.coeff,pval.sq.coeff = pval.sq.coeff,rsq = rsq,arsq = arsq))
          }
          
          modelData <- modelFn(traffic.df)
          if("modelData" %in% search()){detach(modelData)}
          attach(modelData)
          
          ##### if it not rule 1 then add 0 ancor point and model again.
          if (!(!is.na(maxcpc.sq.coeff) & (maxcpc.sq.coeff < 0)))
          {          
            tmpRow <- traffic.df[nrow(traffic.df),]   # add an 0,0 row
            tmpRow$MAX_CPC = maxcpc.impr
            tmpRow$NETREV = 0.0
            print('printing temprow ... ')
            print(tmpRow)
            trafficDf <- rbind(traffic.df,tmpRow)[,c("CLICKS","MAX_CPC","NETREV")]          
            detach(modelData)
            modelData <- modelFn(trafficDf)
            attach(modelData)
            #### if its still rule 2 then try modelling the latest 3 dates data.
            if (!is.na(maxcpc.sq.coeff) & (maxcpc.sq.coeff > 0) & (length(unique(traffic.df$DTE))>3))
            {
              trafficDf1 <- head(traffic.df[order(traffic.df$DTE,decreasing = TRUE),],3)
              detach(modelData)
              trafficDf <- rbind(trafficDf1,tmpRow)[,c("CLICKS","MAX_CPC","NETREV")]
              modelData <- modelFn(trafficDf)
              attach(modelData)
            }
          }
          
          print(paste("maxcpc.sq.coeff=",maxcpc.sq.coeff,",maxcpc.coeff=",maxcpc.coeff,",maxcpc.int=",maxcpc.int))
          if (!is.na(maxcpc.sq.coeff) & (maxcpc.sq.coeff != 0)) { #6: Quadratic term is non-missing
            if (maxcpc.sq.coeff < 0) { #5: Quadratic term is negative
              maxcpc.opt = maxcpc.mean  - maxcpc.coeff/(2 * maxcpc.sq.coeff)
              if (maxcpc.opt <= 0) {
                print(paste("Warning: maxcpc.opt <= 0 at maxcpc.opt =", maxcpc.opt))
                maxcpc.opt = maxcpc.A
              } 
              netrev.opt = predict(netrev.mdl, data.frame(MAX_CPC=maxcpc.opt), type="response")
              if (netrev.opt <= 0) { 
                print(paste("Warning: netrev.opt <= 0 at maxcpc.opt =", maxcpc.opt))
                maxcpc.opt = maxcpc.A
              }
              rule.id = 1
            } else { #4. For the case where quadratic term coefficient is positive
              maxcpc.opt = nzmaxcpc.B; netrev.opt = NA
			  netrev.opt = predict(netrev.mdl, data.frame(MAX_CPC=maxcpc.opt), type="response")
              if (netrev.opt <= 0) { 
                print(paste("Warning: netrev.opt <= 0 at maxcpc.opt =", maxcpc.opt))
                maxcpc.opt = maxcpc.A
              }
              rule.id = 2
            }
          } else { #5. For the case where quadratic term coefficient is missing
            if (!is.na(maxcpc.coeff) & (maxcpc.coeff != 0)) {
              if (maxcpc.coeff > 0) {
                maxcpc.opt = nzmaxcpc.B + 0.01
              } else {
                maxcpc.opt = nzmaxcpc.A - 0.01
              }
            } else {
              maxcpc.opt = nzmaxcpc.A - 0.01
            }
            netrev.opt = predict(netrev.mdl, data.frame(MAX_CPC=maxcpc.opt), type="response")
	    if (netrev.opt <= 0) { 
	      print(paste("Warning: netrev.opt <= 0 at maxcpc.opt =", maxcpc.opt))
	      maxcpc.opt = maxcpc.A
	    }
            rule.id = 3
          }
        } else {
          if (traffic.df$NETREV[1] >= 0) {
            maxcpc.opt = traffic.df$MAX_CPC[1] + 0.01
            rule.id = 3
          } else {
            maxcpc.opt = traffic.df$MAX_CPC[1] - 0.01
            rule.id = 3
          }
          netrev.opt = NA; maxcpc.int = NA; maxcpc.coeff = NA; maxcpc.sq.coeff = NA; pval.int = NA; pval.coeff = NA; pval.sq.coeff = NA; rsq=NA; arsq = NA
        }	  
      maxcpc.opt <- max(traffic.df[traffic.df$MAX_CPC==round(maxcpc.opt,2),"MAX_CPC1"],maxcpc.opt)  
      } else if (nrow(traffic.df[traffic.df$NETREV != 0.0,]) == 0) { # 6. For the case where the NETREV has always been zero for all bids
        maxcpc.A = maxcpc.B = maxcpc.opt = max(traffic.df$MAX_CPC) + 0.01
        rule.id = 4
      } else { #7 For the case where the netrev has always been negative or zero for all bids
        maxcpc.B = min(traffic.df[traffic.df$NETREV < 0.0, "MAX_CPC"])
        if (nrow(traffic.df[traffic.df$NETREV == 0.0,]) > 0) {
          maxcpc.A = max(traffic.df[traffic.df$NETREV == 0.0, "MAX_CPC"])
          if (maxcpc.A < maxcpc.B) {
            maxcpc.opt = min(traffic.df[traffic.df$NETREV == 0.0, "MAX_CPC"]) - 0.01
          } else {
            maxcpc.opt = maxcpc.A + 0.01
          }
        } else {
          maxcpc.opt = maxcpc.A = maxcpc.B - 0.01
        }
        revsum = sum(traffic.df$REVENUE); clicksum = sum(traffic.df$CLICKS)
        maxcpc.opt = maxcpc.cap # over riding all the changes above. last bid retained.
        maxcpc.opt.orig =  maxcpc.cap
        maxcpc.cap = round(ifelse(clicksum >=5, ifelse(revsum == 0, 0.01, revsum/clicksum), NA), 2)
        maxcpc.cap = ifelse(!is.na(maxcpc.cap) & maxcpc.cap == 0, 0.01, maxcpc.cap)
        if (!is.na(maxcpc.cap) & (maxcpc.opt > maxcpc.cap)) maxcpc.opt = maxcpc.cap
        rule.id = 5
      } 
    } else { #8. Where the clicks == 0 at max bid
      maxcpc.A = maxcpc.B = maxcpc.max
      maxcpc.opt = maxcpc.max + 0.01
      rule.id = 6
    }
    print(paste("Applied rule", rule.id, "to mkt_id", mkt.id))
    
    ############# Perturbation ##############################################
    print(paste("Before perturbation MAXCPC_OPT=", maxcpc.opt))
    maxcpc.opt = ifelse((rule.id == 5), maxcpc.opt, perturb.fn(maxcpc.opt))
    print(paste("After perturbation MAXCPC_OPT=", maxcpc.opt))
    
    ############ Capping #####################################################
    if (rule.id != 5) {
      maxcpc.opt.orig = maxcpc.opt
    }
    
    print(paste("Before capping MAXCPC_OPT=", maxcpc.opt))
    if (rule.id < 5) {
      maxcpc.opt = cap.fn(maxcpc.opt, maxcpc.clkltst,maxcpc.delta = 0.03) 
    }
    else if (rule.id != 5) { #No capping for rule 5      
      maxcpc.opt = cap.fn(maxcpc.opt, maxcpc.cap)       
    }
    print(paste("After capping for MAXCPC_CAP=", maxcpc.cap, ", MAXCPC_OPT=", maxcpc.opt))
    
    if (rule.id < 5){ # adding tolerance of 0.01
      if ((maxcpc.clkltst-maxcpc.opt)==0.01){ 
        maxcpc.opt = maxcpc.clkltst
        }
    }
    
    if (maxcpc.opt <= 0) maxcpc.opt = 0.01 
    fit_rules = c(1,2,3)
    result.df = data.frame(MKT_ID=mkt.id, DATE=run.date, MAXCPC_OPT=sprintf("%.2f",maxcpc.opt), RULE_ID=rule.id, MAXCPC_A=maxcpc.A, MAXCPC_B=maxcpc.B, MAXCPC_CAP=maxcpc.cap, MAXCPC_DELTA=maxcpc.cap-maxcpc.opt, NETREV_OPT=ifelse(rule.id %in% fit_rules, netrev.opt, NA), MAXCPC_OPT_ORIG=maxcpc.opt.orig, CID=cid, RF_CODE=rf_code,RSQ=ifelse(rule.id %in% fit_rules,rsq,NA),ARSQ=ifelse(rule.id %in% fit_rules,arsq, NA), INTERCEPT=ifelse(rule.id %in% fit_rules, maxcpc.int, NA), LIN_COEFF=ifelse(rule.id %in% fit_rules, maxcpc.coeff, NA), SQ_COEFF=ifelse(rule.id %in% fit_rules, maxcpc.sq.coeff, NA), PVAL_INT=ifelse(rule.id %in% fit_rules, pval.int, NA), PVAL_COEFF=ifelse(rule.id %in% fit_rules,pval.coeff, NA), PVAL_SQ_COEFF=ifelse(rule.id %in% fit_rules, pval.sq.coeff, NA))
	print("Resulting characteristics of market Id")  
  print(result.df)
    
  } else { #9 All bids = 0.01
    print(paste("Error: Found bad data for mkt id", mkt.id))
    result.df = data.frame(MKT_ID=mkt.id, DATE=run.date, MAXCPC_OPT=NA,RULE_ID=NA, MAXCPC_A=NA, MAXCPC_B=NA, MAXCPC_CAP=NA, MAXCPC_DELTA=NA, NETREV_OPT=NA, MAXCPC_OPT_ORIG=NA, CID=cid, RF_CODE=rf_code,RSQ=NA,ARSQ=NA)
  }
  
  return(result.df)
}
library(reshape2)
library(ggplot2)
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

 
 run.date = as.Date("2014-06-02")
 cnm=colnames(read.csv(paste(base_path,"data.csv",sep=""),sep="\t"))
 a=system(paste("grep ",  mktid, paste(" ",base_path,"run_data.tsv",sep=""),sep=""),intern=T,ignore.stderr=T)
 alltraffic.df=as.data.frame( t(sapply(1:length(a), function(x) strsplit(a, "\t")[[x]])) ) 
 colnames(alltraffic.df)=cnm
 alltraffic.df$DTE=as.Date(as.character(alltraffic.df$DTE))
 alltraffic.df=alltraffic.df[alltraffic.df$DTE!='2014-05-27',]
 alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))]=apply(alltraffic.df[,!(colnames(alltraffic.df) %in% c("DTE","RF_CODE"))],2,as.numeric)
 audits.csv = paste("_", run.date,".csv", sep="")
 mkt.id = mktid
 rf_code ="gfx"
print("Time Series of market Id")
print(alltraffic.df)
plotFandV(alltraffic.df)
quadratic.fn(mkt.id, rf_code, run.date)

