#subclinical rate - mixture distribution as option 1
require(dplyr)
require(RCurl)
iters=10000
dfoutbreakdata <-read.csv(text=getURL("https://raw.githubusercontent.com/datagook/qracourse_oct2020/main/data_outbreaks_ca.csv"))
dfoutbreakdata.subclin<-dfoutbreakdata %>% dplyr::filter(year %in% c("2011","2014","2016"))

o.2011=rbeta(iters, 
             (dfoutbreakdata.subclin[1,]$cases_subclinical + 1),
             (dfoutbreakdata.subclin[1,]$cases_all - dfoutbreakdata.subclin[1,]$cases_subclinical + 1)) ; hist(o.2011)

o.2014p=rbeta(iters, 
             (dfoutbreakdata.subclin[2,]$cases_subclinical + 1),
             (dfoutbreakdata.subclin[2,]$cases_all - dfoutbreakdata.subclin[2,]$cases_subclinical + 1)) ; hist(o.2014p)

o.2014r=rbeta(iters, 
             (dfoutbreakdata.subclin[3,]$cases_subclinical + 1),
             (dfoutbreakdata.subclin[3,]$cases_all - dfoutbreakdata.subclin[3,]$cases_subclinical + 1)) ; hist(o.2014r)

o.2016=rbeta(iters, 
             (dfoutbreakdata.subclin[4,]$cases_subclinical + 1),
             (dfoutbreakdata.subclin[4,]$cases_all - dfoutbreakdata.subclin[4,]$cases_subclinical + 1)) ; hist(o.2016)


o.dat=cbind(o.2011, o.2014p, o.2014r, o.2016)

subclin.samples=rep(NA, iters)
n.outbreaks=ncol(o.dat)
for(i in 1:iters){
  which.outbreak=sample(1:n.outbreaks,1)
  subclin.samples[i] = o.dat[i,which.outbreak] 
}
hist(subclin.samples, freq=F)

# option 2 is fixed effects meta analysis

subclin.samples.ma<-rbeta(iters, 
                          sum(dfoutbreakdata.subclin$cases_subclinical), 
                          sum(dfoutbreakdata.subclin$cases_all)-sum(dfoutbreakdata.subclin$cases_subclinical))
summary(subclin.samples.ma)
hist(subclin.samples.ma, freq = F)

