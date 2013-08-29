Sys.setenv(TZ = "Europe/London")
library(quantmod)
library(PerformanceAnalytics)
path.src <- "C:/Users/Keiran/Documents/Backtest_Source/R/"

# # hard coded paths for debug - need trailing slash
# path.in <- "Y:/Saxons Cloud Data/Cloud Data/Trades_NT7 Backtest/Trades_Reports/MR/2006 2013 Q2/08 05 12/Buys/240/"
# filenames <- grep("BT2.*_pnl_daily.csv",dir(path.in),value=TRUE)
# filestem <- "Test BT2"
# path.out <- "Y:/SIS Cloud/Trades_NT7 Backtest/Portfolio_Reports/"
# setwd(path.in)
# rel.rtns <- FALSE
# start.end.dates <- c("2006-01-01","2013-06-30") 
# print(filenames)

# load benchmark files
index.path <- "Y:/Saxons Cloud Data/Data History/Benchmarks Indices/Other/"
new.edge.csv <- read.csv(paste0(index.path,"NewEdge_CTA_Historical.csv"),header=F,stringsAsFactors=F)
vals <- as.numeric(strsplit(new.edge.csv[,2],"%"))/100
new.edge.xts <- xts(vals,as.Date(new.edge.csv[,1],format="%d/%m/%Y",tz="Europe/London"))
new.edge.xts <- na.omit(new.edge.xts)
colnames(new.edge.xts) <- "NewEdge CTA Index"
print("loaded NewEdge file")

# load S&P500 index
getSymbols("^GSPC",src="yahoo",from=as.Date("2001-01-01"))
sp500 <- Ad(GSPC)
print("loaded S&P500")

# load pnl files, combine into returns object
AUM <- 1.e8
ref.xts <- merge(new.edge.xts,ROC(sp500,1),fill=0)
ref.xts[is.na(ref.xts)] <- 0
colnames(ref.xts) <- c("NewEdge.CTA","SP500")
rtns.xts <- ref.xts
meta.data <- NULL
last.date <- index(first(ref.xts))
first.date <- index(last(ref.xts))
for (f in filenames) {
  f.csv <- read.csv(f,sep=",")
  f.xts <- xts(f.csv[,2],as.Date(f.csv[,1],format="%Y-%m-%d"))
  m <- strsplit(colnames(f.csv)[2],'.',fixed=TRUE)
  meta.data <- rbind(meta.data,m[[1]][c(4,5,6,8)])
  colnames(f.xts) <- m[[1]][4]
  rtns.xts <- merge(rtns.xts,f.xts/AUM,fill=0)
  if (index(last(f.xts)) > last.date) last.date <- index(last(f.xts))
  if (index(first(f.xts)) < first.date) first.date <- index(first(f.xts))
  print(paste(m,first.date,last.date,sep="  "))
}
rtns.xts <- rtns.xts[index(rtns.xts) >= first.date & index(rtns.xts) <= last.date]
print("loaded daily pnl files")
meta.data <- data.frame(meta.data)
colnames(meta.data) <- c("ccy.pair","strategy","timeframe","buy.sell")

# make appropriate portfolio returns
if (rel.rtns) {
  ptf.xts <- xts(rowMeans(rtns.xts[,-c(1:2)]),index(rtns.xts))
} else {
  ptf.xts <- xts(rowSums(rtns.xts[,-c(1:2)]),index(rtns.xts))
}
colnames(ptf.xts) <- "Portfolio"
ptf.daily <- merge(ptf.xts,ref.xts,all=FALSE)

# trim to specified window
start.date <- as.Date(start.end.dates[1],format="%d/%m/%Y")
end.date <- as.Date(start.end.dates[2],format="%d/%m/%Y")
ptf.daily <- ptf.daily[index(ptf.daily) >= start.date & index(ptf.daily) <= end.date]
print(paste0("dimension of ptf.daily: ",dim(ptf.daily)))
# make monthly returns
ep <- endpoints(index(ptf.daily),"months")
ptf.monthly <- period.sum(ptf.xts,ep)
ep <- endpoints(index(ref.xts),"months")
new.edge.monthly <- period.sum(ref.xts[,1],ep)
sp500.monthly <- period.sum(ref.xts[,2],ep)
ptf.monthly <- merge(ptf.monthly,new.edge.monthly,sp500.monthly,all=FALSE)
colnames(ptf.monthly) <- c("Portfolio","NewEdge.CTA.Index","SP500")


# # download treasury rate data if necessary
# if (!exists("DTB3") || index(last(DTB3)) < index(last(processed$pnl.daily))) {
#   getSymbols('DTB3',src='FRED') # risk free rate
#   rf.rate.daily <- na.locf(DTB3/100)
#   rf.rate.weekly <- period.apply(DTB3/100,endpoints(rf.rate.daily,'weeks'),FUN=mean)
#   rf.rate.monthly <- period.apply(DTB3/100,endpoints(rf.rate.daily,'months'),FUN=mean)
# }
# print("loaded treasury rate data")

setwd("C:/Temp/TeX_Tmp/")
write.zoo(ptf.daily,file=paste0(filestem,"_pnl_daily.csv"),sep=",")
write.zoo(ptf.monthly,file=paste0(filestem,"_pnl_monthly.csv"),sep=",")

save(filenames,rtns.xts,meta.data,AUM,ptf.daily,ptf.monthly,filestem,path.out,rel.rtns,file="temp_rtns.RData")
print("saved temp_rtns.RData")


library(knitr)

### Set knitr options
opts_chunk$set(echo=FALSE, concordance=TRUE)

### Create a file name for the output file
ptfreport <- paste0(filestem,".tex")

### Run knitr on the .Rnw file to produce a .tex file
knit(paste0(path.src,"PortfolioReport.Rnw"),output=ptfreport)


### Run texi2pdf on the .tex file within R or process it from your latex system
tryCatch(tools::texi2pdf(ptfreport), error=function(e) print(e), finally=traceback())


