#####
#determination of the Seasonal components
#calculation of a seasonality score
#calculation of Seasonal components over mean errors
#####
#typeof(instrument1) = double
#typeof(benchmark) = double
#typeof(level) = character
#typeof(method) = character
#typeof(freq) = double
#typeof(starta) = double
#####
seasonalityF2<-function(instrument1,benchmark,level,method,freq,starta)
{
#get data to analyse
if(missing(benchmark))
  Y<-instrument1
else
  Y<-instrument1/benchmark
#set level
if(missing(level))
  level="price"
if(level!="return")
  level="price"
#set method
if(missing(method))
  method<-"loess"
if(method!="classical")
  method<-"loess"
#set freq
if(missing(freq))
  freq<-12
#set starta
if(missing(starta))
  starta=c(1900,1)
#set time series
TS<-ts(Y,frequency=freq,start=starta)
#set additive model
Y<-log(TS)
if(level=="return"){
#transform price into returns
  Y<-diff(Y)}
#decompose
if(method=="loess")
  Ycomponents<-stl(Y,s.window="periodic")
else
  Ycomponents<-decompose(Y)
#sum absolute seasonal components and mean absolute remainders
if(method=="loess"){
  Seasons<-Ycomponents[[1]][,1][1:freq]
  Remainders<-Ycomponents[[1]][,3]}
else{
  Seasons<-Ycomponents$figure
  Remainders<-Ycomponents$random
}
absSeasons<-abs(Seasons)
sumSeasons<-sum(absSeasons)
absRemainders<-abs(Remainders)
meanRemainders<-mean(absRemainders,na.rm=TRUE)
Seasons2<-data.frame(1)
for(i in 1:freq){
Seasons2[i]<-Seasons[i]/mean(absRemainders[cycle(absRemainders)==i])
}
Seasons2<-as.numeric(Seasons2)
#ratio
ratio<-sumSeasons/meanRemainders
#results
return(list(score=ratio,seasons=Seasons,seasonsOvEr=Seasons2))
}
###end function
