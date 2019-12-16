load('2018년 성북구 도서관별 대출목록')
library(dplyr)
a<- sb_lib_list$정릉도서관[[4]]$대출건수-sb_lib_list$정릉도서관[[5]]$대출건수

library(xlsx)


name<-c()
#name<-as.data.frame(name)
for (idx in 1:12) {
  temp<-read.csv(paste0('c:\\semi\\6조\\성북구\\인기대출도서_2018-',idx,'.csv'), 
                 skip=13, header=T, stringsAsFactors=F)
  temp<-temp$서명
  print(length(name))
  name<-c(name, temp)
  print(length(name))
  name<-unique(name)
  print(length(name))
}

