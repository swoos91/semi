library(dplyr)
setwd('c:\\semi\\최신\\6조')
sb_lib<-readRDS('2018 seongbook library monthly data (성북정보, 아리랑정보 제외할 것).rds')

###

sb_1<-read.csv('성북구/인기대출도서_2018-2.csv', skip=13, stringsAsFactors=F, header=T)
sb_1_name<-trimws(sb_1$서명, which='both')

 
# idx<-length(sb_lib$달빛마루도서관[[1]]$도서명)
# index <- c()
# for(i in 1:idx){
#   if(sb_lib$달빛마루도서관[[1]]$도서명[i] %in% sb_1_name){
#     index <- c(index,i)
#   }
# }
# 달빛마루<-sb_lib$달빛마루도서관[[1]][index,]
# 


lib_index = list()
     
for (library in names(sb_lib)){
  x<-sb_lib[[library]][[1]]
  idx<-length(x$도서명)
  index <- c()
  for(i in 1:idx){
    if(x$도서명[i] %in% sb_1_name){
      index <- c(index,i)
    }
  }
  lib_index[[library]] <- index
}


sb_lib$달빛마루도서관[[1]][lib_index[[1]],]
