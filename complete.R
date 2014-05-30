# complete ver
# 필요한 테이블 : givestation
# 필요한 함수 : arrivetext, bnums => uniq_bnums, hnums => uniq_hnums, 

library(shiny)
library(dplyr)
library(XML)
library(stringr)

# 전체 저상버스 노선 정류장 테이블
buscomplete <- read.csv("buscomplete.csv", header=T, stringsAsFactors=F, fileEncoding = "UTF-8")
givestation <- read.csv("givestation.csv", header=T, stringsAsFactors=F, fileEncoding = "UTF-8")
givestation$arsid2 <- ifelse(str_length(givestation$arsid) == 5, givestation$arsid, paste("0", givestation$arsid, sep="")) 
# 복지관 경유하는 노선번호
bnums <- read.csv("bnums.csv", header=T, stringsAsFactors=F, fileEncoding = "UTF-8")
uniq_bnums <- unique(bnums[,1])
# 병원 경유하는 노선번호
hnums <- read.csv("hnums.csv", header=T, stringsAsFactors=F, fileEncoding = "UTF-8")
uniq_hnums <- unique(hnums[,1])

findbh <- function(x) {
  for (i in 1:length(uniq_bnums)) {
    bres <- identical(x, uniq_bnums[i])
    if (bres == TRUE) break
  }
  for (j in 1:length(uniq_hnums)) {
    hres <- identical(x, uniq_hnums[j])
    if (hres == TRUE) break
  }
  if (bres == TRUE && hres == TRUE) return("이 버스는 복지관과 병원 모두 경유하는 저상버스 노선입니다.") # 복지관, 병원 모두 해당
  else if (bres == TRUE && hres == FALSE) return("이 버스는 복지관을 경유하는 저상버스 노선입니다.") # 복지관
  else if (bres == FALSE && hres == TRUE) return("이 버스는 병원을 경유하는 저상버스 노선입니다.") # 병원
  else if (bres == FALSE && hres == FALSE) return("") # 둘다 아니다
}


getsta <- function(busRouteId) {
  library(XML)
  mykey <- "&ServiceKey=pQjoSAThlQ%2BBcK5qq%2BobcE7XjOyBaTT1EzvPstpGXUqZjw38Mo50xnW%2BWqNeBQVi%2FHKlmBiDqZm20zOiTz6dXg%3D%3D"
  busurl4 <- "http://ws.bus.go.kr/api/rest/busRouteInfo/getStaionByRoute?"
  # 노선별 경유 정류소 조회 서비스
  busRouteId <- paste("&busRouteId=", busRouteId, sep="")
  urle <- paste(busurl4, mykey, busRouteId, sep="")
  # cat(urle)
  result <- xmlTreeParse(urle, useInternal=TRUE)
  rootNode <- xmlRoot(result)
  busrouteid <- xpathSApply(rootNode,"//busRouteId",xmlValue)
  busroutenm <- xpathSApply(rootNode,"//busRouteNm",xmlValue)
  seq <- xpathSApply(rootNode,"//seq",xmlValue)
  station <- xpathSApply(rootNode,"//station",xmlValue)
  stationname<- xpathSApply(rootNode,"//stationNm",xmlValue)
  stationno <- xpathSApply(rootNode,"//stationNo",xmlValue)
  stable <- cbind(busrouteid, busroutenm, seq, station, stationname, stationno)
  return(stable)
} 

findbusall <- function(x) {
  library(dplyr)
  bustable <- filter(buscomplete, routenum==x)
  return(bustable)
}

haptable <- function(routenum) {
  library(stringr)
  library(dplyr)
  allt <- findbusall(routenum)
  lastchr <- substr(routenum, str_length(routenum), str_length(routenum))
  if (lastchr %in% LETTERS) routenum <- substr(routenum, 1, str_length(routenum)-1)
  busRouteId <- paste("30", routenum, ifelse(str_detect(routenum, "A") == TRUE, "00", ifelse(str_detect(routenum, "B") == TRUE, "11", "00")), sep="")
  stable <- getsta(busRouteId)
  combus <- cbind(stable, allt)
  return(combus)
}

getarrivetext <- function(busRouteId) {
  library(XML)
  busurl8 <- "http://ws.bus.go.kr/api/rest/buspos/getLowBusPosByRtid?"
  # 노선ID로 저상버스들의 위치정보를 조회한다.
  mykey <- "&ServiceKey=pQjoSAThlQ%2BBcK5qq%2BobcE7XjOyBaTT1EzvPstpGXUqZjw38Mo50xnW%2BWqNeBQVi%2FHKlmBiDqZm20zOiTz6dXg%3D%3D"
  busRouteId <- paste("&busRouteId=", busRouteId, sep="") # 노선ID
  urli <- paste(busurl8, mykey, busRouteId, sep="")
  # cat(urli)
  result <- xmlTreeParse(urli, useInternal=TRUE)
  rootNode <- xmlRoot(result)
  ord <- xpathSApply(rootNode,"//sectOrd",xmlValue)
  firstord <- ord[1]
  secondord <- ord[2]
  return(as.integer(c(firstord, secondord)))
}

arrivetext <- function(name1, name2) {
  library(dplyr)
  hapt <- filter(haptable(name1), stationno == name2) 
  ord <- as.integer(hapt[1,8]) 
  getord <- as.integer(getarrivetext(hapt[1,1]))
  firstord <- getord[1]
  secondord <- getord[2]
  difford <- firstord - ord
  diffsord <- secondord - ord
  if (difford < 0) return("버스가 아직 대기중입니다.")
  else if (difford == 0) {
    smin <- diffsord * 2
    return(paste("버스가 곧 도착합니다.", " 다음 버스는 ", smin, "분 후에 도착할 예정입니다.", sep=""))
  }
  else {
    min2 <- difford * 2
    smin <- diffsord * 2
    return(paste(min2, "분 후에 버스가 도착할 예정입니다. 다음 버스는 ", smin, "분 후에 도착할 예정입니다.", sep=""))
  }
}
