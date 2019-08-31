########################################################################################################################
#1.localBusDb 구성

#1.1) 대구시 버스 정류장 데이터 수집 (버스 정류장 명)
daeguBus<-read.csv("D://DustWaveProject//data//daeguBus.csv")
BUS_NAME<-daeguBus$정류소.명
localBusDb<-data.frame(BUS_NAME)

#1.2) 버스 정류장 별 고유 ID 부여(BUS_ID)
BUS_ID<-c(1:length(localBusDb$BUS_NAME))
localBusDb$BUS_ID<-BUS_ID

#1.3) 버스 정류장 명 별 google Api LAT, LON 변환
library(rvest)
geo1<-paste0("http://maps.googleapis.com/maps/api/geocode/xml?sensor=false&language=ko&address='",localBusDb$BUS_NAME)
LAT<-c()
LON<-c()
for(i in 1:length(localBusDb$BUS_NAME)){
  LAT<-c(LAT,substr(xml_node(read_xml(geo1[i],encoding='UTF-8'),'location'),19,28))
  LON<-c(LON,substr(xml_node(read_xml(geo1[i],encoding='UTF-8'),'location'),43,53))
}
localBusDb$LAT<-LAT
localBusDb$LON<-LON




#1.4) 기상청 격자 위치 정보(gridX, grixY)
gridxy<-read.csv("D://0812//DustWaveProject//data//gridxy.csv")
localBusDb$gridX<-gridxy$gridX
localBusDb$gridY<-gridxy$gridY

####error수정 ####################################
#LON2<-c(2365:length(localBusDb$BUS_NAME))
#LAT2<-c(2365:length(localBusDb$BUS_NAME))
#LAT<-c(LAT,LAT2)
#LON<-c(LON,LON2)

########################################################################################################################
#2. 매쉬업 데이터 파악
library(XML)

#localBusDb의 gridX,Y값과 URL 연동
url1<-paste0("http://www.kma.go.kr/wid/queryDFS.jsp?gridx=",localBusDb$gridX)
url2<-paste0(url1,"&gridy=")
url3<-paste0(url2,localBusDb$gridY)

#최신 날씨 데이터 받아오기(완성)
fileUrl<-as.vector(url3)
doc1<-c()
for(i in 1:1000){
  doc1<-c(doc1,xmlTreeParse(fileUrl[i],useInternalNodes = TRUE))
}
doc2<-c()
for(i in 1001:2000){
  doc2<-c(doc2,xmlTreeParse(fileUrl[i],useInternalNodes = TRUE))}
doc3<-c()
for(i in 2001:3000){
  doc3<-c(doc3,xmlTreeParse(fileUrl[i],useInternalNodes = TRUE))}
doc4<-c()
for(i in 3001:length(url3)){
  doc4<-c(doc4,xmlTreeParse(fileUrl[i],useInternalNodes = TRUE))
}
doc<-c()
doc<-c(doc1,doc2,doc3,doc4)

rootNode<-c()
tm<-c();temp<-c();sky<-c();pty<-c();wfEn<-c();ws<-c();wd<-c();reh<-c()

for(i in 1:length(url3)){
  rootNode<-c(rootNode,xmlRoot(doc[[i]]))
  tm<-c(tm,xpathSApply(rootNode[[i]], "//tm",xmlValue)[1])
  temp<-c(temp,xpathSApply(rootNode[[i]], "//temp",xmlValue)[1])
  sky<-c(sky,xpathSApply(rootNode[[i]], "//sky",xmlValue)[1])
  pty<-c(pty,xpathSApply(rootNode[[i]], "//pty",xmlValue)[1])
  wfEn<-c(wfEn,xpathSApply(rootNode[[i]], "//wfEn",xmlValue)[1])
  ws<-c(ws,xpathSApply(rootNode[[i]], "//ws",xmlValue)[1])
  wd<-c(wd,xpathSApply(rootNode[[i]], "//wd",xmlValue)[1])
  reh<-c(reh,xpathSApply(rootNode[[i]], "//reh",xmlValue)[1])
}

##################################################################################################################
# dustWave dataframe 구성 (매쉬업 데이터 + 미세먼지 데이터 + BUS_ID)
##################################################################################################################
dustWave<-data.frame(BUS_ID)
dustWave$tm<-as.factor(tm)
dustWave$temp<-as.numeric(temp)
dustWave$sky<-as.factor(sky)
dustWave$pty<-as.factor(pty)
dustWave$wfEn<-as.factor(wfEn)
dustWave$ws<-as.numeric(ws)
dustWave$wd<-as.factor(wd)
dustWave$reh<-as.numeric(reh)

#tm(시간-yyyymmddhhMM), 
#temp (온도'C)
#sky(하늘상태코드-1:맑음,2:구름조금,3:구름많음,4:흐림), 
#pty(강수상태코드-0:없음,1:비,2:비/눈,3:눈/비,4:눈),
#wfEn(날씨영어-1.Clear,2.Partly Cloudy, 3.Mostly Cloudy, 4.Cloudy, 5.R ain Snow/Rain), 
#ws(풍속)
#wd(풍향 0~7(북,북동,동,남동,남,남서,서,북서))
#reh(습도%)


##################################################################################################################
# server 통신
##################################################################################################################
#server busDB upload
pp1<-paste0("http://107.20.131.63:3000/SETUPSTOP?ID=",localBusDb$BUS_ID)
pp2<-paste0(pp1,"&LON=")
pp3<-paste0(pp2,localBusDb$LON)
pp4<-paste0(pp3,"&LAT=")
pp5<-paste0(pp4,localBusDb$LAT)
pp6<-paste0(pp5,"&gridX=")
pp7<-paste0(pp6,localBusDb$gridX)
pp8<-paste0(pp7,"&gridY=")
pp9<-paste0(pp8,localBusDb$gridY)
pp10<-pp9[1:length(localBusDb$BUS_ID)]

for(i in 1:length(localBusDb$BUS_ID)){
  GET(pp10[i])
}




#############날씨정보 포함해서 보내기########################################################
p1<-paste0("http://34.224.156.142:3000/UPDATERECENT?S_ID=",dustWave$BUS_ID)
p2<-paste0(p1,"&PM_10=")
p3<-paste0(p2,data2$PM_10)
p4<-paste0(p3,"&PM_25=")
p5<-paste0(p4,data2$PM_25)
p6<-paste0(p5,"&PM_10_PRED=")
p7<-paste0(p6,data2$PM_10_PRED)
p8<-paste0(p7,"&PM_25_PRED=")
p9<-paste0(p8,data2$PM_25_PRED)
p10<-paste0(p9,"&TEMP=")
p11<-paste0(p10,data2$temp)
p12<-paste0(p11,"&WINDSPEED=")
p13<-paste0(p12,data2$ws)
p14<-paste0(p13,"&WEATHER=")
p15<-paste0(p14,data2$sky)
p16<-paste0(p15,"&WINDIRECTION=")
p17<-paste0(p16,data2$wd)
p18<-paste0(p17,"&HUMIDITY=")
p19<-paste0(p18,data2$reh)
p20<-paste0(p19,"&TIME=")
p21<-paste0(p20,data2$tm)
p22<-p21[1:length(data2$PM_10)]
for(i in 1:length(data2$PM_10)){
  GET(p22[i])
}