##################################################################################################################
##작업 디렉토리 설정 / 패키지 및 라이브러리 설치
##################################################################################################################
#install.packages("leaflet")
setwd("D://홀로닉스//MTCC결과발표//시연자료//DustWaveProject")
library(leaflet)
library(shiny)
library(purrr)
library(rsconnect) #Deployment Interface for R Markdown Documents and Shiny Applications
##################################################################################################################
#데이터, 그림, 아이콘 설정
##################################################################################################################


#1. 데이터 불러오기
#localBusDb
localBusDb<-read.csv("data//localBusDb.csv")
localBusDb[length(localBusDb$BUS_ID)+1,]<-c(length(localBusDb$BUS_ID)+1,"묵어리",128.5555,36.5555,90,90)
localBusDb[length(localBusDb$BUS_ID)+1,]<-c(length(localBusDb$BUS_ID)+1,"묵어리",128.5655,36.5555,90,90)
localBusDb[length(localBusDb$BUS_ID)+1,]<-c(length(localBusDb$BUS_ID)+1,"묵어리",128.5755,36.5555,90,90)
localBusDb[length(localBusDb$BUS_ID)+1,]<-c(length(localBusDb$BUS_ID)+1,"묵어리",128.5855,36.5555,90,90)
#recentdata

##################################################################################################################
# server 에서 다운로드 및 파싱 -> 로컬 위치ID와 매칭
##################################################################################################################
#install.packages("curl")
#install.packages("RCurl")
#install.packages("XML")
library(XML)
library(curl)
library(RCurl)
library(httr)
library(jsonlite)
library(stringr)
#recentdataall1<-getURI("http://107.20.131.63:3000/RECENTDATAALL")
recentdataall2<-str_split(recentdataall1,"_id")
recentdataall3<-unlist(str_split(recentdataall2[[1]][2],"\\\""))
str_sub(recentdataall3[c(6,8,10,12,14,16,18,20,22,24)],start=2,end=-2)
str_sub(recentdataall3[27],start=1,end=-1)

#recentdata dataframe
recentdata<-data.frame(matrix(ncol=11))
p<-str_sub(recentdataall3[c(5,7,9,11,13,15,17,19,21,23,25)],end=-1) #변수 추출(홀수)
colnames(recentdata)<-c("BUS_ID",p[2:11])
length(recentdataall2[[1]])
for(i in 1:length(recentdataall2[[1]])){
  recentdataall4<-unlist(str_split(recentdataall2[[1]][i+1],"\\\""))
  q1<-str_sub(recentdataall4[c(6,8,10,12,14,16,18,20,22,24)],start=2,end=-2) #값 추출(짝)
  q2<-str_sub(recentdataall4[27],start=1,end=-1) #time 값
  d1<-c(q1,q2)
  recentdata[i,]<-d1
}

recentdata<-recentdata[1:3010,]
###error를 방지하기 위한 fake data 삽입
recentdata[length(recentdata[,1])+1,]<-c(length(recentdata[,1])+1,10,10,1,1,2,3,4,5,6,2)
recentdata[length(recentdata[,1])+1,]<-c(length(recentdata[,1])+1,40,30,1,1,2,3,4,5,6,2)
recentdata[length(recentdata[,1])+1,]<-c(length(recentdata[,1])+1,100,75,1,1,2,3,4,5,6,2)
recentdata[length(recentdata[,1])+1,]<-c(length(recentdata[,1])+1,200,200,1,1,2,3,4,5,6,2)
recentdata$BUS_ID<-as.numeric(recentdata$BUS_ID)
library(plyr)
######################################################################################
##############localBusDb와 매칭하기 by S_ID
recentdata<-join(localBusDb,recentdata,by='BUS_ID')
recentdata$PM_10<-as.numeric(recentdata$PM_10)
recentdata$PM_25<-as.numeric(recentdata$PM_25)
################################################################################################################
##################################################################################################################

#2. 데이터 처리
##data2 : 원 데이터 + PM_xx_PRED, PM_xx_COLOR, PM_XX_LEVEL, TEMP, WINDSOEED, ...

##함수 정의
getColor10 <-function(dataset){
  sapply(dataset$PM_10,function(PM_10){
    if(PM_10<=30){
      "blue"
    }else if(PM_10<=80){
      "green"
    }else if(PM_10<=150)
    { "yellow"
    }else if(PM_10>150)
    {
      "red"
    }else{
      "grey"
    }
  })
}
getColor25 <-function(dataset){
  sapply(dataset$PM_25,function(PM_25){
    if(PM_25<=15){
      "blue"
    }else if(PM_25<=50){
      "green"
    }else if(PM_25<=100)
    {
      "yellow"
    }else if(PM_25>100)
    {
      "red"
    }else{
      "grey"
    }
  })
}
getLevel10 <-function(dataset){
  sapply(dataset$PM_10,function(PM_10){
    if(PM_10<=30){
      "좋음"
    }else if(PM_10<=80){
      "보통"
    }else if(PM_10<=150)
    { "나쁨"
    }else if(PM_10>150)
    {
      "매우나쁨"
    }else{
      "미관측"
    }
  })
}
getLevel25 <-function(dataset){
  sapply(dataset$PM_25,function(PM_25){
    if(PM_25<=15){
      "좋음"
    }else if(PM_25<=50){
      "보통"
    }else if(PM_25<=100)
    {
      "나쁨"
    }else if(PM_25>100)
    {
      "매우나쁨"
    }else{
      "미관측"
    }
  })
}
getPred10<-runif(length(recentdata[,1]),0,200)
getPred25<-runif(length(recentdata[,1]),0,150)

##변수 저장
PM_10_COLOR<-getColor10(recentdata)
PM_25_COLOR<-getColor25(recentdata)
PM_10_LEVEL<-getLevel10(recentdata)
PM_25_LEVEL<-getLevel25(recentdata)
PM_10_PRED<-getPred10
PM_25_PRED<-getPred25
gridX<-recentdata$gridX #기상청사용위한 위치정보.
gridY<-recentdata$gridY


##data2에 새로운 변수를 저장
data2<-data.frame(recentdata,PM_10_COLOR,PM_25_COLOR, PM_10_LEVEL,PM_25_LEVEL)
data2$LAT<-as.numeric(data2$LAT)
data2$LON<-as.numeric(data2$LON)
##아이콘
length(data2[,1])

leafIcon<-iconList(
  blue = makeIcon("blue_leaf.png",18,18),
  green =makeIcon("green_leaf.png",18,18),
  yellow =makeIcon( "yellow_leaf.png",18,18),
  red =makeIcon( "red_leaf.png",18,18),
  grey = makeIcon("grey_leaf.png",18,18)
)

#데이터 그룹
datagroup_Overall<-split(data2,data2$PM_10_COLOR)
datagroup_PM10<-split(data2,data2$PM_10_COLOR)
datagroup_PM25<-split(data2,data2$PM_25_COLOR)


##################################################################################################################
#서버 프로그래밍
##################################################################################################################
shinyServer<-function(input,output,session){
  output$text1<-renderText({
    return(input$stopname)
  })
  output$bus_station<-renderLeaflet({
    
    var<-switch(input$concentration,
                "Overall"=datagroup_Overall,
                "PM10"=datagroup_PM10,
                "PM2.5"=datagroup_PM25)
    
    var2<-switch(input$concentration,
                 "Overall"=data2$PM_10_COLOR,
                 "PM10"=data2$PM_10_COLOR,
                 "PM2.5"=data2$PM_25_COLOR)
    
    
    leaflet(data=data2) %>%
      setView(lat=35.867010, lng=128.581174, zoom=12) %>%
      addProviderTiles(providers$OpenStreetMap,group="Map Only",
                       options = providerTileOptions(minZoom = 6, maxZoom=18)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(minZoom = 6, maxZoom=18)) %>%
      addLegend(labels = c("좋음","보통","나쁨","매우나쁨"),
                colors=c("blue","green","yellow","red")) %>%
      
      addMarkers(data=var$blue,group="좋음",
                 lng=var$blue$LON,####################################
                 lat=var$blue$LAT,
                 icon=leafIcon$blue,
                 label=~as.character(NAME),
                 popup=~as.character(PM_10),
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))%>%
      addMarkers(data=var$green,group="보통",
                 lng=var$green$LON,####################################
                 lat=var$green$LAT,
                 icon=leafIcon$green,
                 label=~as.character(NAME),
                 popup=~as.character(PM_10),
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))%>%
      addMarkers(data=var$yellow,group="나쁨",
                 lng=var$yellow$LON,####################################
                 lat=var$yellow$LAT,
                 icon=leafIcon$yellow,
                 label=~as.character(NAME),
                 popup=~as.character(PM_10),
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))%>%
      addMarkers(data=var$red,group="매우나쁨",
                 lng=var$red$LON,####################################
                 lat=var$red$LAT,
                 icon=leafIcon$red,
                 label=~as.character(NAME),
                 popup=~as.character(PM_10),
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))%>%
      addCircles(data=data2,group="Colored",color=var2) %>%
      addLayersControl(
        baseGroups=c("Colored","Map Only"),
        overlayGroups = c("좋음","보통","나쁨","매우나쁨"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("보통","나쁨","매우나쁨")) #default로 무엇을 나타낼지 결정.  
  })
  
  observe({
    input$reset_button
    leafletProxy("bus_station") %>% setView(lat=35.867010, lng=128.581174, zoom=12)
  })
  
  ###########################################################################################
  # sidebar의 menuItemOutput() 연동
  output$slider_sidebar = renderMenu({
    sidebarMenu(uiOutput("input1"))
  })
  output$Text_input = renderMenu({
    sidebarMenu(uiOutput("input2"))
  })
  output$input1 = renderUI({
    sliderInput("Slider", label = "Threshold", 1, 20, 5)
  })
  output$input2 = renderUI({
    textInput("Text", label = "Text input")
  })
  ############################################################################################ 
  
  ############################################################################################  
  set.seed(122)
  histdata<-rnorm(500)
  
  output$plot1<-renderPlot({
    data<-histdata[seq_len(input$slider)]
    hist(data)
    ############################################################################################
  })
}

#deployApp()
#rsconnect::showLogs() 
#runApp("C://Users//HP//Desktop//sem1//SCSC//6_16//6_16",display.mode = "showcase")
