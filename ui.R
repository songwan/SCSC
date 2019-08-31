#########################################################################################
library(leaflet)
library(shiny)
library(purrr)
library(rsconnect)
library(shinydashboard)
if(interactive()){
  dashboardPage(
    #########################################################################################
    
    dashboardHeader(
      # Message menus
      dropdownMenu(type = "messages",
                   messageItem(from = "Sales Dept", 
                               message = "Mobile Applications are available now.", icon = icon("mobile")),
                   messageItem(from = "New User", 
                               message = "Do you have any questions?", icon = icon("question"), time = "13:45"),
                   messageItem(from = "Support",  
                               message = "The new server is ready.", icon = icon("life-ring"), time = "2017-06-23")
      ),
      # Notification menus
      dropdownMenu(type = "notifications",
                   notificationItem(text = "5 new users today", icon("users")),
                   notificationItem(text = "12 items delivered", icon("truck"),
                                    status = "success"),
                   notificationItem(text = "Server load at 86%",
                                    icon = icon("exclamation-triangle"), status = "warning")
      ),
      
      # task menus
      dropdownMenu(type = "tasks",
                   badgeStatus = "success", 
                   taskItem(value = 90, color = "green", "mobile App"),  # Scroll bar
                   taskItem(value = 80, color = "aqua", "Server deployment"),
                   taskItem(value = 75, color = "yellow", "visualization"),
                   taskItem(value = 80, color = "red","Overall project")
      ),
      title=span(tagList(icon("bus"), "Dustwave"))
    ), #header title
    
    #########################################################################################
    ##검색 - 서버측 코드의 값은 input$searchText , input$searchButton임.
    dashboardSidebar(
      sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
      sidebarMenu(
        id = "tabs",
        menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard"),badgeLabel = "updated", badgeColor = "green"),
        # Charts
        menuItem("Charts", icon = icon("bar-chart-o"),
                 menuSubItem("Chart sub-item 1", tabName = "subitem1"),
                 menuSubItem("Chart sub-item 2", tabName = "subitem2")
        ),
        menuItem("Team",tabName="team", icon=icon("users")),
        
        # Control Slider : server에서 연동
        menuItemOutput("slider_sidebar"),
        # Text Input : server에서 연동
        menuItemOutput("Text_input")
      )
    ),
    
    #########################################################################################
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                h3("Fine Dust Concentration Map"),
                box(width=12,
                    fluidRow(
                      column(4, selectInput("concentration",label=h4("PM concentration"),
                                            choices=list("Overall","PM10","PM2.5"),selected=1)),
                      column(4, selectInput("route", label = h4("Bus Route"),
                                            choices=list("101"=1,"101-1"=2,"156"=3,"204"=4,"234"=5,"240"=6,"300"=7,"304"=8,"306"=9,"309"=10,
                                                         "323"=11,"323-1"=12,"349"=13,"356"=14,"401"=15,"403"=16,"405"=17,"410"=18,"410-1"=19,"413"=20,
                                                         "425"=21,"449"=22,"503"=23,"509"=24,"518"=25,"523"=26,"524"=27,"527"=28,"564"=29,"600"=30,
                                                         "609"=31,"618"=32,"623"=33,"649"=34,"650"=35,"651"=36,"653"=37,"655"=38,"706"=39,"708"=40,
                                                         "719"=41,"724"=42,"726"=43,"730"=44,"750"=45,"805"=46,"808"=47,"814"=48,"836"=49,"840"=50),
                                            selected=1)),
                      column(4, selectInput("stopname", label = h4("Bus stop"), choices=list("a","b","c")))
                    ),
                    actionButton("reset_button", "Reset view"),
                    leafletOutput("bus_station")
                )
                
                
                
        ),
        
        tabItem(tabName="subitem1",
                h2("subitem1 tab content"),
                fluidRow(
                  box(
                    title = "Plots",
                    plotOutput("plot1",height=250)
                  ),
                  box(
                    title = "Controls",
                    sliderInput("slider","Number of obs: " , 1, 100, 50)
                  )
                )
        ),
        tabItem(tabName="subitem2",
                box(width=12,
                    h1("Outline"),
                    h2("A Visualization of the Fine Dust Concentration"),
                    p("* The fine dust concentration can be divided into pm (fine dust) and pm 
                      (minute fine dust), which can be used to check the concentration of fine dust information."),
                    p("* This is a Web-based service-based service that develops the ui classification 
                      and predictive algorithm and analyzes the BigData of the particles in a fine dust 
                      measuring instrument (DustWave) and serves as a service within a radius of 1 km (within a radius of 1 km).."),
                    h2("Features"),
                    p("* We measure the latitude, longitude, time, and fine particulate matter concentration
                      using the fine dust measuring sensor and aduino."),
                    p("* The data to be utilized in conjunction with observational data include the weather
                      direction, weather type, weather type, weather type, humidity, and direction of weather provided by the weather agency.")
                    )
                    ),
        tabItem(tabName="team",
                box(
                  div(img(src="image3.png",height=350, width=710), style="text-align: center;"),
                  #img(src="image3.png",height=245, width=500,align="center"),
                  infoBox(width=6,"Woonjae Han (앱 개발)","portgus92@naver.com", icon=icon("mobile"),color='light-blue'),
                  infoBox(width=6,"Haeyin Kim (서버)","gn00090@naver.com", icon=icon("server"),color='light-blue'),
                  infoBox(width=6,"KyungEn Kim (통계)","kyungen@gmail.com", icon=icon("line-chart"),color='light-blue'),
                  infoBox(width=6,"Songwan Joun (시각화)","thddhks1256@gmail.com", icon=icon("map"),color='light-blue'),
                  br()
                  ,width = 12
                )
        ) 
                )
    )
    
    )
}

