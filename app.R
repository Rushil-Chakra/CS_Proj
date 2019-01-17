#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(googlecalendar)
library(ggplot2)
library(XML)
library(quantmod)
library(viridis)
source("helpers.R")

#weather setup for Fremont, CA
dat <- (xmlParse("http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22fremont%2C%20ca%22)&format=xml&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys") %>%
        xmlToList())[1]
#dat <- xmlToList(data)
#dat <- dat[1]
#set up time
currentTime <- Sys.time() %>% substring(12,16) %>% strsplit(split = ':')
currTime <- as.numeric(currentTime[[1]][[1]]) + as.numeric(currentTime[[1]][[2]])/60
#Not sure if the as.vector part is needed
sunrise <- (gsub(":"," ", dat$results$channel$astronomy[1]) %>% strsplit(split=" "))[[1]] %>% as.vector()
#sunrise <- sunrise[[1]] %>% as.vector()
sunset <- (gsub(":", " ", dat$results$channel$astronomy[2]) %>% strsplit(split=" "))[[1]] %>% as.vector()
#sunset <- sunset[[1]] %>% as.vector()
#returns military conversion of time
military <- function(x){
    if(x[3] == "pm"){
        hours <- 12
    }
    else{
        hours <- 0
    }
    return(hours + as.numeric(x[1]) + as.numeric(x[2])/60)
}
#set up weather continued
sunlight <- military(sunrise)
moonlight <- military(sunset)
#I  have no idea what this is supposed to be
moonx <- ((moonlight + (24 - moonlight + sunlight) %% 24)/24)/12
temperature <- dat$results$channel$item$condition[3]
weather <- dat$results$channel$item$condition[4]
windSpeed <- dat$results$channel$wind[3]
#get wind direction into cardinal directions
#dir <- round(as.numeric(dat$results$channel$wind[[2]])/45)
dir <- (dat$results$channel$wind[[2]] %>% as.numeric())/45 %>% round()
directions <- c("North", "North East", "East", "South East", "South", "South West", "West", "North West")
forecast <- list()
for(i in 7:16){
    forecast <- append(forecast, dat[[1]][[1]][[13]][i])
}
#background setup
color <- 'yellow'
if((currTime < sunlight && currTime > 0) || (currTime > moonlight && currTime <24)){
    color<- 'purple'
}
#stock setup #last price, change, change %
SPY <- c(getQuote('SPY')[[2]], getQuote('SPY')[[3]], getQuote('SPY')[[4]])
AMD <- c(getQuote('AMD')[[2]], getQuote('AMD')[[3]], getQuote('AMD')[[4]])
NVDA <- c(getQuote('NVDA')[[2]], getQuote('NVDA')[[3]], getQuote('NVDA')[[4]])
SOXL <- c(getQuote('SOXL')[[2]], getQuote('SOXL')[[3]], getQuote('SOXL')[[4]])
#list of stocks to show as portfolio
stocks <- list(SPY, AMD, NVDA, SOXL)

#changes color based on whether markets open and making money or losing money
stockColor <- function(x){
    if(as.numeric(x[[2]]) < 0){
        return('red')
    }
    else{
        return('darkgreen')
    }
}
stockBackground <- 'white'
stockText <- 'black'
if(currTime > 13 || currTime < 6.5){
    stockBackground <- 'black'
    stockText <- 'white'
}

#set up moisture
moisture <- read.csv('~/Desktop/test/codes/Home_mon/Moisture-Sensor-master/data_log.csv')
moist_text <- "Healthy"
moist_color <- 'green'
moist_reading <- moisture[length(moisture[,1]),3]
#threshold for how moist the water should be, higher value = dryer, right now its at 25%
if(moist_reading <= 768){
    moist_text <- 'Danger'
    moist_color <- 'red'
}

gPalette <- c("#5AB953", "#71D161")

ui <- dashboardPage(skin = color,
    dashboardHeader(title = paste0(temperature,"˚ F ", weather)),
    dashboardSidebar(
        #creates sidebar stuff
        sidebarMenu( 
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Weather", tabName = "weather", icon = icon("cloud")),
            menuItem("Stocks", tabName = "stocks", icon = icon("usd")),
            menuItem("Plant Health", tabName = "moisture", icon = icon("leaf"))
        )
    ),
    dashboardBody(
        tabItems(
           #the extra pages for each sidebar thing
            tabItem(tabName = "dashboard",
                #google calendar
                box(
                    htmlOutput('frame')
                ),
               #stock portfolio
               #figure out how to make this into a for loop, use a function, i is input output is vector of stock
                fluidRow(
                    box(
                        textOutput('prices'),style=paste('background-color: ',stockBackground),
                        tags$table(style=paste("width: 100%; color:", stockText), 
                            tags$tr(tags$th("Ticker"), tags$th("Last Price"), tags$th("Change"), tags$th("Change %")),
                            tags$tr(style = paste("color: ", stockColor(stocks[[1]])), tags$th("SPY"), tags$td(stocks[[1]][[1]]), tags$td(stocks[[1]][[2]]), tags$td(stocks[[1]][[3]])),
                            tags$tr(style = paste("color: ", stockColor(stocks[[2]])), tags$th("AMD"), tags$td(stocks[[2]][[1]]), tags$td(stocks[[2]][[2]]), tags$td(stocks[[2]][[3]])),
                            tags$tr(style = paste("color: ", stockColor(stocks[[3]])), tags$th("NVDA"), tags$td(stocks[[3]][[1]]), tags$td(stocks[[3]][[2]]), tags$td(stocks[[3]][[3]])),
                            tags$tr(style = paste("color: ", stockColor(stocks[[4]])), tags$th("SOXL"), tags$td(stocks[[4]][[1]]), tags$td(stocks[[4]][[2]]), tags$td(stocks[[4]][[3]]))
                        )
                    ),
                    #wind speed
                    box(
                        textOutput('weathers'),
                        tags$table(style = 'width: 100%',
                            tags$tr(tags$th('Wind Speed (mph)'), tags$th('Wind Direction')),
                            tags$tr(tags$td(dat$results$channel$wind[[3]]), tags$td(directions[dir]))
                        )
                    ),
                    #plant health
                    valueBoxOutput("Plants"),
                    #xkcd
                    htmlOutput('image')
                )
            ),
            #weather
            tabItem(tabName = "weather",
                #weather for today + next 5 days
                fluidRow(
                    box(width = 2,
                        textOutput('Today'),
                        tags$body("Today"), br(),
                        tags$body(paste0(forecast[[1]][[5]]," ˚F - ", forecast[[1]][[4]])," ˚F"), br(),
                        tags$body(forecast[[1]][[6]])
                    ),
                   
                    box(width = 2,
                        tags$body("Tomorrow"), br(),
                        tags$body(paste0(forecast[[2]][[5]]," ˚F - ", forecast[[1]][[4]])," ˚F"), br(),
                        tags$body(forecast[[2]][[6]])
                    ),
                    box(width = 2,
                        tags$body(format(Sys.Date()+2,'%m/%d/%y')), br(),
                        tags$body(paste0(forecast[[3]][[5]]," ˚F - ", forecast[[3]][[4]])," ˚F"), br(),
                        tags$body(forecast[[3]][[6]])
                    ),
                    box(width = 2,
                        tags$body(format(Sys.Date()+2,'%m/%d/%y')), br(),
                        tags$body(paste0(forecast[[4]][[5]]," ˚F - ",forecast[[4]][[4]])," ˚F"), br(),
                        tags$body(forecast[[4]][[6]])
                    ),
                    box(width = 2,
                        tags$body(format(Sys.Date()+2,'%m/%d/%y')), br(),
                        tags$body(paste0(forecast[[5]][[5]]," ˚F - ",forecast[[5]][[4]])," ˚F"), br(),
                        tags$body(forecast[[5]][[6]])
                        
                    ),
                    box(width = 2,
                        tags$body(format(Sys.Date()+2,'%m/%d/%y')), br(),
                        tags$body(paste0(forecast[[6]][[5]]," ˚F - ",forecast[[6]][[4]])," ˚F"), br(),
                        tags$body(forecast[[6]][[6]])
                    )
                ),
                # current time on 24 dial, spins to show whether sunlight or moonlight
                fluidRow(
                    box(width=12,
                        plotOutput('timeplot')
                    )
                )   
            ),
            #looks at stocks of any company, last price on bottom, can be adjusted for inflation, needs to have indicators added
            tabItem(tabName = "stocks",
                fluidRow(
                    column(3,
                        #panel for lookup
                        wellPanel(
                        textInput('symb',label=h3(''),value='SPY'),
                        dateRangeInput("dates", 
                            "Date range",
                            start = "2017-01-01", 
                            end = as.character(Sys.Date())
                        ),
                        checkboxInput("adjust", 
                        "Adjust prices for inflation", value = FALSE)
                        )
                    ),
                    #plot of the chart
                    column(9,
                        plotOutput('stockplot'),
                            verbatimTextOutput('text')
                    )
                )
            ),
            #plant stuff
            tabItem(tabName = 'moisture',
                box(
                    plotOutput('plantPlot')
                )
            )
        )
    )
)


server <- function(input, output) {
   # gc_auth(new_user = TRUE)
    #setup calendar
    test <- paste0("https://calendar.google.com/calendar/embed?showPrint=0&amp;showCalendars=0&amp;mode=WEEK&amp;height=600&amp;wkst=1&amp;bgcolor=%23FFFFFF&amp;src=rushil735%40gmail.com&amp;color=%23102372&amp;ctz=America%2FLos_Angeles")
    output$frame <- renderUI({
    my_test <- tags$iframe( src = test, width='100%', height=600)
    print(my_test)
    my_test
    })
    
    #plant health
    minuteUpdate <- reactiveTimer(60000)
    output$Plants <- renderValueBox({
        minuteUpdate()
        valueBox(
            moist_text, moist_reading, icon = icon('tint'), color = moist_color
        )
    })
    
    #xkcd
    output$image <- renderUI({
        tags$img(src='https://imgs.xkcd.com/comics/ten_thousand.png')
    })
    #handles exceptions for stock, except if ticker is not acceptable  
    dataInput <- reactive({ 
        validate(
            need(input$symb != "" || input$symb ,'Enter a valid ticker symbol')
        )
        validate(
            need(input$dates[1] < Sys.Date() || input$dates[2] < Sys.Date(), "Date is in the future")
        )
        validate(
            need(input$dates[2] > input$dates[1], "End date is earlier than start date")
        )
        validate(
            need(difftime(input$dates[2], input$dates[1], "days") > 4, "Dates must be 5 trading days apart")
        )
    #lookup
    getSymbols(input$symb, src = "google", 
        from = input$dates[1],
        to = input$dates[2],
        auto.assign = FALSE)
    })
    #whether price is adjusted
    finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
    })
    #actual plot
    output$stockplot <- renderPlot({
        barChart(finalInput(), theme = chartTheme(stockBackground), name = input$symb)
    })
    #last price
    output$text<-renderText({
        paste("Last Price: ", getQuote(input$symb)$Last, "\n","   Volume: ", getQuote(input$symb)$Volume)
    })
    #clock to update the rotating graph every hour
    hourUpdate <- reactiveTimer(3600000)
    #moving clock, made of 3 sections, 0/24-sunrise, sunrise-sunset,sunset-24/0
    output$timeplot <- renderPlot({
        hourUpdate()
        ggplot()+
            geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = sunlight), fill = 'midnightblue', color = 'midnightblue')+
            geom_rect(aes(xmin = 0, xmax = 1, ymin = sunlight, ymax = moonlight), fill = 'gold', color = 'gold')+
            geom_rect(aes(xmin = 0, xmax = 1, ymin = moonlight, ymax = 24), fill = 'midnightblue', color = 'midnightblue')+
            geom_segment(aes(x = 0, y = currTime, yend = currTime, xend = 1), color = 'green')+
            coord_polar(theta = 'y', start = 2 * pi - currTime/12 * pi)+
            ggtitle(paste0("Sunrise: ", dat$results$channel$astronomy[[1]], " Sunset: ", dat$results$channel$astronomy[[2]]))+
            scale_y_continuous(breaks = seq(0,24,6))+
            theme(
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                line = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(
                    family = 'Trebuchet MS',
                    size = 32,
                    hjust = .5
                )
            )
    })
    
    #moisture plot
    output$plantPlot <- renderPlot({
        minuteUpdate()
        #set up moisture
        moisture <- read.csv('~/Desktop/test/codes/Home_mon/Moisture-Sensor-master/data_log.csv')
        moisture.new <- moisture[seq(1, nrow(moisture), 5),]
        ggplot(moisture.new, aes(x = Time, y = Reading, group = Bed, color = factor(Bed)))+
        geom_line() +
        geom_hline(yintercept = 768, color = 'red', linetype = 'dashed') +
        labs(color = 'Pi Number') +
        scale_color_manual(values = gPalette) +
        theme_bw() +
        theme(
            axis.text = element_text(
                hjust = 1,
                angle = 45
            )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
