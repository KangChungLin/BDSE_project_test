library(shiny)
library(shinydashboard)
library(DBI)
library(RMariaDB)
library(pool)
library(readr)
library(wordcloud2)
library(ggplot2)

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}
drv <- dbDriver("MariaDB")
#con <- dbConnect(drv, username="root", password="", dbname ="stock", host="localhost")
con <- dbPool(drv, username="root", password="", dbname ="stock", host="localhost")
df <- read_csv("C:/Users/Kang/Documents/websiteByMonth.csv")


#ui <- fluidPage(
#  selectInput("y", 
#              label = "年份",
#              choices = c("2010", "2011",'2012','2013','2014','2015','2016','2017','2018',
#                          "2019", "2020"), 
#              selected = "2020"),
#  selectInput("m", 
#              label = "月份",
#              choices = c("1", "2",'3','4','5','6','7','8','9','10',"11", "12"), 
#              selected = "7"),
#  wordcloud2Output("wordcloud2"),
#  wordcloud2Output("wc2"),
#  plotOutput("bar")
#)

ui <- dashboardPage(
  dashboardHeader(title = "股票網站輿情分析"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )),
  dashboardBody(
    fluidRow(
      # A static valueBox
      valueBox(865471, "總文章數", icon = icon("list"),width=2),
      
      # Dynamic valueBoxes
      valueBoxOutput("monthBox",width=2),
      
      valueBoxOutput("newsBox",width=2),
      valueBoxOutput("platformBox",width=2),
      valueBoxOutput("blogBox",width=2)
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(tags$b("股票聲量文字雲"),
          wordcloud2Output("wordcloud2",height = 340),
          tags$b("熱詞文字雲"),
          wordcloud2Output("wc2",height = 340)),
      
      box(
        selectInput("y", 
                    label = "年份",
                    choices = c("2010", "2011",'2012','2013','2014','2015','2016','2017','2018',
                    "2019", "2020"), 
                    selected = "2020"),
        selectInput("m", 
                    label = "月份",
                    choices = c("1", "2",'3','4','5','6','7','8','9','10',"11", "12"), 
                    selected = "7")
      ),
      box(tags$b("十大熱門股票聲量"),
          plotOutput("bar"))
    )
  )
)

server <- function(input, output, session) {
  output$monthBox <- renderValueBox({
    num <- sum(df[df$year==as.numeric(input$y)&df$month==as.numeric(input$m),4])
    valueBox(
      as.character(num), "本月文章數", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$newsBox <- renderValueBox({
    total <- sum(df[df$year==as.numeric(input$y)&df$month==as.numeric(input$m),4])
    num <- df[df$year==as.numeric(input$y)&df$month==as.numeric(input$m)&df$Website=='news',4]
    valueBox(
      paste0(round(num/total,3)*100,'%'), "新聞佔比", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$platformBox <- renderValueBox({
    total <- sum(df[df$year==as.numeric(input$y)&df$month==as.numeric(input$m),4])
    num <- df[df$year==as.numeric(input$y)&df$month==as.numeric(input$m)&df$Website=='platform',4]
    valueBox(
      paste0(round(num/total,3)*100,'%'), "論壇佔比", icon = icon("list"),
      color = "green"
    )
  })
  
  output$blogBox <- renderValueBox({
    total <- sum(df[df$year==as.numeric(input$y)&df$month==as.numeric(input$m),4])
    num <- df[df$year==as.numeric(input$y)&df$month==as.numeric(input$m)&df$Website=='blog',4]
    valueBox(
      paste0(round(num/total,3)*100,'%'), "部落格佔比", icon = icon("list"),
      color = "red"
    )
  })
  output$wordcloud2 <- renderWordcloud2({
    res <- dbGetQuery(con, paste0("SELECT * FROM stock", input$y,input$m, ";"))
    #temp <- dbFetch(res)
    #dbClearResult(res)
    # wordcloud2(demoFreqC, size=input$size)
    #wordcloud2(res,color = "random-light", backgroundColor = "grey")
    wordcloud2a(res,color = "random-light")
  })
  output$wc2 <- renderWordcloud2({
    res2 <- dbGetQuery(con, paste0("SELECT * FROM words", input$y,input$m, ";"))
    # wordcloud2(demoFreqC, size=input$size)
    wordcloud2a(res2,color = "random-light", backgroundColor = "grey")
    #wordcloud2(res2,color = "random-light")
    #wordcloud2a(word20207,color = "random-light",backgroundColor = "grey")
  })
  output$bar <- renderPlot({
    res3 <- dbGetQuery(con, paste0("SELECT * FROM stock", input$y,input$m, ";"))
    df <- res3[order(res3$Counts,decreasing = T),]
    df <- df[1:10,]
    ggplot(df,aes(x = reorder(Name, -Counts),y = Counts) ) +
      geom_bar(stat = "identity", colour = gray(0.5), fill = gray(0.5)) +
      #labs(x = '股票名稱', y = '網路聲量') +
      labs(x = '', y = '') +
      theme(axis.title.x = element_text(size=14,face = "bold"),
            axis.text.x = element_text(size=12,face = "bold"),
            axis.title.y = element_text(size=14,face = "bold"),
            axis.text.y = element_text(size=12,face = "bold"))
  })
}

shinyApp(ui,server)