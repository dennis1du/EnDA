### load packages
library(shiny)
library(ggplot2)
library(grid)
library(jpeg)
library(png)
library(RCurl)
library(DT)
library(scales)

### preprocessing
# read data
dir <- getwd()
shotData <- read.csv(paste(dir,"/Shotlog_GSW.csv",sep=''))
name <- unique(as.character(shotData$PLAYER_NAME))

# static image for court
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

### UI
ui <- fluidPage(
  
  headerPanel("Golden States Warriors | 2016-17 Regular Season", windowTitle = "DU_JIANWEI_FINAL"),

# configuration for inputs  
  sidebarPanel(
    selectInput("playerName", "Player", choices = name, selected = "Stephen Curry"),
    br(),
    dateRangeInput("date", "Date", start = "2016-10-26", end = "2017-04-12", startview = "month"),
    br(),
    br(),
    plotOutput("Profile")
  ),
  
# configuration for outputs
  mainPanel(
    tabsetPanel(
      tabPanel("Shot Chart", 
               plotOutput("ShotChart", height = "600px")
               ),
      tabPanel("Stats",
               h3("Summary"),
               verbatimTextOutput("Summary"),
               br(),
               h3("Hit Rate"),
               DT::dataTableOutput("Hitrate")
               ),
      tabPanel("Analysis",
               h3("Shot Type"),
               fluidRow(
                 column(5,
                        plotOutput("a1_1")
                 ),
                 column(5,
                        DT::dataTableOutput("a1_2"),
                        offset = 1
                 )),
               h3("Shot Range"),
               fluidRow(
                 column(5,
                        plotOutput("a2_1")
                 ),
                 column(5,
                        DT::dataTableOutput("a2_2"),
                        offset = 1
                        ))
               ))
))


### Server
server <- function(input, output) {

# read reactive data
  data <- reactive({subset(shotData, PLAYER_NAME == input$playerName & as.Date(GAME_DATE) >= as.Date(input$date[1]) & as.Date(GAME_DATE) <= as.Date(input$date[2]))})

# output1: Player Profile  
  output$Profile <- renderPlot({
    id <- data()$PLAYER_ID[1]
    playerImg.URL <- paste("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/1610612744/2016/260x190/",id,".png", sep="")
    playerImg <- rasterGrob(readPNG(getURLContent(playerImg.URL)), height=unit(0.8, "npc"))
    ggplot() +
      annotation_custom(playerImg) +
      theme(panel.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
  })

# output2: plot 1 for Shot Chart
  output$ShotChart <- renderPlot({
    ggplot(data(), aes(x=LOC_X, y=LOC_Y)) +
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
      scale_shape_manual(values = c(16,88)) +
      xlim(250, -250) +
      ylim(-50, 420) +
      coord_fixed() +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank())
  })

# output3: Summary for shooting stats
  output$Summary <- renderPrint({
    summary(data()[,c(4,5,6,7,8,9)])
  })

# output4: table 1 for Hit Rate
  output$Hitrate <- DT::renderDataTable({
    FA <- sum(data()$SHOT_ATTEMPTED_FLAG)
    FM <- sum(data()$SHOT_MADE_FLAG)
    FA2 <- sum(data()[data()$SHOT_TYPE == "2PT","SHOT_ATTEMPTED_FLAG"])
    FM2 <- sum(data()[data()$SHOT_TYPE == "2PT","SHOT_MADE_FLAG"])
    FA3 <- FA - FA2
    FM3 <- FM - FM2
    PTs <- FM2*2 + FM3*3
    PTA <- PTs/FA
    P2A <- FM2*2/FA2
    P3A <- FM3*3/FA3
    FG <- FM/FA
    F2 <- FM2/FA2
    F3 <- FM3/FA3
    dt1 <- data.frame(PTs, round(PTA,2), round(P2A,2), round(P3A,2), percent(FG), percent(F2), percent(F3))
    names(dt1) <- c("PT", "PT/Attmpt", "2P/Attmpt", "3P/Attmpt", "FG%", "2P%", "3P%")
    datatable(dt1)
  })
  
# output5: plot 2 for Shot Type
  output$a1_1 <- renderPlot({
    dt2 <- data.frame(table(data()$ACTION_TYPE))
    T1 <- sum(data()[data()$ACTION_TYPE == "Jump Shot","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Jump Shot","SHOT_ATTEMPTED_FLAG"])
    T2 <- sum(data()[data()$ACTION_TYPE == "Layup","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Layup","SHOT_ATTEMPTED_FLAG"])
    T3 <- sum(data()[data()$ACTION_TYPE == "Fadeaway","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Fadeaway","SHOT_ATTEMPTED_FLAG"])
    T4 <- sum(data()[data()$ACTION_TYPE == "Hook Shot","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Hook Shot","SHOT_ATTEMPTED_FLAG"])
    T5 <- sum(data()[data()$ACTION_TYPE == "Bank Shot","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Bank Shot","SHOT_ATTEMPTED_FLAG"])
    T6 <- sum(data()[data()$ACTION_TYPE == "Dunk","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Dunk","SHOT_ATTEMPTED_FLAG"])
    dt2$Var1 <- factor(dt2$Var1, levels=c('Jump Shot','Layup','Fadeaway','Hook Shot', 'Bank Shot', 'Dunk'))
    dt2 <- cbind(dt2[c(5,6,3,4,1,2),],c(percent(T1),percent(T2),percent(T3),percent(T4), percent(T5), percent(T6)))
    names(dt2) <- c("Type", "Attmpt", "FG%")
    rownames(dt2) <- NULL
    ggplot(data = dt2) + 
      geom_bar(aes(Type, Attmpt), stat = 'identity', width = 0.5, fill = 'yellow2')
  })
  
# output6: table 2 for Shot Type
  output$a1_2 <- DT::renderDataTable({
    dt2 <- data.frame(table(data()$ACTION_TYPE))
    T1 <- sum(data()[data()$ACTION_TYPE == "Jump Shot","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Jump Shot","SHOT_ATTEMPTED_FLAG"])
    T2 <- sum(data()[data()$ACTION_TYPE == "Layup","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Layup","SHOT_ATTEMPTED_FLAG"])
    T3 <- sum(data()[data()$ACTION_TYPE == "Fadeaway","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Fadeaway","SHOT_ATTEMPTED_FLAG"])
    T4 <- sum(data()[data()$ACTION_TYPE == "Hook Shot","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Hook Shot","SHOT_ATTEMPTED_FLAG"])
    T5 <- sum(data()[data()$ACTION_TYPE == "Bank Shot","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Bank Shot","SHOT_ATTEMPTED_FLAG"])
    T6 <- sum(data()[data()$ACTION_TYPE == "Dunk","SHOT_MADE_FLAG"])/sum(data()[data()$ACTION_TYPE == "Dunk","SHOT_ATTEMPTED_FLAG"])
    dt2$Var1 <- factor(dt2$Var1, levels=c('Jump Shot','Layup','Fadeaway','Hook Shot', 'Bank Shot', 'Dunk'))
    dt2 <- cbind(dt2[c(5,6,3,4,1,2),],c(percent(T1),percent(T2),percent(T3),percent(T4), percent(T5), percent(T6)))
    names(dt2) <- c("Type", "Attmpt", "FG%")
    rownames(dt2) <- NULL
    datatable(dt2)
  })

# output7: plot 3 for Shot Range  
  output$a2_1 <- renderPlot({
    R1 <- sum(data()[data()$SHOT_ZONE_RANGE == "8- ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "8- ft.","SHOT_ATTEMPTED_FLAG"])
    R2 <- sum(data()[data()$SHOT_ZONE_RANGE == "8-16 ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "8-16 ft.","SHOT_ATTEMPTED_FLAG"])
    R3 <- sum(data()[data()$SHOT_ZONE_RANGE == "16-24 ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "16-24 ft.","SHOT_ATTEMPTED_FLAG"])
    R4 <- sum(data()[data()$SHOT_ZONE_RANGE == "24+ ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "24+ ft.","SHOT_ATTEMPTED_FLAG"])
    dt3 <- data.frame(table(data()$SHOT_ZONE_RANGE))
    dt3 <- cbind(dt3[c(3,4,1,2),],c(percent(R1),percent(R2),percent(R3),percent(R4)))
    names(dt3) <- c("Range", "Attmpt", "FG%")
    rownames(dt3) <- NULL
    dt3$Range <- factor(dt3$Range, levels=c('8- ft.','8-16 ft.','16-24 ft.','24+ ft.'))
    ggplot(data = dt3) + 
      geom_bar(aes(Range, Attmpt), stat = 'identity', width = 0.5, fill = 'light blue')
  })

# output8: table 3 for Shot Range  
  output$a2_2 <- DT::renderDataTable({
    P1 <- sum(data()[data()$SHOT_ZONE_RANGE == "8- ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "8- ft.","SHOT_ATTEMPTED_FLAG"])
    P2 <- sum(data()[data()$SHOT_ZONE_RANGE == "8-16 ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "8-16 ft.","SHOT_ATTEMPTED_FLAG"])
    P3 <- sum(data()[data()$SHOT_ZONE_RANGE == "16-24 ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "16-24 ft.","SHOT_ATTEMPTED_FLAG"])
    P4 <- sum(data()[data()$SHOT_ZONE_RANGE == "24+ ft.","SHOT_MADE_FLAG"])/sum(data()[data()$SHOT_ZONE_RANGE == "24+ ft.","SHOT_ATTEMPTED_FLAG"])
    dt3 <- data.frame(table(data()$SHOT_ZONE_RANGE))
    dt3 <- cbind(dt3[c(3,4,1,2),],c(percent(P1),percent(P2),percent(P3),percent(P4)))
    names(dt3) <- c("Range", "Attmpt", "FG%")
    rownames(dt3) <- NULL
    dt3$Range <- factor(dt3$Range, levels=c('8- ft.','8-16 ft.','16-24 ft.','24+ ft.'))
    datatable(dt3)
  })
}

### run app
shinyApp(ui = ui, server = server)