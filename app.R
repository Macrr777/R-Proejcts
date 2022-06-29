#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here

df <- read.csv("C:/Users/choud/Desktop/DJ/R Assignment Amir/Shinny App with data/covid_19_india.csv")
df$Date <- as.Date(df$Date,format = "%d/%m/%y")
library(shiny)
library(markdown)
library(dplyr)
# Define UI for application that draws a histogram
ui <- fluidPage(

  navbarPage("Navbar",
             navbarMenu("Home",
                        tabPanel("About",
                                 headerPanel(strong("Covid-19 Tracker Application")),
                                 fluidRow(
                                   column(6,
                                          h4(p(
                                            "This Rshiny Application is used to check the",
                                            "spread of Covid-19 throughout the nation through predictive analytics.",
                                            "Check it out for acccurate prediction",
                                            "& be safe."
                                          ))
                                   ),
                                   column(3,
                                          img(src='myImage.jpg',align = "bottom"
                                              
                                          )
                                   )
                                 )
                                 
                        ),
                        tabPanel("Table",
                                 DT::dataTableOutput("table")
                        ),
                        tabPanel("Statewise data",
                                 headerPanel("Enter the Date & select a State"),
                                 sidebarPanel(
                                   dateInput("dt1","Date: ",
                                             min = "2020-01-01",
                                             max = "2020-05-26"),
                                   selectInput("state", "STATE:",choices = unique(df$State.UnionTerritory)),
                                 ),
                                 mainPanel(
                                   h3("Confirmed Case :",textOutput("oconf"),style="color:orange"),
                                   h3("Cured Case :",textOutput("ocur"),style="color:blue"),
                                   h3("Death Case :",textOutput("odet"),style="color:red")
                                 )
                                 
                        )
                        
             ),
             tabPanel("EDA",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("plotType", "Plot type",
                                       c("Barplot"="b", "Line"="l")
                          )
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
                      
             ),
             tabPanel("Predictive Analysis",
                      pageWithSidebar(
                        headerPanel("Case's Increased Estimation"),
                        sidebarPanel(
                          sliderInput('sl1','No of Days',value = 5,min=0,max=10,step=1)
                        ),
                        mainPanel(
                          plotOutput('plot1')
                        )
                        
                      )
             )
             
  )
  
)

library(forecast)
attach(df)
library(ggplot2)
library(dplyr)
library(tidyr)
server <- function(input, output, session) {
  
  output$oconf <- renderText({
    d<-df[df$Date==input$dt1 & df$State.UnionTerritory==input$state,]
    paste(d$Confirmed)
    
  })
  
  output$mydates<-renderText({
    input$date1})
  
  output$ocur <- renderText({
    d<-df[df$Date==input$dt1 & df$State.UnionTerritory==input$state,]
    paste(d$Cured)
  })
  
  output$odet <- renderText({
    d<-df[df$Date==input$dt1 & df$State.UnionTerritory==input$state,]
    paste(d$Deaths)
  })
  
  output$plot <- renderPlot({
    if (input$plotType=='b'){
      barplot(df$Confirmed, 
              main = "States&Ut Vs Confirmed Cases",
              xlab = "State.UnionTerritory",
              ylab = "Confirmed",
              names = df$State.UnionTerritory,
              col = "chocolate",
              border = "red")
      
    } else {
      info_cov_india1<-arrange(df,Date)%>%group_by(Date)%>% 
        summarize(cured=sum(Cured),deaths=sum(Deaths),case=sum(Confirmed))
      info_cov_india1
      
      JJ<-ggplot(info_cov_india1,aes(x=Date))+
        geom_line(aes(y=case,color="Cases"), size=1.5)+ 
        geom_line(aes(y=deaths,color="Death"), size=1.5)+ 
        geom_line(aes(y=cured,color="Recovered"), size=1.5)+
        theme_bw()+ylab("Total Count")+xlab("Period")+ 
        labs(title="Cumulative Count of Covid19 cases, Recovered and Deaths",color = "Legend")+
        scale_color_manual(values = c("orange","red","blue"))
      
      JJ
      
    }
  })
  
  
  
  
  output$table <- DT::renderDataTable(
    df
  )
  
  output$plot1<-renderPlot({
    x<-input$sl1
    x
    fit <- auto.arima(df$Confirmed,)
    forecastedValues <- forecast(fit, x)
    plot(forecastedValues, main = "Graph with forecasting", 
         col.main = "darkgreen")
  }) 
}
# Run the application 
shinyApp(ui = ui, server = server)
