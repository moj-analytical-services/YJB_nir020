library(shiny)
library(s3tools)
library(shinydashboard)

Children <-s3tools::s3_path_to_full_df("alpha-yjb-shiny/Children.csv")

YOT<-unique(Children$YOT)
Eth<-unique(Children$Ethnicity)
Year<-unique(Children$Financial_Year)

ui <- dashboardPage(
  dashboardHeader(title = "Asset Plus Tool"),
  dashboardSidebar(selectInput("YOTs","Select YOT", choices = YOT),selectInput("Year","Select financial year", choices = Year) ),
  
  dashboardBody(
    
    fluidRow(valueBoxOutput(width=5,"value1")),
    fluidRow(valueBoxOutput(width=3,"value2"),valueBoxOutput(width=3,"value3"),valueBoxOutput(width=3,"value4")),
    fluidRow(box(width=8,title="Trends breakdown",status="primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot4",height ="200px"))),
    fluidRow(box(width=2,title="Gender breakdown",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot1",height ="200px")),
             box(width=5,title="Ethnicity breakdown",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot2",height ="200px")),
             box(width=2,title="Age breakdown",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot3",height ="200px"))
    )
    
  )
)