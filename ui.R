library(RColorBrewer)
library(shinydashboard)
library(ggplot2)
library(shiny)
library(s3tools)


Children <-s3tools::s3_path_to_full_df("alpha-yjb-stats/Children.csv")

ui <- fluidPage(
  max(Children$YOT)

)
