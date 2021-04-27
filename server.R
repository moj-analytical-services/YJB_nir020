library(RColorBrewer)
library(shinydashboard)
library(ggplot2)
library(shiny)
library(s3tools)

myPalette <- brewer.pal(5, "Set1") 
Children <-s3tools::s3_path_to_full_df("alpha-yjb-stats/Children.csv")

server<-function(input,output) {
  
  output$value1 <- renderValueBox({
    
    valueBox(
      formatC(input$YOTs,format ="d", big.mark=',')
      ,paste('Financial year:',input$Year)
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "yellow")
    
  })  
  output$value2 <- renderValueBox({
    
    Children_Filter<-subset(Children, Financial_Year== input$Year,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Children_Filter<-subset(Children_Filter, YOT == input$YOTs,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Number_YP <- sum(Children_Filter$Number_Children)
    valueBox(
      formatC(Number_YP, format="d", big.mark=',')
      ,paste('Number of children cautioned or sentenced')
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "teal")
    
  })
  
  output$value3 <- renderValueBox({
    
    YOT_Pop <-s3tools::s3_path_to_full_df("alpha-yjb-stats/YOT_Pop.csv")
    Pop <- subset(YOT_Pop, YOT==input$YOTs, select = c("YOT","X2019"))
    valueBox(
      formatC(Pop$`X2019`, format="d", big.mark=',')
      ,paste('2019 Local 10-17 population')
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "purple")
  })
  
  output$value4 <- renderValueBox({
    
    YOT_Pop <- YOT_Pop <-s3tools::s3_path_to_full_df("alpha-yjb-stats/YOT_Pop.csv")
    Pop <- subset(YOT_Pop, YOT==input$YOTs, select = c("YOT","X2019"))
    Children_Filter<-subset(Children, Financial_Year== input$Year,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Children_Filter<-subset(Children_Filter, YOT == input$YOTs,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Number_YP <- sum(Children_Filter$Number_Children)
    
    valueBox(
      formatC((Number_YP/Pop$`X2019`*10000), format="d", big.mark=',')
      ,paste('Rate of children cautioned or sentenced')
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "green")
    
  })
  
  output$plot1 <- renderPlot({
    Children_Filter<-subset(Children, Financial_Year== input$Year,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Children_Filter<-subset(Children_Filter, YOT == input$YOTs,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    GenCounts<-aggregate(Children_Filter$Number_Children, by=list(Category=Children_Filter$Sex), FUN=sum)
    ggplot(data = GenCounts) + 
      geom_bar(mapping = aes(x = Category,y=x, fill = Category),stat="identity")+
      scale_fill_hue(c = 40) +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(face = "bold",size = 8, angle = 0),
            axis.text.y = element_text(face = "bold",size = 8, angle = 0))+
      labs(x = "Sex",y="Number of children")
  })
  
  output$plot2 <- renderPlot({
    Children_Filter<-subset(Children, Financial_Year== input$Year,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Children_Filter<-subset(Children_Filter, YOT == input$YOTs,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Number_YP <- sum(Children_Filter$Number_Children)
    EthCounts<-aggregate(Children_Filter$Number_Children, by=list(Category=Children_Filter$Ethnicity_Group), FUN=sum)
    
    ggplot(data = EthCounts) + 
      geom_bar(mapping = aes(x = Category,y=x, fill = Category),stat="identity")+
      scale_fill_hue(c = 40) +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(face = "bold",size = 8, angle = 0),
            axis.text.y = element_text(face = "bold",size = 8, angle = 0))+
      labs(x = "Ethnicity",y="Number of children")
  })
  
  output$plot3 <- renderPlot({
    Children_Filter<-subset(Children, Financial_Year== input$Year,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Children_Filter<-subset(Children_Filter, YOT == input$YOTs,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    Number_YP <- sum(Children_Filter$Number_Children)
    AgeCounts<-aggregate(Children_Filter$Number_Children, by=list(Category=Children_Filter$Age_Group), FUN=sum)
    
    ggplot(data = AgeCounts) + 
      geom_bar(mapping = aes(x = Category,y=x, fill = Category),stat="identity")+
      scale_fill_hue(c = 40) +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(face = "bold",size = 8, angle = 0),
            axis.text.y = element_text(face = "bold",size = 8, angle = 0))+
      labs(x = "Age",y="Number of children")
  })
  
  output$plot4 <- renderPlot({
    Children_Filter<-subset(Children, YOT == input$YOTs,select = c("Financial_Year","YOT","Age_Group","Ethnicity_Group","Sex","Number_Children"))
    YearCount<-aggregate(Children_Filter$Number_Children, by=list(Category=Children_Filter$Financial_Year), FUN=sum)
    
    ggplot(data= YearCount, aes(x=Category,y=x,group=1)) + geom_line()+geom_point() +
      labs(x = "Year", y = "Number of children")
    
  })
}
