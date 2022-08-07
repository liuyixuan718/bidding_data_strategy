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
library(readr)
library(tidyverse)
library(ggplot2)
library(forcats)


df <- read_csv('cleaned2.csv')

##revenue
df <- df%>%
  mutate(catapultx_revenue = remote_feed_gross_revenue - publisher_estimated_revenue)

df1 <- df %>%
  select(remote_feed_name, remote_feed_coverage_percent)%>%
  group_by(remote_feed_name)%>%
  summarise(mean_coverage_percent=mean(remote_feed_coverage_percent, na.rm=TRUE))%>%
  arrange(desc(mean_coverage_percent))%>%
  mutate(remote_feed_name=factor(remote_feed_name, levels=remote_feed_name))

df <- df%>%
  mutate(month_c = format(df$date,"%m"))






header <- dashboardHeader(title = "CatapultX Analysis")

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Optimization Dashboard", tabName = "dashboard_2", icon = icon("dashboard")),
    menuItem("Coverage % by Feed Name", tabName = "dashboard_3", icon = icon("dashboard")),
    menuItem("Revenue Time-serise", tabName = "dashboard_6", icon = icon("dashboard"))
    )
  )


body <- dashboardBody(
  tabItems(
    tabItem("dashboard_2",
            titlePanel("Optimization Dashboard"),
            sidebarLayout(
              sidebarPanel(
                varSelectInput("groupbyvalue",
                               "Group by Value:",
                               df%>% select(hour_of_day_1,day_of_month,day_of_week,month),
                               selected = 'hour_of_day_1')
              ),
              mainPanel(
                plotOutput("line_chart4"),
                plotOutput("line_chart2"),
                plotOutput("line_chart1")
              )
            )
            
    ),
    tabItem("dashboard_3",
            titlePanel("Coverage % by Feed Name"),
            sidebarLayout(
              sidebarPanel(
                numericInput("min_num","Min Coverage Percentage",0),
                numericInput("max_num","Max Coverage Percentage",10)
              ),
              mainPanel(
                plotOutput("distPlot")
              )
            )
           
    ),
    
    tabItem("dashboard_6",
            titlePanel("Revenue Time-serise"),
            sidebarLayout(
              sidebarPanel(
                varSelectInput("groupbyvalue1",
                               "Group By Value:",
                               df%>% select(hour_of_day_1,day_of_month,day_of_week,month),
                               selected = 'hour_of_day_1'),
                varSelectInput("groupbyvalue2",
                               "Group By Value:",
                               df%>% select(month_c,remote_feed_name,publisher_name),
                               selected = 'month_c')
              ),
              mainPanel(
                plotOutput("line_chart3"),
                plotOutput("pie_chart1")
              )
            )
            
    )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    output$distPlot <- renderPlot({
      df1 <- df1%>%
        filter(mean_coverage_percent <= input$max_num)%>%
        filter(mean_coverage_percent >= input$min_num)
      
      x<-df1$remote_feed_name
      y<-df1$mean_coverage_percent
      
      ggplot(df1, aes(x, y))+
        geom_bar(stat = "identity")+
        theme(axis.text.x = element_text(angle = 90),
              plot.title = element_text(hjust = 0.5))+
        labs(x="Remote Feed Name",y="Average Coverage Percentage",title="Average Coverage Percentage by Advertiser")+
        theme(plot.title = element_text(face = "bold"))+
        theme(plot.title=element_text(hjust=0.5))
        
    })
    output$line_chart1 <- renderPlot({
      # data_manipulation 
      df2<- df%>%
        select(hour_of_day_1,day_of_month, day_of_week, avg_price,win_prob, 
               month,remote_feed_coverage_percent)%>%
        group_by(!!input$groupbyvalue)%>%
        summarise(mean_win_prob=mean(win_prob, na.rm=TRUE),
                  mean_avg_price=mean(avg_price, na.rm=TRUE),
                  mean_coverage_percent=mean(remote_feed_coverage_percent, na.rm=TRUE))
      
      # draw the line chart1: Average Win Rate vs Average Price
      ggplot(data=df2)+
        geom_line(aes(x=!!input$groupbyvalue, y=mean_win_prob, color='Average Win Rate'), size = 1)+
        geom_line(aes(x=!!input$groupbyvalue, y=mean_avg_price*400, color="Average Price"), size = 1)+
        scale_y_continuous(n.breaks = 9,
                           name="Average Win Rate",
                           sec.axis=sec_axis(trans = ~ ./400, name="Average Price"))+
        theme(axis.text.x = element_text(angle=60, hjust = 1))+
        scale_color_manual(name='Line', values=c('Average Win Rate'='red',"Average Price"='blue'))+
        labs(title="Probability of Winning and Bid Price")+
        theme(plot.title = element_text(face = "bold"))+
        theme(plot.title=element_text(hjust=0.5))
      
      
    })
    output$pie_chart1 <- renderPlot({
      df3<-aggregate(publisher_estimated_revenue ~ publisher_name, data = df, sum )
      
      ggplot(df3,aes(x="",y=publisher_estimated_revenue,fill=publisher_name))+
        geom_bar(stat="identity",color="black")+
        geom_text(aes(label = paste0(round(publisher_estimated_revenue/sum(publisher_estimated_revenue)*100), "%")), position = position_stack(vjust = 0.5))+
        coord_polar("y")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5))+
        labs(x="Country",y="Count",title="Revenue by Publisher")+
        theme(plot.title = element_text(face = "bold"))+
        theme(plot.title=element_text(hjust=0.5))
      
      
    })
    output$line_chart2 <- renderPlot({
      df_plott<- df%>%
        group_by(!!input$groupbyvalue)%>%
        summarise(mean_coverage_percent=mean(remote_feed_coverage_percent, na.rm=TRUE))
      
      ggplot(data=df_plott, aes(x= !!input$groupbyvalue, y=mean_coverage_percent))+
        geom_line(size = 1)+
        scale_y_continuous(n.breaks=10)+
        theme(axis.text.x = element_text(angle=60, hjust = 1))+
        geom_point()+
        labs(y='Average Coverage %', title = "Coverage Percentage")+
        theme(plot.title = element_text(face = "bold"))+
        theme(plot.title=element_text(hjust=0.5))
      
      
      
    })
    output$line_chart3 <- renderPlot({
      df_plot3<- df%>%
        group_by(!!input$groupbyvalue1,!!input$groupbyvalue2)%>%
        summarise(c_revenue=sum(catapultx_revenue, na.rm=TRUE))
      
      ggplot(df_plot3,aes(x=!!input$groupbyvalue1, y= c_revenue,color = !!input$groupbyvalue2))+
        geom_line(size = 1)+
        geom_point()+
        theme(plot.title = element_text(face = "bold"))+
        theme(plot.title=element_text(hjust=0.5))+
        labs(y="Total Revenue",title="Revenue Time-serise")
      
    })
    output$line_chart4 <- renderPlot({
      df_plot4<- df%>%
        group_by(!!input$groupbyvalue)%>%
        summarise(c_revenue=sum(catapultx_revenue, na.rm=TRUE))
      
      ggplot(df_plot4,aes(x=!!input$groupbyvalue, y= c_revenue))+
        geom_line(size = 1)+
        geom_point()+
        labs(y="Total Revenue",title="Revenue")+
        theme(plot.title = element_text(face = "bold"))+
        theme(plot.title=element_text(hjust=0.5))
      
    })
  }
)

























