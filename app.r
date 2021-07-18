#ARUSHI TEJPAL 
# 28130006
# TOPIC: Data Breaches in United States of America. 
# TUTOR: Farah ; Friday 4pm. 
# Finished code : 14/11/2020 


# TO BEGIN: PLEASE UNHASH Install.packages FOR LIBRARIES YOU DO NOT HAVE!!!!

library(shiny)
#install.packages("shiny")
library(leaflet)
#install.packages("leaflet")
library(dplyr)
#install.packages("dplyr")

#install.packages("htmltools")
library(htmltools)
#library(maps)
#install.packages("maps")

library(ggplot2)
#install.packages("ggplot2")

library(reshape2)
#install.packages("reshape2")
library(viridis)
#install.packages("viridis")
library(hrbrthemes)
#install.packages("hrbrthemes")


#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("collapsibleTree")
library(collapsibleTree)
#install.packages("highcharter")
library(highcharter) 
#install.packages("shinyalert")
library(shinyalert)
library(tidyverse)
#install.packages("tidyverse")


####### CREATING MAPS 

f<- read.csv("ff2.csv") #has the IA
L<-read.csv("newloc.csv") #has the correct lat long

colnames(L)[3]<- "name" # city name 

f[order(f$name),]
L<-L[order(L$name),]
L$name<-tolower(L$name)
f$name<-tolower(f$name)
f$X<-NULL
f$X.1<-NULL
f$Lat <-NULL 
f$long <-NULL


#Merging data 
n<-merge(x= L, y=f, by="name", all.x= TRUE) # merging to get Individuals affected, long lat and name 


#using maps library 
#mapStates <- map( "state", fill = TRUE, plot = FALSE)

# split the string with : as seperator
#spliteNames <- strsplit(mapStates$names, ":") 

# get first part of the origin string;
# e.g. get washington from washington:san juan island
#firstPartNames <- lapply(spliteNames, function(x) x[1]) 
#for df 

# matching names of maps and df 
#total <- n$IA[match(firstPartNames, n$name)]


# Using html tool library to create hover lables 
labels <- sprintf(
  "<strong>%s</strong><br/>%g breaches </sup>",
  n$name, n$IA
) %>% lapply(htmltools::HTML)


#mapbreach <- colorQuantile("Blues", total) # prepare the color mapping


######### COLLAPSIBLE code 

B<-read.csv("Breach_clean copy2.csv")
T <-B %>% 
  group_by(Type.of.Breach, Covered.Entity.Type, Year) %>% 
  summarise(Records_Lost=median(Individuals.Affected)) 




#### LINE GRAPH 


B<-read.csv("Breach_clean copy2.csv")

agr<- aggregate(Individuals.Affected~Year, B, median)
agr

####### PIE CHART 

U<-B%>% group_by(Type.of.Breach) %>%
  summarise(avg= median(Individuals.Affected)) # aggregating by median. 

percentage_avg <- round(100 * U$avg/sum(U$avg), 1) # getting percentage 
U$perc<- percentage_avg # creating new column 





##########UI FUNCTION #####################
ui <- dashboardPage(
 
  
  header<- dashboardHeader(title="DATA BREACHES",titleWidth = 200),
  sidebar<- dashboardSidebar(width = 350,
                   sidebarMenu(
                     menuItem("Breaches map ",tabName = "Tab1",icon=icon("map")),#creating side menu for dashboard
                     menuItem("collapsible Tree",tabName = "Tab2",icon = icon("bar-chart")),
                     menuItem("Breaches over time",tabName = "Tab3",icon = icon("line-chart")),
                     menuItem("pie chart",tabName = "Tab4",icon = icon("pie-chart"))
                   )
                     ), 
  body<-dashboardBody(
    ## TAB1 
    tabItems( tabItem(tabName = "Tab1",   useShinyalert(),
             fluidRow(box(width = 500, title = "Breaches map per state", status = "success",background = "purple", solidHeader = TRUE, collapsible = TRUE,
                box(leafletOutput("mymap2", height = 500, width = 500))) ))),
  ##TAB2
    tabItems(tabItem( tabName="Tab2",
                      fluidRow(box(width = 700, collapsible = TRUE,title = "Over View of Data Breaches", # background = "olive", solidHeader = TRUE, #collapsible = TRUE,
                               box(collapsibleTreeOutput("tree", height= 600, width = 700)) )))),
    #TAB 3 
    tabItems(
      tabItem( tabName="Tab3",
        fluidRow(box(width = 500, title = "Line graph", solidHeader = TRUE, collapsible = TRUE, status= "success", background = "light-blue",
                 box(plotOutput("plot", "click", height= 400, width = 500)))),
                  fluidRow(box( width= 400, title = "line-graph Table", solidHeader = TRUE, 
                                box(tableOutput("data")))))),

  
### TAB 4 
tabItems(
  tabItem( tabName="Tab4",
           fluidRow(box(width = 500, title = "percentage of Data Breaches Cause", solidHeader = TRUE, collapsible = TRUE, background = "purple", status = "success",
                        box(highchartOutput("hc3", height= 800, width = 500)),
                        )))),
  

## Tags The font of the header 
tags$head(tags$link(rel= "stylesheet", type = "text/css", href= "custom.css")),

fluidRow(box(width = 70, height= 70, background = "black", 
             title = "Welcome to Data Breaches Visualisation", "Click sidebar icon to display graphs" ))

 ),

 skin = "purple"
 
)
  
  
  

####### SERVER FUNCTION ###############
server <- function(input, output, session) {
  # ALert! 
shinyalert("Welcome to Data Breaches visualisation", "In this visualisation we will have multiple graphs shown on the sidebar, click the minimise button on each header of the graph to see the whole graph", confirmButtonText = "Start!" )
  
  output$mymap2 <- renderLeaflet({ # create leaflet map
    leaflet(data = n) %>%
      addTiles() %>%
      #setView(n$Longitude, n$Latitude, zoom =10) %>%
      addCircleMarkers(n$Longitude,
                     n$Latitude,
                       radius= ~sqrt(n$IA)/200,
                       label = labels,
                       popup = ~IA,
                       fillColor = "purple", weight = 4,
                       layerId = ~n$name)
  })
  
    
   

  #COLLAPSIBLE TREE 
  output$tree <- renderCollapsibleTree({ 
    collapsibleTreeSummary( T,
                            hierarchy = c("Covered.Entity.Type","Type.of.Breach", "Year"), # CREATING HIERATCHY 
                            root= "Total", 
                            width = 500, 
                            attribute =  "Records_Lost",
                            nodeSize = "leafCount",
                            zoomable= FALSE
    )
    })
  
  # LINE GRAPH 
  output$plot <- renderPlot({
    ggplot(data = agr, aes(x= Year, y = Individuals.Affected))+
      geom_point( color= "black", size = 2) +
      geom_line( color = "blue") + theme(panel.background = element_rect(fill="lightblue", color= "red"))
    
  })
  # TABLE OUTPUT FOR LINE GRAPH 
  output$data <- renderTable({ 
    nearPoints(agr, input$click, xvar = "Year", yvar = "Individuals.Affected")
  })
  
  #PIE CHART 
  output$hc3<- renderHighchart({  U %>% 
    hchart("pie", hcaes(x = Type.of.Breach, y = perc), name = "Data breaches cause")
  
  })
 
}

#run application
shinyApp(ui, server)

