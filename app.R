library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

# Read dataset

movies <- read.csv('MoviesDataset.csv', numerals = c("no.loss"), stringsAsFactors = F,header=T)
head(movies)


# Remove NAs

sum(is.na(movies))
colSums(is.na(movies))
movies.clean <- na.omit(movies)
nrow(movies.clean)

# Dashboard header carrying the title of the dashboard

header <- dashboardHeader(title = "Movies Explorer")  

# Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
)

frow2 <- fluidRow(
  
  box(
    title = "Screens per Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("screens.per.year", height = "300px")
  )
  
  ,box(
    title = "Views per year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("views.per.year", height = "300px")
  ) 
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

# completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  # some data manipulation to derive the values of KPI boxes

  screens.by.year <- movies.clean %>% group_by(Year) %>% summarise(value = sum(Screens)) %>% filter(value==max(value))
  views.per.year <- movies.clean %>% group_by(Year) %>% summarise(value = sum(Views)) %>% filter(value==max(value))
  
  
  # creating the valueBoxOutput content
 
  output$value1 <- renderValueBox({
    
    valueBox(
      "..."
      ,paste('Top year (by Screens):', screens.by.year)
      ,icon = icon("menu-movies",lib='glyphicon')
      ,color = "blue")
    
  })
  
  
  
  output$value2 <- renderValueBox({

    valueBox(
      "..."
      ,paste('Top Year (by Views):',views.per.year)
      ,icon = icon("menu-movies",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  # creating the plotOutput content
  
  output$screens.per.year <- renderPlot({
    ggplot(data = movies.clean, 
           aes(x=Year, y=Screens, fill=factor(Year))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Screens") + 
      xlab("Year") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Screens by Year") + labs(fill = "Year")
  })
  
  
  output$views.per.year <- renderPlot({
    ggplot(data = movies.clean, 
           aes(x=Year, y=Views)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Views") + 
      xlab("Year") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=20, face="bold")) + 
      ggtitle("Views by Year") + labs(fill = "Year")
  })
  
}


shinyApp(ui, server)


