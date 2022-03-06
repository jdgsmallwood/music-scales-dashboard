##
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets4)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(broom)

# This is a public sheet so there's no need for authentication.
gs4_deauth()

#PROD
data = read_sheet("https://docs.google.com/spreadsheets/d/1sfHyl2iu8YHO276aMogAU5x0_F3fpneVGG-t7MnllYw/edit?usp=sharing", "History")

#TEST
#data = read_sheet("https://docs.google.com/spreadsheets/d/1a9sy8N3lTiJFjB6vYNLNqDIDXxd7x1BcWrGMkxkS2gc/edit?usp=sharing", "History_2")
data$Date <- as.Date(data$Date)
# Chromatic scales are pretty much always the same. So remove all except C
all <- data %>% expand(Tonic, ScaleType, Articulation, Date) %>% 
  filter(!(Tonic != "C" & ScaleType == "Chromatic")) %>%
  filter(!((Tonic != "C" & Tonic != "C#" ) & ScaleType == "WholeTone"))

data = data %>% 
  select(c("Tonic", "ScaleType", "Articulation", "Date", "BestTempo")) %>%
  mutate(LastDate=Date) %>%
  right_join(all) %>% group_by(Tonic, ScaleType, Articulation) %>% 
  arrange(Date, .by_group=TRUE) %>% 
  fill(BestTempo) %>% 
  fill(LastDate) %>%
  replace_na(list(LastDate=min(data$Date), BestTempo=80)) %>%
  ungroup() %>%
  mutate(DaysBetween=as.integer(Date-LastDate)) %>%
  mutate(DecayedTempo=round(pmax(80, BestTempo*(1-DaysBetween/(365 * 3))), 2))

# Get just the data from the last date.
current_data = data %>%
                  filter(Date == max(Date))

# This is a fitted model to see which ones are below expected.
model <- lm(BestTempo ~ 0 +Tonic + Articulation + ScaleType, data=current_data)
aug_model <- augment(model)

# We use pmax as the parallel maximum to get the rowwise max, otherwise it 
# uses the whole column max instead. 
# The 1.5 is to make it so that you lose ~ 1bpm p/w you don't practice that scale
# at 80bpm.

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="PracticeBuddy"),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    fluidRow(
      box(plotOutput("totalPlot"),  width=12),
      box(plotOutput("scaleTypeTotalPlot"),  width=12),
      box(plotOutput("tonicTotalPlot"),  width=12),
    ),
    fluidRow(
      box(dataTableOutput("scalesToPracticeNext"), width=6),
      box(plotOutput("TempoHistogram"), width=6),
    ),
    fluidRow(
      box(dataTableOutput("QuickWinsTable"), width=6)
    )
   # fluidRow(
  #    box(dataTableOutput("currentSpeedTable"), width=6),
  #  )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  output$totalPlot <- renderPlot({
    grouped_data = data %>% 
      group_by(Date) %>%
      summarize(MedianTempo=median(DecayedTempo), MeanTempo=mean(DecayedTempo))
    
    ggplot(grouped_data, aes(x=Date))+
      geom_line(aes(y=MedianTempo, color="Median")) +
      geom_line(aes(y=MeanTempo, color="Mean")) +  theme_minimal() +
      scale_color_manual(name = "Y series", values = c("Median" = "darkblue", "Mean" = "red")) +
      ggtitle("Mean and Median Speed of Scales (Total, bpm, 4 notes per beat)")
  })
  
  output$scaleTypeTotalPlot <- renderPlot({
    grouped_data = data %>%
      group_by(Date, ScaleType) %>%
      summarize(MedianTempo=median(DecayedTempo), MeanTempo=mean(DecayedTempo))
    ggplot(grouped_data, aes(x=Date, y=MeanTempo, color=ScaleType))+
      geom_line() + geom_point() + theme_minimal() +
      ggtitle(ggtitle("Mean Speed of Scales by Type (bpm, 4 notes per beat)"))
  })
  
  output$tonicTotalPlot <- renderPlot({
    grouped_data = data %>%
      group_by(Date, Tonic) %>%
      summarize(MedianTempo=median(DecayedTempo), MeanTempo=mean(DecayedTempo))
    ggplot(grouped_data, aes(x=Date, y=MeanTempo, color=Tonic))+
      geom_line() + geom_point() + theme_minimal() +
      ggtitle(ggtitle("Mean Speed of Scales by Tonic (bpm, 4 notes per beat)"))
  })
  

  output$scalesToPracticeNext <- DT::renderDataTable({
   current_data %>%
      top_n(-10, DecayedTempo) %>%
      top_n(-10, LastDate) %>%
      select(c("Tonic", "ScaleType", "Articulation", "LastDate", "DecayedTempo", 
               "BestTempo"))
              
  }, options = list(pageLength=10), caption="Scales to Practice Next", autoHideNavigation=TRUE)
 
  output$currentSpeedTable <- renderDataTable({
    current_data %>%
      select(c("Tonic", "ScaleType", "Articulation", "LastDate", "DecayedTempo", 
               "BestTempo"))
  }, options = list(pageLength=10))
   
  output$TempoHistogram <- renderPlot({
    ggplot(current_data, aes(x=DecayedTempo)) + geom_histogram() +theme_minimal() + 
      ggtitle("Current Decayed Tempos")
  })
  
  output$QuickWinsTable <- DT::renderDataTable({
    aug_model %>%
      filter(.resid <= quantile(.resid, c(0.1))) %>%
      mutate(ExpectedTempo = round(.fitted, 0)) %>%
      select(c("Tonic", "ScaleType", "Articulation", "BestTempo", "ExpectedTempo"))
  }, options = list(pageLength=10), caption="Potential Quick Wins", autoHideNavigation=TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
