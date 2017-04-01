library(shiny)
library(shinyBS)
library(shinyAce)
library(dplyrExtras)

options(warn=1)
#library(ExpEconDB)
source("D:/libraries/ExpEconDB/source project.r")
setwd("D:/libraries/ExpEconDB")
init.ee.shiny("D:/libraries/ExpEconDB")

shinyUI(pageWithSidebar(
    headerPanel=headerPanel("XEcon Explorer"),
    sidebarPanel=sidebarPanel(
        uiSelectGame(),
        uiOutput("pivotSelectors"),
        uiComment()
    ), 
    mainPanel=mainPanel(tabsetPanel(id ="gameTabs",
      tabPanel("Structure",uiOutput("gameMain")),
      tabPanel("Data",dataTableOutput("gamePivotTable")),
      tabPanel("Barplot",
       plotOutput("gameBarplot1"),
       plotOutput("gameBarplot2"),
       plotOutput("gameBarplot3")
      ),
      tabPanel("Equilibria")
    ))
))


 
#runApp("D:/libraries/ExpEconDB/ExpEconDB/inst/shiny")