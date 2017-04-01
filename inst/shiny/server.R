library(shiny)
library(shinyBS)
library(shinyAce)

#library(ExpEconDB)
source("D:/libraries/ExpEconDB/source project.r")
setwd("D:/libraries/ExpEconDB")
init.ee.shiny("D:/libraries/ExpEconDB")
max_plots=3

counter1 = counter2 = 0 

shinyServer(function(input, output, session) {
  
  
  r.gameId <- reactive(input$gameInput)
  
  r.equ = reactive({
    load.equilibria(r.gameId())
  })
  r.eeo.li <- reactive({
    cat("\nr.eeo()")
    input$updateButton
    isolate({
      eeo.li <- get.eeo.for.variants(r.equ(),gi = r.gi(),variants=r.variant(),by=r.groupBy())
    })
    eeo.li
  })
  
  
  r.variant <- reactive(input$variantInput)
  r.var <- reactive(input$varInput)
  r.groupBy <- reactive(input$groupByInput)
  r.filter <- reactive(input$filterInput)
  r.selector = reactive({
    cat("\nr.selector()")
    list(gameId=r.gameId(),var=r.var(), variant=r.variant(), groupBy=r.groupBy(), filter=r.filter())    
  })
  r.bar1.sel = reactive({
    cat("\nr.bar1.sel()")
    input$updateButton
    sel = isolate(r.selector()); sel$var = get.element(1, sel$var)
    sel
  })
  
  r.sgi <- reactive({
    cat("\nget.sgi()")
    get.gi(r.gameId(), small=TRUE)
  })
  r.gi <- reactive({
    cat("\nget.gi()")
    get.gi(r.gameId(), small=FALSE)
  })

  
  r.gd <- reactive({
    cat("\nload game.data")
    load.game.data(r.gameId())
  })
  
  output$pivotSelectors <- renderUI({
    cat("\nuiPivotSelectors: ",r.gameId())    
    uiPivotSelectors(r.sgi(), r.gd())
  })
  
  output$variantInput <- renderUI({
    cat("\nuiSelectVariant: ", r.gameId())    
    uiSelectVariant(r.gameId(), r.sgi()$gs)
  })
  
  output$gameMain <- renderUI({
    cat("\nuiGameStructure")    
    uiGameStructure(r.sgi())    
    #HTML(list.to.html(r.gs()$obj.li[[1]]))
  })
  
  output$gamePivotTable <- renderDataTable({
    cat("\nuiGamePivotTable")
    var = r.var()
    if (is.null(var))
      return(NULL)
    
    game.data.pivot.table(gi=r.sgi(),gd=r.gd(),var=var,by=r.groupBy(),variant=r.variant(), filter=NULL)

    #uiGamePivotTable(r.sgi(), r.gd(), var = r.var())    
  })

  output$gameBarplot1 <- renderPlot({
    cat("\nrender_gameBarplot1()")
    sel = r.bar1.sel()
    print(sel)
    isolate(game.data.barplot(var=sel$var,gi=r.sgi(),gd=r.gd(),by=sel$groupBy,variant=sel$variant, eeo.li = r.eeo.li()))
  })
  output$gameBarplot2 <- renderPlot({
    input$updateButton
    isolate(game.data.barplot(var.ind=2,var=r.var(),var,gi=r.sgi(),gd=r.gd(),by=r.groupBy(),variant=r.variant(), eeo.li = r.eeo.li()))
  })
  output$gameBarplot3 <- renderPlot({
    input$updateButton
    isolate(game.data.barplot(var.ind=3,var=r.var(),var,gi=r.sgi(),gd=r.gd(),by=r.groupBy(),variant=r.variant(),eeo.li = r.eeo.li()))
  })
 
  
  output$gameParameterTable = renderDataTable({
    cat("\nuiGameParameterTable")    
    dataGameParameterTable(r.sgi())
  },options = list(
      bSortClasses = TRUE,
      bPaginate=FALSE,
      bLengthChange = FALSE,
      bFilter = FALSE,
      bSort = TRUE,
      bInfo = FALSE,
      bAutoWidth = TRUE
     )
  )

})

#runApp("D:/libraries/ExpEconDB/ExpEconDB/inst/shiny")
#runExample("07_widgets")
#bsDemo()
