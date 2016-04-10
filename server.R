library(shiny)
library(DT)
source("ui_fun.R")

CONTENT=gsub("\n", " ", HTML(as.character(sliderInput("X", "X", 1, 100, 4))))



#! Simple Server to add custom plot modules to a shiny plot,
#! Assuming they follow a template with three functions:
#! 1. %s_main_UI(id, config) displays the plot, including CSS
#! 2. %s_options_UI(id, config) is a set of configs
#! 3. %s_plot(input, output, session, ...) is the main module function,
#!, describing observers and renderers

shinyServer(  function(input, output) {



    b_plot <- sprintf('b_%s', FIG$name)
    output[[b_plot]] <- renderPlot(FIG$recipe, quoted=T, width=500, height=500)

    output$mydf <- renderUI(list(sliderInput("a", label="Y", max=4e6,
                                             min=100,  value=100),
                                 plotOutput("PSADF")))
    output$PSADF <- renderPlot(plot(runif(input$a)))

    output$tbl <-  DT::renderDataTable({
        x = iris
        DT::datatable(
          x, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15,
        colReorder = T,
        scrollX = T,
        scrollY = T,
        dom = 'Bfrtip',
        #columnDefs = list(list(targets=c(1:9,12,15:33),
        #           visible=F)),
        buttons = c('csv', 'colvis')
          ),
        extensions = c('Buttons', 'ColReorder'))})

    output$POPOVER <- renderUI({
        sliderInput("POPSLIDER", "a slider", min=0, max=10, value=4)
    })

    observeEvent(input$nunif,
        print("ASDFSADFAS")
    )

    option_panel_controller <- reactiveValues(x=NULL)
    observeEvent(input$ob_runif, {
                 option_panel_controller$x <- FIG
    })

    output$sidebar_dyn <- renderUI({
        x <- option_panel_controller$x
        if(is.null(x)) {
            sb <- div("empty")
        } else {
             print("FIRE!!!")
             print(x)
            sb <- eval(x$get_options_sidebar())
        }
        div(h3("Options for runif"),
            sb)
    })

})
