library(shiny)
library(DT)
source("ui_fun.R")
library(vizbase)



server_fun <- function(FIGURES){
    function(input, output) {

        controller <- reactiveValues(option_panel=NULL)

        for(FIG in FIGURES){

            # main plot renderer
            b_plot <- sprintf('canvas_%s', FIG$id)
            if(is.null(FIG$renderEnv)){
                f <- match.fun(FIG$renderFun)
            } else {
                f <- get(FIG$renderFun, envir=FIG$renderEnv)
            }
            ff <<- f
            output[[b_plot]] <- f(FIG$recipe, quoted=T)

            # option box display control
            opt_plot <- sprintf('optbox_%s', FIG$id)
            observeEvent(input$ob_runif, {
                         controller$option_panel <- FIG
            })
        }
    }
}


shinyServer( server_fun(FIGURES))
