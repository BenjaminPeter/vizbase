library(shiny)
library(R6)
library(DT)

#' Function to shorten init
set <- function(self, value){
    name <- deparse(substitute(value))
    if(!is.null(value)){
        self[[name]] <- value
    }
    return(self)
}

#' Function to shorten init
set_more <- function(self, ...){
    l <- list(...)
    print(l)
    attr_names <- names(l)
    if((is.null(attr_names)  && length(l) > 0) || any( attr_names=='' )) {
        stop("only named arguments allowed in `...`")
    }
    lapply(attr_names, function(a){self[[a]] <- l[[a]]})
 }

#' @export
VizFigure <- R6Class("VizFigure",
                     public = list(
                         analysis = 1,
                         id="fig",
                         name ="Generic Figure",
                         size = c(3,3),
                         pos = c(1,1),
                         renderFun = "renderPlot",
                         renderEnv = NULL,
                         description = "Set description",
                         info =  "set some info",
                         active_on_startup = T,
                         recipe = quote({plot(NA, xlim=0:1, ylim=0:1)
                         }),

                         initialize = function(...){
                             set_more(self, ...)
                             l <- list(...)
                             if(!is.null(l$analysis)){
                                 self$analysis$add_figure(self)
                             }
                         },
                         get_menu = function(){
                             MI <- menuSubItem(self$name)
                             MI$attribs[['data-target']] <- sprintf(".mainbox_%s", self$id)
                             if(self$active_on_startup){
                                 MI$attribs[['class']] <- 'toggler active'
                             } else {
                                 MI$attribs[['class']] <- 'toggler'
                             }
                             MI
                         },
                         options_sidebar = function(){quote(
                             div(class="tab-content",
                                 div(class='tab-pane active',
                                     "No Options Defined"
                                 )
                             )
                         )}
                     )
)

#'@export
VizFigureDemo <- R6Class("VizFigureDemo",
                    inherit = VizFigure,
                     public = list(
                         id="runif",
                         name ="Uniform Random Variables",
                         size = c(5,4),
                         description = "Simple Demo About Plotting random uniform
                         numbers",
                         info =  "Info about how program was run,
                               how to cite and missing data goes here.",
                         recipe = quote({
                             n <- ifelse(is.null(input$nunif), 100, input$nunif)
                             plot(runif(n))
                         }),

                         options_sidebar = function(){quote(
                             div(class="tab-content",
                                 div(class='tab-pane active',
                                     sliderInput('nunif',
                                                 'How many uniforms would you like?',
                                                 min=100, max=1e6, step=100, value=100,
                                                 animate=T)
                                 )
                             )
                         )}
                     )
)

#'@export
VizDataDisplay <- R6Class("VizDataDisplay",
    inherit = VizFigure,
    public = list(
        data_set = NULL,
        renderFun = "renderDataTable",
        renderEnv = as.environment("package:DT"),
        recipe = quote({
            x = FIG$data_set$data
            xx <<- x
            print("desperately trying dt")
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
            extensions = c('Buttons', 'ColReorder'))
        })

    )
)

#' @export
VizAnalysis <- R6Class("VizAnalysis",
                       public = list(
                           name ="Debug",
                           description = "Basic analsysis class",
                           figures = list(),
                           initialize = function(name=NULL, description=NULL, ...){
                               self$figures = list()

                               set(self, name)
                               set(self, description)
                               set_more(self, ...)

                           },
                           get_menu = function(){
                               fig_menus <- lapply(self$figures,
                                                   function(fig)fig$get_menu())
                               menuItem(self$name, fig_menus)
                           },
                           add_figure = function(fig){
                               self$figures <- c(self$figures, fig)
                           }
                       )
)





