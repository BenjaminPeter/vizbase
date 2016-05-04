library(shiny)
library(R6)
library(DT)
library(sp)
library(leaflet)

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
    attr_names <- names(l)
    if((is.null(attr_names)  && length(l) > 0) || any( attr_names=='' )) {
        stop("only named arguments allowed in `...`")
    }
    lapply(attr_names, function(a){self[[a]] <- l[[a]]})
 }

#' @title Basic Figure Object
#' @description VizFigure defines the main module of the Viz App. You wll
#' likely want to inherit from it for your own figure and datasets. It defines
#' help, options and all other settings (but most can be overridden when
#' object is initialized)
#' @field menu menu / analysis this should be displayed in
#' @field id unique, html-nice id
#' @field name human readable name
#' @field size size (in gridster coordinates, i.e. 140 pixel units)
#' @field pos default position (in gridster tiles, from topleft),
#' @field description Brief description of Figure, displayed on questionmark
#' @field info Information about citation, etc, displayed on info sign
#' @field active_on_startup should this be displayed on startup?
#' @field data_sets list of data sets this figures will look for, they should
#'  be of type VizDataSet
#' @field recipe Shiny Module that returns stuff, and creates all the figures
#' @field initialize constructor function
#' @field body Shiny UI describing content of main box.
#' @field get_menu Menu button
#' @field options_sidebar Shiny UI function to display options in right sidebar
#' @export
VizFigure <-    R6Class("VizFigure",
                     public = list(
                         menu = NULL,
                         id="fig",
                         name ="Generic Figure",
                         size = c(3,3),
                         pos = c(1,1),
                         description = "Set description",
                         info =  "set some info",
                         active_on_startup = F,
                         data_sets = c(),
                         recipe = function(input, output, session, data_sets, selected, ...){
                             print("recipe plt called")
                             output$canvas <- renderPlot({
                                 plot(NA, xlim=0:1, ylim=0:1)
                             })
                         },

                         initialize = function(...){
                             set_more(self, ...)
                             l <- list(...)
                             if(!is.null(l$menu)){
                                 self$menu$add_figure(self)
                             }
                         },
                         body = function(){
                             ns <- NS(self$id)
                             plotOutput(ns("canvas"))
                         },
                         get_menu = function(){
                             MI <- menuSubItem(self$name)
                             MI$attribs[['data-target']] <- sprintf(".%s-mainbox", self$id)
                             if(self$active_on_startup){
                                 MI$attribs[['class']] <- 'toggler active'
                             } else {
                                 MI$attribs[['class']] <- 'toggler'
                             }
                             MI
                         },
                         options_sidebar = function(){
                             print("options called")
                             print(environment())
                             div(class="tab-content",
                                 div(class='tab-pane active',
                                     "No Options Defined"
                                 )
                             )
                         }
                     )
)

#'@export
VizFigureDemo <- R6Class("VizFigureDemo",
    inherit = VizFigure,
    public = list(
        id="runif",
        name ="Uniform Random Variables",
        size = c(5,4),
        description = "Simple Demo About Plotting random uniform  numbers",
        info =  "Info about how program was run,
    how to cite and missing data goes here.",
        recipe = function(input, output, session, data_sets, ...){
            output$canvas <- renderPlot({
                n <- ifelse(is.null(input$nunif), 100, input$nunif)
                plot(runif(n))
            })
        },
        options_sidebar = function(){
            ns <- NS(self$id)
            div(class="tab-content",
                div(class='tab-pane active',
                    sliderInput(ns('nunif'),
                                'How many uniforms would you like?',
                                min=100, max=1e6, step=100, value=100,
                                animate=T)
                )
            )
        }
    )
)



#'@export
VizMapDemo <- R6Class("VizMapDemo",
                         inherit = VizFigure,
                         public = list(
                             id="mapdemo",
                             name ="Map",
                             size = c(6,4),
                             description = "Simple example for leaflet
                             renderer",
                             body = function(){
                                 leafletOutput(sprintf("%s-canvas", self$id),
                                               height=1000)
                             },
                             recipe = function(input, output, session, ...){
                                 output$canvas <- renderLeaflet({
                                    map_leaflet <- leaflet() %>%
                                        addTiles() %>%
                                        addCircleMarkers(lat=1:10, lng=-5:4)

                                 })
                                 observe({leafletProxy(sprintf('%s-canvas', self$id))})
                             }
                         )
)

#'@export
VizMap <- R6Class("VizMap",
  inherit = VizFigure,
  public = list(
      id="map",
      name ="Map",
      size = c(6,4),
      description = "Circles show sample locations",
      body = function(){
          leafletOutput(sprintf("%s-canvas", self$id),
                        height=500)
      },
      data_sets = 'meta',
      recipe = function(input, output, session, data_sets, ...){
          output$canvas <- renderLeaflet({
              x <- data_sets$meta()
              map_leaflet <- leaflet() %>%
                  addTiles() %>%
                  addCircleMarkers(lat=x$latitude, lng=x$longitude,
                                   color=x$color,
                                   radius=(x$accuracy/20+6),
                                   label = paste(x$name, "Accuracy:", x$accuracy))

          })
      }
  )
)

#'@export
VizPCASpat <- R6Class("VizPCASpat",
    inherit = VizFigure,
    public = list(
        id="pca_spat",
        name ="Spatial interpolation of PCA",
        size = c(6,4),
        description = "Circles show sample locations",
        data_sets = c('meta','pca', 'boundary'),
        recipe = function(input, output, session, data_sets, selected=NULL){
            output$canvas <- renderPlot({
                meta <- data_sets$meta()
                pca <- data_sets$pca()
                boundary <- data_sets$boundary()
                x <- merge(meta, pca, all.y=T)
                coordinates(x) <- ~ longitude + latitude

                selIds <- x$sampleId %in% selected$individuals
                selInds <- x$sampleId[selIds]

                print("selected Individuals")
                print(selInds)

                plot_factor_interpolation(x, boundary,
                                          PC=paste0("PC", input$PC),
                                          idp=input$idp, selected=selected$individuals)
            })
        },
        options_sidebar = function(){
            ns <- NS(self$id)
            div(class="tab-content",
                div(class='tab-pane active',
                    sliderInput(ns('idp'),
                                'Choose the interpolation parameter',
                                min=0.001, max=20, value=5),
                    sliderInput(ns('PC'),
                                'Choose the PC to display',
                                min=1, max=20, value=1, step=1
                    )
                )
            )
        }

    )
)






#' @export
VizMenu <- R6Class("VizMenu",
    public = list(
       name ="Debug",
       description = "Basic analsysis class",
       icon = "bar-chart-o",
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
           menuItem(self$name, fig_menus, icon=icon(self$icon))
       },
       add_figure = function(fig){
           self$figures <- c(self$figures, fig)
       }
    )
)





