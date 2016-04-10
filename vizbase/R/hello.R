library(shiny)
library(R6)

CONTENT=gsub("\n", " ", HTML(as.character(checkboxInput("X", "X"))))

#' @export
VizFigure <- R6Class("VizFigure",
                       public = list(
                           analysis = 1,
                           name ="Generic Figure",
                           size = c(5,4),
                           pos = c(1,1),
                           description = "This is a generic figure",
                           help_text = "HALP",
                           recipe = quote({
                               n <- ifelse(is.null(input$nunif), 100, input$nunif)
                               plot(runif(n))
                               }),



                           initialize = function(name, analysis, ...){
                                self$name = name
                                self$analysis = analysis
                                if(!is.null(analysis)){
                                    self$analysis$add_figure(self)
                                }
                           },
                           get_menu = function(){
                               MI <- menuSubItem(self$name)
                               MI$attribs[['class']] <- 'active toggler'
                               MI$attribs[['data-target']] <- sprintf(".t_%s", self$name)
                              MI
                           },

                           get_help = function(){
                               "Simple Demo About Plotting random uniform
                               numbers"
                           },

                           get_info = function(){
                               "Info about how program was run,
                               how to cite and missing data goes here."
                           },

                           get_options_popup = function()list(
                               sliderInput('nunif',
                                           'How many uniforms would you like?',
                                           min=100, max=1e6, step=100, value=100,
                                           animate=T),
                               tags$script("$('#nunif').ionRangeSlider();")
                          ),

                          get_options_sidebar = function(){quote(
                              div(class="tab-content",
                                  div(class='tab-pane active',
                                      sliderInput('nunif',
                                                  'How many uniforms would you like?',
                                                  min=100, max=1e6, step=100, value=100,
                                                  animate=T)
                                  )
                              )
                          )},




                           get_box = function(){
                               box_fig(self, id=sprintf("t_%s", self$name),
                                            head=self$name,
                                            plotOutput(sprintf("b_%s", self$name))
                               )
                           }
                       )
)

#' @export
VizAnalysis <- R6Class("VizAnalysis",
                       public = list(
                           name ="Debug",
                           description = "Basic analsysis class",
                           figures = list(),
                           initialize = function(name="Debug", desc="", ...){
                               self$name = name
                               self$description = desc
                               self$figures = list()
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




VizMetaData <- R6Class('VizMetaData',
                       public = list(
                           file_name = "test.meta",
                           loader = read.csv,
                           data = NULL

                       )
)

VizAnalysisData <- R6Class('VizMetaData',
                       public = list(
                           file_name = "test.meta",
                           loader = read.csv,
                           data = NULL,
                           anlysis = VizAnalysis,
                           description = ""
                       )
)
