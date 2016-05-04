
#'@export
VizGGVScatter <- R6Class("VizGGVScatter",
                      inherit = VizFigure,
                      public = list(
                          id="pca_ggv_scatter",
                          name ="Basic scatterplot of 2 PCs",
                          size = c(6,4),
                          description = "Standard Scatterplot of two PCs",
                          data_sets = c('meta','pca'),
                          body = function(){
                              ggvisOutput(sprintf("%s-canvas", self$id))
                          },
                          recipe = function(input, output, session, data_sets, selected=NULL){

                              data <- reactive({
                                  PCX <- paste0('PC', input$PCX)
                                  PCY <- paste0('PC', input$PCY)

                                  pcs <- data_sets$pca()[,c('sampleId', PCX, PCY)]
                                  names(pcs)[-1] <- c('x', 'y')
                                  data <- merge(pcs, data_sets$meta(), all.x=T)
                                  print(c("GGV SCATTER-RECIPE", names(data)))
                                  data$color[1:100] = 'blue'
                                  data
                                  })
                              #reactive({
                                #  print("GGV SCATTER-RECIPE")
                                #


                                  QQQ = data %>% ggvis(x = ~x, y = ~y, key := ~sampleId, fill = ~color, stroke := 'black') %>%
                                      layer_points(stroke.brush := 'purple') %>%
                                      handle_brush(brushed_summary)%>%
                                      add_axis('x', title='') %>%
                                      add_axis('y', title='') %>%
                                      bind_shiny(paste0(self$id, "-canvas"))

                              #})
                          },
                          options_sidebar = function(){
                              ns <- NS(self$id)
                              div(class="tab-content",
                                  div(class='tab-pane active',
                                      sliderInput(ns('PCX'),
                                                  'Choose the PC to display',
                                                  min=1, max=20, value=1, step=1
                                      ),
                                      sliderInput(ns('PCY'),
                                                  'Choose the other PC to display',
                                                  min=1, max=20, value=2, step=1
                                      )
                                  )
                              )
                          }

                      )
)
