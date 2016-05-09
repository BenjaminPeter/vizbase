
#'@export
VizGGVScatter <- R6Class("VizGGVScatter",
                      inherit = VizFigure,
                      public = list(
                          id="pca_ggv_scatter",
                          name ="Basic scatterplot of 2 PCs",
                          size = c(6,4),
                          ggvis_workaround = T,
                          description = "Standard Scatterplot of two PCs",
                          data_sets = c('meta','pca'),
                          body = function(){
                              ggvisOutput(sprintf("%s-canvas", self$id))
                          },
                          click_handler = function(data=NULL, items=NULL, ...){
                              if(missing(data)){
                                  data=items
                              }
                              print("CLICKCLICK")
                              print(data)
                          },
                          recipe = function(input, output, session, data_sets, selected=NULL,
                                             ...){
                                print("in ggvis")
                              print(data_sets)
                              datas <- reactive({
                                  print("reacting to data changing")
                                  PCX <- paste0('PC', 1)#input$PCX)
                                  PCY <- paste0('PC', 2)#input$PCY)

                                  pcs <- data_sets$pca()[,c('sampleId', PCX, PCY)]
                                  names(pcs)[-1] <- c('x', 'y')
                                  data <- merge(pcs, data_sets$meta(), all.x=T)
                                  data$color <- as.character(data$color)
                                  print(c("GGV SCATTER-RECIPE", names(data)))
                                  data
                                  })

                              test_click_handler <- function(data, ...){
                                  reactive({
                                  print(data)
                                     d <- datas()
                                     is_selected <- as.character(data$sampleId) %in% as.character(d$sampleId)
                                    d$color[is_selected] <- 'yellow'

                              })}


                                  QQQ = datas %>% ggvis(x = ~x, y = ~y, key := ~sampleId, fill := ~color, stroke := 'black') %>%
                                      layer_points(stroke.brush := 'purple', stroke.hover:='green') %>%
                                      handle_click(test_click_handler) %>%
                                      scale_nominal('fill') %>%
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
