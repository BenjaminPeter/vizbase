#' @title Generic Data Set
#'
#' @description represents a single data set, which we can think of the
#' basis unit of data we want to handle.
#'
#' @details test
#' @export
VizDataSet <- R6Class("VizDataSet",
                      inherit = VizFigure,
                      public = list(
                          name = "Uninitialized dataset",
                          id = "uninitialized_data",
                          description = "",
                          .data = runif(100),
                          initialize = function(id, name=id, ...){
                              set(self, id)
                              set(self, name)
                              set_more(self, ...)
                              l <- list(...)
                              if(!is.null(l$menu)){
                                  self$menu$add_figure(self)
                              }
                          },
                          recipe = function(input, output, session){
                              return(self$.data)
                          }
                      ),
                      active = list(

                      ),
                      lock_objects = F
)

#' @title Template to load a set of files with predefined names
#' @field templates named vector of strings, constituting default values
#' to be loaded.
VizDataTemplate <- R6Class("VizDataTemplate",
                           inherit = VizDataSet,
                           public = list(
                               templates = c(),
                               body = function(){
                                   DT::dataTableOutput(sprintf("%s-canvas", self$id))
                               }
                           )
)


#' @export
VizFileMat <- R6Class("VizFileMat",
                      inherit=VizDataSet,
                      public=list(
                          name = 'Data Files',
                          id = 'filesmat',
                          active_on_startup = T,
                          data = NA,
                          description = "Matrix of all files",
                          initialize = function(mat){
                              self$data <- mat
                          },
                          recipe = function(input, output, session, selected=NULL){
                              ns <- session$ns
                              output$canvas <- DT::renderDataTable({
                                  x = as.data.frame(self$data)
                                  DT::datatable(
                                      x, options = list(
                                          lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                          pageLength = 15,
                                          colReorder = T,
                                          scrollX = T,
                                          scrollY = T,
                                          dom = 'Bfrtip',
                                          buttons = c('csv', 'colvis')
                                      ),
                                      extensions = c('Buttons', 'ColReorder'),
                                      selection = 'single')
                              })
                              proxy <- dataTableProxy(ns("canvas"))

                              observeEvent(input$canvas_rows_selected, {
                                  row_id <- rownames(self$data)[input$canvas_rows_selected]
                                   selected$data_set <- row_id
                               })

                              observe({
                                  rows <- which(rownames(self$data) %in% selected$data_set)
                                  print("files-table: observer")
                                  print(names(self$data))
                                  print(rownames(self$data))
                                  print(colnames(self$data))
                                  print(selected$data_set)
                                  DT::selectRows(proxy, rows)
                              })

                          },
                          body = function(){
                              DT::dataTableOutput(sprintf("%s-canvas", self$id))
                          }
                      ))



#' Data structure handling pca results data from flashpca,
#' using generic input name
#' @export
VizPCATemplate <- R6Class('VizPCATemplate',
      inherit = VizDataTemplate,
      lock_objects = F,
      public = list(
          templates = c(pc='pca/flash_%s_dim20.pc',
                         fam='subset/%s.fam'),
          recipe = function(input, output, session, files){
              data <- reactive({
                    src = files()

                  fam <- data.frame(sampleId=read.table(src['fam'])[,1])
                  pc <- read.table(src['pc'])
                  names(pc) <- sprintf("PC%s", 1:ncol(pc))
                  cbind(fam, pc)
              })

              output$canvas <- DT::renderDataTable({
                  x =data()
                  print(c('xdim:', dim(x)))
                  DT::datatable(
                      x, options = list(
                          lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                          pageLength = 15,
                          colReorder = T,
                          scrollX = T,
                          scrollY = T,
                          dom = 'Bfrtip',
                          buttons = c('csv', 'colvis')
                      ),
                      extensions = c('Buttons', 'ColReorder'))
              })
              return(data)
          }
      )
)

#' Data structure handling pca results data from flashpca,
#' using generic input name
#' @export
VizMetaTemplate <- R6Class('VizMetaTemplate',
  inherit = VizDataTemplate,
  lock_objects = F,
  public = list(
      templates = c(indiv_meta='subset/%s.indiv_meta',
                    pop_display='subset/%s.pop_display',
                    pop_geo='subset/%s.pop_geo'),
      recipe = function(input, output, session, files){
          data <- reactive({
              src = files()

              pd <- read.csv(src['pop_display'], as.is=T)
              pg <- read.csv(src['pop_geo'], as.is=T)
              im <- read.csv(src['indiv_meta'], as.is=T)
              pop_data <- merge(pd, pg)

              data <- merge(im, pop_data, all.x=T)
          })

          output$canvas <- DT::renderDataTable({
              x = data()
              print(c('xdim:', dim(x)))
              DT::datatable(
                  x, options = list(
                      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                      pageLength = 15,
                      colReorder = T,
                      scrollX = T,
                      scrollY = T,
                      dom = 'Bfrtip',
                      buttons = c('csv', 'colvis')
                  ),
                  extensions = c('Buttons', 'ColReorder'))
          })
          return(data)
      }
  )
)

#' Data structure handling boundary polygon of a region
#' using generic input name
#' @export
VizBoundaryTemplate <- R6Class('VizBoundaryTemplate',
                           inherit = VizDataTemplate,
                           lock_objects = F,
                           public = list(
                               templates = c(boundary='subset/%s.polygon'),
                               recipe = function(input, output, session, files){
                                   data <- reactive({
                                       src = files()
                                       bd <- read.table(src['boundary'])
                                       names(bd) <- c('x','y')
                                       bd
                                   })


                                   output$canvas <- DT::renderDataTable({
                                       x = data()
                                       print(c('xdim:', dim(x)))
                                       DT::datatable(
                                           x, options = list(
                                               lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                               pageLength = 15,
                                               colReorder = T,
                                               scrollX = T,
                                               scrollY = T,
                                               dom = 'Bfrtip',
                                               buttons = c('csv', 'colvis')
                                           ),
                                           extensions = c('Buttons', 'ColReorder'))
                                   })
                                   return(data)
                               }
                           )
)

