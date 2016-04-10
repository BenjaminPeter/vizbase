#' @title Generic Data Set
#'
#' @description represents a single data set, which we can think of the
#' basis unit of analysis.
#'
#' @details test
#' @export
VizDataSet <- R6Class("VizDataSet",
                      public = list(
                          name ="Generic dataset",
                          description = "Basic dataset",
                          genetic_data = NULL,
                          meta_data = NULL,
                          load = function(...){
                              self$meta_data <- read.csv(...)
                              self$genetic_data <- read.table(...)
                          },
                          initialize = function(name){
                              self$name <- name
                              self$meta_data <- VizMetaData(name)
                              self$meta_data <- VizMetaData(name)
                          }
                      )
)

