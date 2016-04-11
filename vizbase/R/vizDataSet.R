library(R6)

#' @title Generic Data Set
#'
#' @description represents a single data set, which we can think of the
#' basis unit of analysis.
#'
#' @details test
#' @export
VizDataSet <- R6Class("VizDataSet",
    public = list(
        name = "Generic dataset",
        id = "data",
        description = "",
        status = FALSE, #T if successfully loaded

        initialize = function(id, name=NULL, ...){
            set(self, id)
            self$name = ifelse(is.null(name), id, name)
            set_more(self, ...)
        }
    )
)


#'@export
VizDataFile <- R6Class('VizDataCSVFile',
      inherit = VizDataSet,
      public = list(
          file_name = NULL,
          data = NULL,
          load = function(){print("parent")},
          initialize = function(file_name){
              set(self, file_name)
              self$id <- file_name
              self$name <- file_name
              self$load()
          }
      )
)
#'@export
VizCSVDataFile <- R6Class('VizCSVDataFile',
    inherit = VizDataFile,
    public = list(
        load = function(...){
            self$data <-read.csv(self$file_name)
            self$status <- T
        }
    )
)

#'@export
VizWSDataFile <- R6Class('VizWSDataFile',
      inherit = VizDataFile,
      public = list(
          has_header = F,
          load = function(...){
              set_more(self, ...)
              self$data <-read.table(self$file_name, header=self$has_header)
              self$status <- T
          }
      )
)



#' Data structure handling meta data
#' @field files If length one, name of all files, otherwise names of
#'  individual files
#' @export
VizMetaData <- R6Class('VizMetaData',
    inherit = VizDataSet,
    public = list(
        indiv_label = NULL,
        indiv_prov = NULL,
        pop_display = NULL,
        pop_geo = NULL,
        .data = NULL,
        .indiv_data = NULL,
        .pop_data = NULL,
        initialize = function(files, ...){
            if(length(files)==1){
                self$indiv_label = VizCSVDataFile$new(sprintf('%s.indiv_label', files))
                self$indiv_prov = VizCSVDataFile$new(sprintf('%s.indiv_prov', files))
                self$pop_display = VizCSVDataFile$new(sprintf('%s.pop_display', files))
                self$pop_geo = VizCSVDataFile$new(sprintf('%s.pop_geo', files))
                self$status = T
            } else if(length(files)==4){
                self$indiv_label = VizCSVDataFile$new(files[1])
                self$indiv_prov = VizCSVDataFile$new(files[2])
                self$pop_display = VizCSVDataFile$new(files[3])
                self$pop_geo = VizCSVDataFile$new(files[4])
                self$status = T
            } else if(length(files)==3){
                self$load_meta_style(files[1], files[2], files[3])
                self$status =T
            } else {
                print('warning: not init')
            }
        },
        load_meta_style = function(meta, pop_display, pop_geo){
            self$indiv_label = VizCSVDataFile$new(meta)
            self$indiv_prov = VizCSVDataFile$new(meta)
            self$pop_display = VizCSVDataFile$new(pop_display)
            self$pop_geo = VizCSVDataFile$new(pop_geo)
            self$status = T
        }
    ),
    active = list(
        indiv_data = function(){
            if(!is.null(self$.indiv_data)) return(self$.indiv_data)
            self$.indiv_data <- merge(self$indiv_prov$data,
                                      self$indiv_label$data)
        },
        pop_data = function(){
            if(!is.null(self$.pop_data)) return(self$.pop_data)
            self$.pop_data <- merge(self$pop_display$data,
                                      self$pop_geo$data)
        },
        data = function(){
            if(!is.null(self$.data)) return(self$.data)
                self$.data <- merge(self$indiv_data,
                                    self$pop_data, all.x=T)
        }
    )
)

#' Data structure handling pca results data from flashpca
#' @field files If length one, name of all files, otherwise names of
#'  individual files
#' @export
VizPCAData <- R6Class('VizPCAData',
    inherit = VizDataSet,
    public = list(
        data = NULL,
        fam = NULL, pc=NULL,
        initialize = function(pc=NULL, fam=NULL, ...){
            print("init")
            set_more(...)
            fam <- data.frame(sampleId=read.table(fam)[,1])
            pc <- read.table(pc)
            names(pc) <- sprintf("PC%s", 1:ncol(pc))
            self$fam <- fam
            self$pc <- pc
            print(dim(fam))
            self$data <- cbind(fam, pc)
        }
    )
)
