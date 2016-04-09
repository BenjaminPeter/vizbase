library(shiny)
library(DT)
source("helpers.R")



#! Simple Server to add custom plot modules to a shiny plot,
#! Assuming they follow a template with three functions:
#! 1. %s_main_UI(id, config) displays the plot, including CSS
#! 2. %s_options_UI(id, config) is a set of configs
#! 3. %s_plot(input, output, session, ...) is the main module function,
#!, describing observers and renderers

shinyServer(  function(input, output) {

    config <- list()

    input_files <- reactive({
	files <- list()
	#s <- input$which_data
        s <- "oceania"
        files$pca <- sprintf("pca/flash_%s_dim20.pc", s)
	files$fam <- sprintf("subset/%s.fam", s)
	files$indiv_meta <- sprintf("subset/%s.indiv_meta", s)
        files$pop_display <- sprintf("pgs/gvar.pop_display")
	files$pop_geo <- sprintf("subset/%s.pop_geo", s)
	files$boundary <- sprintf("subset/%s.polygon", s)
	files
    })

    data <- list()

    data$indiv_meta <- reactive({
	print("reload indiv data")
	files <- input_files()
	indiv_meta <- read.csv(files$indiv_meta)
    })

    data$pop_meta <- reactive({
	print("reload pop data")
	pop_display <- read.csv(input_files()$pop_display)
	pop_geo <- read.csv(input_files()$pop_geo)
	pop_meta <- merge(pop_display, pop_geo)

    })
    data$pca <- reactive({
	print("reload pc data")
	files = input_files()
	pcs <- read.table(files$pca)
	names(pcs) <- paste0("PC", 1:ncol(pcs))
	fam <- read.table(files$fam)[,1]
	pc_data <- data.frame(sampleId=fam, pcs, n=1:length(fam))
    })

    data$boundary <- reactive({
	print("reload boundary")
	    boundary <- read.table(input_files()$boundary)
	    names(boundary) <- c('x', 'y')
	    boundary
    })

    get_dataset <- reactive({
	print("reload_all data")
	indiv_meta <- data$indiv_meta()
	pop_meta <- data$pop_meta()
	pca_data <- data$pca()
	indiv <- merge(indiv_meta, pop_meta, all.x=T)
        mdata <- merge(indiv, pca_data, all.y=T)
	mdata <- mdata[order(mdata$n),]
        print(input$coloring)
        if(F){
        if(input$coloring == 'Source'){
            mdata$color <- get_cols_source(mdata)
        } else{
            mdata$color <- get_cols_wrap(mdata)
        }
        }

	test_data <<- mdata

        mdata
    })

    boundary <- reactive({
	    data$boundary()
    })

    get_pop_data <- reactive({
	data <- get_dataset()
	columns <- c('popLabel', 'abbrev', 'latitude', 'longitude', 'wasDerivedFrom',
		  'accuracy', 'name', 'color')
	pop_data <- unique(data[,columns])
	pop_data$sample_size <- aggregate(data$sampleId, data[,columns],
					  length)$x
	pop_data
    })


    #for(mod in modules$names){
	#data[[mod]] <- modules$dataloader[[mod]]
    #}


    output$options <- renderUI({
	mod = input$which_options
	modules$options_ui[[mod]](mod, config[[mod]])
    })


    output$element <- renderUI({
	lapply(modules$names, function(mod){
	    isolate(if(mod %in% input$plot_selector){
		    modules$main_ui[[mod]](mod, config[[mod]])
	    })
	})
    })


    feval <- function(sss)eval(parse(text=sss))

    if(T){
    config <- sapply(modules$names, simplify=F, USE.NAMES=T,
	function(mod){
	    do.call(callModule, c(lapply(modules$args[[mod]], feval),
				  module=modules$plot[[mod]],
				  id=mod))
    })
    }

    if(F){
    config[['map']] <- callModule(module=map_plot,
				  id='map',
				  get_pop_data)
    config[['pc2d']] <- callModule(module=pc2d_plot,
				   id='pc2d',
				   get_dataset, reactive(input$coloring))
    config[['pca_spat']] <- callModule(module=pca_spat_plot,
				   id='pca_spat',
				   get_dataset, data$boundary)
    }

    output$plot <- renderPlot(plot(runif(121)), width=500, height=200)

    output$mydf <- renderUI(list(sliderInput("a", label="Y", max=400,
                                             min=100,  value=100),
                                 plotOutput("PSADF")))
    output$PSADF <- renderPlot(plot(runif(input$a)))

output$tbl <-  DT::renderDataTable({
    x = data$pop_meta()
    rownames(x) <- x$sampleId
    x <- x[,-1]
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
})
