library(shiny)


server_fun <- function(FIGURES, TEMPLATES, DASHBOARDS){
    function(input, output, session) {

        # Reactive to keep track of what was selected by the user, 
        # and allows exchanging it between different widgets.
	# - data_set: keeps track which DATA_SET is currently selected, 
	# 	synced between left sidebar and FILES module

	# TODO:
	# - individuals: a vector of ids of selected individuals
	# - pops: a vector of ids of selected populations; 
	#  	(populations are groups of individuals), exact
	#    	interaction with individual TBD
	# - SNP: a vector of SNP ids that are currently selected
        selected = reactiveValues(data_set=DATA_SETS[1],
                                  individuals=NULL,
				  pops=NULL,
				  SNPs=NULL)


        # these observers/reactive keep track of the loaded dataset,
        # and ensure that the input$file_select and input_files stay
        # up to date
        observeEvent(input$which_dataset, {
            selected$data_set <- input$which_dataset
        })
        observeEvent(selected$data_set,{
            updateSelectInput(session, 'which_dataset',
                              selected = selected$data_set)
        })


	# (re)loading data files
        input_files <- reactive({

	    #works, but should use FILES object instead
	    # of global variable
            data_matrix[input$which_dataset,]
        })
        LOADED <- sapply(TEMPLATES, function(tmpl){
             callModule(tmpl$recipe, id=tmpl$id,
                        files=input_files)
            }, USE.NAMES=T
        )

        # displays figures
        fig_res <- lapply(FIGURES, function(fig){
            if(!fig$ggvis_workaround){
            print(LOADED[fig$data_sets])
            cat(paste0("loading element .. ", fig$id))
            callModule(fig$recipe, id=fig$id, 
                       data_sets = LOADED[fig$data_sets],
                       selected=selected
                       )
            print(" ..done")
           } else{
	    #ggvis does not work with shiny modules, see https://github.com/rstudio/ggvis/issues/457
            print(paste0("loading with ggvis workaround", fig$id))
            fig$recipe(input=input, output=output, session=session,
                       data_sets = LOADED[fig$data_sets],
                       selected=selected)
            }
        }
        )
        DEBUG_fig_res <<- fig_res

        # shows table of all possible files, special treatment of the global
        # FIELDS variable
        callModule(FILES$recipe, id=FILES$id, selected=selected)
    }
}


shinyServer( server_fun(FIGURES, TEMPLATES, list()))
