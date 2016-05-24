library(shiny)


server_fun <- function(FIGURES, TEMPLATES, DASHBOARDS){
    function(input, output, session) {

        # Reactive to keep track of what was selected by the user, 
        # and allows exchanging it between different widgets.
        # Currently ther are two values; data_set; which keeps track 
        # of the loaded data set, and individuals, which keeps track
        # of individuals
        selected = reactiveValues(data_set=DATA_SETS[1],
                                  individuals=NULL)

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

        # named vector of file names to be loaded. Gets passed to
        # data modules
        input_files <- reactive({
            data.matrix[input$which_dataset,]
        })


        # loads data files
        LOADED <- sapply(TEMPLATES, function(tmpl){
             callModule(tmpl$recipe, id=tmpl$id,
                        files=input_files)
            }, USE.NAMES=T
        )
        DEBUG_LOADED <<- LOADED


        # displays figures
        fig_res <- lapply(FIGURES, function(fig){
            if(!fig$ggvis_workaround){
            print(LOADED[fig$data_sets])
            cat(paste0("loading.. ", fig$id))
            callModule(fig$recipe, id=fig$id, 
                       data_sets = LOADED[fig$data_sets],
                       selected=selected
                       )
            print(" ..done")
           } else{
            print(paste0("loading with ggvis workaround", fig$id))
            fig$recipe(input=input, output=output, session=session,
                       data_sets = LOADED[fig$data_sets],
                       selected=selected)
            }
        }
        )
        DEBUG_fig_res <<- fig_res

        # shows table of all possible files
        callModule(FILES$recipe, id=FILES$id, selected=selected)
    }
}


shinyServer( server_fun(FIGURES, TEMPLATES, list()))
