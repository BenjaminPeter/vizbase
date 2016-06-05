source("ui_fun.R")
source("options_fun.R")
library(shiny)
library(shinydashboard)
library(shinyjs)



header <- dashboardHeader(disable=F)
sidebar <- function(MENUS, DATA_SETS){
    menuItems <- lapply(MENUS, function(M)M$get_menu())
    dashboardSidebar(
	selectInput('which_dataset', 'Which Dataset should be displayed?',
		    choices=DATA_SETS,  selected=DATA_SETS[1]),
	sidebarMenu(
	    withTags(li(a(icon('file'), span("Files"), href="#"),
		     "data-target"='#filesmat-mainbox',
		     'class' = 'toggler', id='filesmat-menu'
			)),

	    .list=menuItems
	)
    )
}


body <- function(FIGURES, TEMPLATES, FILES){
    FIGURES <- c(FIGURES, TEMPLATES, FILES)
    main_boxes <- lapply(FIGURES, function(FIG)box_fig(FIG))
    opt_boxes <- lapply(FIGURES, function(FIG)options_box_fig(FIG))
    dashboardBody(
    includeCSS("www/css/style.css"),
    setup_gridster(),
    test_js,
    popover_js,


    #call main plot boxes
    gridster(main_boxes),


    #set up options side-bar
    right_sidebar(
        tagList(
            div(tags$button("close", style="width:100%",
                id="btn-close-sidebar")),
            opt_boxes
        )
    )
)}

shinyUI(dashboardPage(header, sidebar(MENUS, DATA_SETS), body(FIGURES, TEMPLATES, FILES)))

