source("ui_fun.R")
source("options_fun.R")
library(shiny)
library(shinydashboard)
library(shinyjs)


popover_js <- tags$script(
    HTML("
    $(document).on('click', '.toggler',function(e) {
      var parent=$(e.target).parent();
      if(parent === undefined) parent=e.target;
      $(parent.data('target')).show();
      var callerid = $(parent.data('target')).attr('id');

      plotid  = callerid.substring(0, callerid.indexOf('-'));
      $('#' + plotid + '-canvas').trigger('show');
      $('#' + plotid + '-canvas').trigger('shown');
      $(parent).addClass('active');
    });

    var popOverSettings = {
        placement: 'bottom',
        container: 'body',
        html: true,
        selector: '[rel=\"popover\"]',
        content: function(){
            return $('#P_target').html();
        }
    }

    $('body').popover(popOverSettings);
"))

test_js <- tags$script(HTML("
    $(document).on('click', '.btnopt', function(e){
           $('.control-sidebar').addClass('control-sidebar-open');
           var callerid = $(e.target).closest('.btnopt').attr('id');
           targetid  = callerid.substring(0, callerid.indexOf('-'));
           $('#' + targetid + '-optbox').show();
    });
    $(document).on('click', '#btn-close-sidebar', function(e){
           $('.control-sidebar').removeClass('control-sidebar-open');
    });
"))

header <- dashboardHeader(disable=F)
sidebar <- function(ANALYSES, DATA_SETS){
    menuItems <- lapply(ANALYSES, function(A)A$get_menu())
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


    #call main plots
    gridster(main_boxes),

    right_sidebar(
        tagList(
            div(tags$button("close", style="width:100%",
                id="btn-close-sidebar")),
            opt_boxes
        )
    )
)}

shinyUI(dashboardPage(header, sidebar(MENUS, DATA_SETS), body(FIGURES, TEMPLATES, FILES)))

