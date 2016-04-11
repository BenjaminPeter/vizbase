source("ui_fun.R")
source("options_fun.R")
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

popover_js <- tags$script(
    HTML("
    $(document).on('click', '.toggler',function(e) {
      var parent=$(e.target).parent();
      $(parent.data('target')).show();
      var callerid = $(parent.data('target')).attr('id');
      plotid  = callerid.substring(callerid.indexOf('_') + 1);
      $('#canvas_' + plotid).trigger('show');
      $('#canvas_' + plotid).trigger('shown');
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
           targetid  = callerid.substring(callerid.indexOf('_') + 1);
           $('#optbox_' + targetid).show();
    });
    $(document).on('click', '#btn-close-sidebar', function(e){
           $('.control-sidebar').removeClass('control-sidebar-open');
    });
"))

header <- dashboardHeader(disable=F)
sidebar <- function(ANALYSES){
    menuItems <- lapply(ANALYSES, function(A)A$get_menu())
    dashboardSidebar(sidebarMenu(
        menuItemNYI("Data", tabName = "dashboard", icon = icon("dashboard"),
            menuSubItemNYI("Summary", tabName = "subitem1", icon=icon("book")),
            menuSubItemNYI("Metadata", tabName = "subitem1"),
            menuSubItemNYI("Genotype Data", tabName = "subitem1")
        ),
        menuItemNYI("EEMS", icon = icon("map"), tabName = "widgets"),
        menuItemNYI("Admixture", icon = icon("bar-chart-o"), tabName = "widgets"),
        menuItemNYI("PCA", icon = icon("bar-chart-o"),
            menuSubItemNYI("biplot", tabName = "tbl"),
            menuSubItemNYI("single PC - bars", tabName = "subitem1"),
            menuSubItemNYI("single PC - violin", tabName = "subitem1"),
            menuSubItemNYI("single PC - spatial Interpolation", tabName = "subitem1")
        ),
        menuItemNYI("Maps", icon = icon("globe"),
            menuSubItemNYI("Sample Locations", tabName = "subitem1")
        ),
        .list=menuItems
    ))
}


body <- function(FIGURES){
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

shinyUI(dashboardPage(header, sidebar(ANALYSES), body(FIGURES)))

