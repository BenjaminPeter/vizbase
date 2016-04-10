source("ui_fun.R")
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)





s="
$('.my-popover').popover({
    html:true,
    placement:'right',
    title: 'TTTT',
    trigger: 'hover',
    content:function(){
        alert('sdfads');
        return $($(this).data('contentwrapper')).html();
    }
});
"


test_js <- tags$script(
HTML("
    $(document).on('click', '.toggler',function(e) {
      var parent=$(e.target).parent();
      $(parent.data('target')).toggle();
      $(parent).toggleClass('active');
    });
    $(document).on('click', '.my-popover',function(e) {
        alert('sdfads');
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

")
                       )
popover_content <- HTML(as.character(
                     sliderInput("X", min=1, max=2, value=1, label=2)))
popover_content <- gsub("\n", "", popover_content)


header <- dashboardHeader(disable=F)
sidebar <- dashboardSidebar(
    sidebarMenu(
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
        menuItemNYIS("Maps", icon = icon("globe"),
            menuSubItemNYI("Sample Locations", tabName = "subitem1")
        ),
        A$get_menu()

    ),
    h5("Options"), 
    sliderInput("test", label="X", min=3, max=1e6, value=4, step=5),
    sliderInput("stest2", label="X", min=3, max=10, value=4, step=5)
)


body <- dashboardBody(
    includeCSS("www/css/style.css"),
    setup_gridster(),
    test_js,
    gridster(
        box_fig(FIG),
        box_gridster(id="X", head="P2", uiOutput("mydf" )),
        box_gridster(id="SDF", head="#DS", DT::dataTableOutput("tbl"), 
        sizex=4, sizey=6)
    ),

    right_sidebar(uiOutput("sidebar_dyn"))


)

shinyUI(dashboardPage(header, sidebar, body))

