library(shiny)
library(shinydashboard)
library(DT)

menuItemNYI <- function(...){
    try(menuItem(..., badgeLabel = "unavailable", 
        badgeColor="red"), silent=T)
    menuItemNYIS(...)
}
menuSubItemNYI <- function(...){
    m <- menuSubItem(...)
    m$children[[1]]$children[[3]] <- HTML('<small class="badge pull-right bg-red">unavailable</small>')
    m
}

menuItemNYIS <- function(...){
    m <- menuItem(...)
    m$children[[1]]$children[[4]] <- HTML('<small class="badge pull-right bg-red">unavailable</small>')
    m
}

setup_gridster <- function(...){
    HTML('<script src="gridster/jquery.gridster.js"></script> 
    <link href="gridster/jquery.gridster.css" rel="stylesheet" />
    <link href="gridster/gridster.style.css" rel="stylesheet" />
    <script type="text/javascript">
        $(function(){ //DOM Ready
            gridster = $(".gridster > ul").gridster({
                widget_margins: [0, 0],
                widget_base_dimensions: [140, 140],
                resize: {
                    enabled: true,
                    min_size: [1, 1]
                },
            draggable: {
            handle: "header"
          }
            }).data("gridster");



            $(document).on("click", ".remove", function(){
                var id=$(this).parent().parent().attr("id");
                var id2 = $(".gridster ul").find("[id=\'" + id + "\']");
                gridster.remove_widget(id2);
            });

        });
    </script>')
}

gridster <- function(...){
    style_str="min-width=100%; min-height=100%; position:absolute; top:0; bottom:0; left:0; right:0;"
        ss2 = sprintf("%s border 41px dashed blue", style_str)
    div(class="gridster", tags$ul(...)
        )
}

box_gridster <- function(head="box", ..., row=1, col=1, sizex=4, sizey=3){
    tags$li(
        "data-row"=row,
        "data-col"=col,
        "data-sizex"=sizex,
        "data-sizey"=sizey,
        style="background:#fff; padding:3px",
    box_gridster_head(head),
    div(..., class="box-gridster-body box-body"),
    box_gridster_footer(),
            class="box box-solid box-danger")
}

button_icon <- function(icon, ...){
    icon_class <- sprintf("fa %s fa-2x", icon)
    tags$button(
        class="btn btn-box-tool",
        style="height:20px",
        ...,
       tags$i( class=icon_class)
    )
}

box_gridster_head <- function(head){
    tags$header(
        button_icon("fa-times ", "data-widget"="remove"),
        button_icon("fa-wrench", "data-toggle"="control-sidebar"),
        button_icon("fa-minus ", "data-widget"="collapse"),
        class="box-header",
        head)
}
box_gridster_footer <- function(){
    tags$footer('done', class="box-footer")
}


plotOutput_b <- function(id){
}

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
            menuSubItemNYI("biplot", tabName = "subitem1"),
            menuSubItemNYI("single PC - bars", tabName = "subitem1"),
            menuSubItemNYI("single PC - violin", tabName = "subitem1"),
            menuSubItemNYI("single PC - spatial Interpolation", tabName = "subitem1")
        ),
        menuItemNYIS("Maps", icon = icon("globe"),
            menuSubItemNYI("Sample Locations", tabName = "subitem1")
        )
    ),
    h5("Options"),
    sliderInput("test", label="X", min=3, max=10, value=4, step=5),
    sliderInput("test", label="X", min=3, max=10, value=4, step=5)
)

body <- dashboardBody(
    setup_gridster(),
    gridster(
        box_gridster(head="P1", plotOutput("plot")),
        box_gridster(head="P2", uiOutput("mydf" )),
        box_gridster(head="#DS", DT::dataTableOutput("tbl"), 
        sizex=4, sizey=6)
    ),
    HTML('
<!-- The Right Sidebar -->
<aside class="control-sidebar control-sidebar-dark">
ASDFAS
</aside>
<!-- The sidebars background -->
<!-- This div must placed right after the sidebar for it to work-->
<div class="control-sidebar-bg">asdfas</div>
')
)

bodys <- dashboardBody(
        box(div(DT::dataTableOutput("tbl"), 
                style="overflow-y: auto; height: 300px"
        ))
                      )


shinyUI(dashboardPage(header, sidebar, body))

