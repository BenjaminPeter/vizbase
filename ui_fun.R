library(shiny)
library(shinydashboard)
library(DT)



#' modified shiny menu with badge for unavailable
menuItemNYI <- function(...){
    menuItemNYIS <- function(...){
        m <- menuItem(...)
        m$children[[1]]$children[[4]] <- HTML('<small class="badge pull-right bg-red">unavailable</small>')
        m
    }
    try(menuItem(..., badgeLabel = "unavailable", 
        badgeColor="red"), silent=T)
    menuItemNYIS(...)
}

#' modified shiny submenu with badge for unavailable
menuSubItemNYI <- function(...){
    m <- menuSubItem(...)
    m$children[[1]]$children[[3]] <- HTML('<small class="badge pull-right bg-red">unavailable</small>')
    m
}


#! initialized gridster for ui
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

        });
    </script>')
}


#! basic gridster tag
gridster <- function(...){
    div(class="gridster", tags$ul(...))
}

box_gridster <- function(id, head="box", ..., 
                         row=1, col=1, sizex=4, sizey=3){
    tags$li(
        "data-row"=row,
        "data-col"=col,
        "data-sizex"=sizex,
        "data-sizey"=sizey,
        style="background:#fff; padding:3px",
    box_gridster_head(head),
    div(..., class="box-gridster-body box-body"),
    box_gridster_footer(),
            class=sprintf("box box-solid box-danger %s", id))
}

#! box making the figure for a FIG object
box_fig <- function(FIG, ...){
    id=sprintf("%s-mainbox", FIG$id)

    classes <- sprintf("box box-solid box-danger %s", id)
    style="background:#fff; padding:3px"
    if(!FIG$active_on_startup){
        style="background:#fff; padding:3px; display:none;"
    }
    

    tags$li(
            id=id,
        "data-row"=FIG$pos[1],
        "data-col"=FIG$pos[2],
        "data-sizex"=FIG$size[1],
        "data-sizey"=FIG$size[2],
        style=style,
        box_fig_head(FIG),
        div(FIG$body()
        #div(plotOutput(sprintf("canvas_%s", FIG$id))
            , class="box-gridster-body box-body"),
        box_gridster_footer(),
            class=classes)
}

box_fig_head <- function(FIG){
    tags$header(
        FIG$name,
        div(class="box-tools pull-right",
            btn_popover("fa-question ", "Help", FIG$description),
            btn_popover("fa-info", "Info", FIG$info),
            button_icon("fa-wrench", id=sprintf("%s-btnopt", FIG$id),
                         class="btnopt btn btn-box-tool"),
            #btn_popover("fa-wrench", "Options", do.call(HTML, lapply(FIG$get_options_popup(), as.character))),
            #button_icon("fa-minus ", "data-widget"="collapse"),
            button_icon("fa-times ", "data-widget"="remove")
        ),
        class="box-header" 
        )
}
box_gridster_footer <- function(...){
    tags$footer('done',..., class="box-footer")
}


#! button at top of box
button_icon <- function(icon, ...){
    icon_class <- sprintf("fa %s fa-2x", icon)
    tags$button(
        type="button",
        class="btn btn-box-tool",
        style="margin-bottom:13px; vertical-align:middle",
        ...,
       tags$i( class=icon_class)
    )
}

btn_popover <- function(icon, title, content, ...){
        button_icon(icon, title=title,
                    "data-toggle"="popover",
                    "data-content"=content,
                    rel="popover", ...)
}


box_gridster_head <- function(head){
    tags$header(
        button_icon("fa-times ", "data-widget"="remove"),
        button_icon("fa-wrench", "data-toggle"="control-sidebar"),
        button_icon("fa-minus ", "data-widget"="collapse"),
        button_icon("fa-info ", "data-toggle"="popover", title="QQ",
             rel="popover"),
        button_icon("fa-question "),
        class="box-header", style="border:3px dashed blue;",
        head)
}


right_sidebar <- function(...){
    s=".control-sidebar-open .control-sidebar,.control-sidebar-open .control-sidebar-bg,.control-sidebar.control-sidebar-open,.control-sidebar.control-sidebar-open+.control-sidebar-bg{right:0px!important}"
            style='width:442px; right:-442px'
    tagList(
        tags$style(".control-sidebar-open { right:0px!important}",
                   type="text/css"),
        tags$style(s,
                   type="text/css"),
        tag("aside",
            varArgs=c(class="control-sidebar control-sidebar-dark", style=style, list(...))),
        div(class="control-sidebar-bg", style=style)
    )

}
