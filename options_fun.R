library(shiny)

options_box <- function(name, id, content){
    b <- box(width=NULL,
        title=sprintf("Options for %s", name),
        background=NULL,
        solidHeader=T,
        collapsible=T,
        content
    )
    btn.rm <- tags$button(icon("times"), class="btn btn-box-tool", 
                          "data-widget"="remove")
    b <- b$children[[1]]
    b$children[[1]]$children[[2]]$children[[2]] <- btn.rm
    b$attribs[['style']] <- "display: none; border: 3px dashed #ffb000"
    b$attribs[['id']] <- id
    b
}

#' Creates placeholder for option display
options_box_fig <- function(FIG){
    name <- FIG$name
    id <- sprintf("%s-optbox", FIG$id)
    content <- FIG$options_sidebar()
    options_box(name, id, content)
}
