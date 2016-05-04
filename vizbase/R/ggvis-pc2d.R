require(ggvis)

if(F){
x <- read.table("pca/flash_africa_dim20.pc")
names(x) <- paste0("PC", 1:20)
meta_data <- VizMetaData$new('pgs/gvar')

pca <- cbind(sampleId=read.table('subset/africa.fam')[,1, drop=F],  x)
names(pca)[1] <- 'sampleId'

data <- merge(meta_data$data, pca, all.y=T)

data$name <- factor(data$name)

lb <- linked_brush(keys = data$name, "red")



}

brushed_summary <- function(items, session, page_loc, ...) {
    print("ASBDSAF")
    if (nrow(items) == 0) return()

    L <<- list(...)
    Q <<- items
    pl <<- page_loc

    items$key__ <- NULL
    lines <- Map(function(name, vals) {
        paste0(name, ": ",
               x_bar, " = ", round(mean(vals), 2), " ",
               sigma_hat, " = ", round(sd(vals), 2)
        )
    }, names(items[,1:2]), items[,1:2])
    html <- paste(unlist(lines), collapse = "<br />\n")

    show_tooltip(session, page_loc$r + 5, page_loc$t, html)
}
