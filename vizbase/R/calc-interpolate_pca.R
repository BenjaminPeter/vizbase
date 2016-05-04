suppressPackageStartupMessages({
    library(ggplot2)
    library(magrittr)
    library(rworldmap)
    library(sp)
    library(gstat)
})

#' Functions for spatial interpolation of PCA


spatial_subset_plot <- function(subset, PC, ...){
    l <- make_subset_data(subset)
    plot_factor_interpolation(l$data, l$boundary, PC, ...)
}

make_subset_data <- function(subset){
    data <- load.data(sprintf("pca/flash_%s_dim20.pc", subset),
                      sprintf("subset/%s.fam", subset),
                      sprintf("subset/%s.indiv_meta", subset),
                      "/data/meta/pgs/gvar.pop_display")
    loc <- read.csv(sprintf("subset/%s.pop_geo", subset))
    data <- merge(data,loc)
    boundary <- read.table(sprintf("subset/%s.polygon", subset))
    names(boundary) <- c('x', 'y')


    d2 <- aggregate(data, list(data$popLabel),
                    function(x)ifelse(is.numeric(x), median(x),x[1]) )

    coordinates(d2) <- ~ longitude + latitude
    list(data=d2, boundary=boundary)
}


plot_factor_interpolation <- function(d2, boundary, PC, selected=NULL, ...){
    k <- get_interpolation(d2, boundary, PC, ...)

    ggplot() %>%
        add_krig(k, boundary) %>%
        ggmap_plot() %>%
        add_original_points(d2, PC, selected=selected)
}


ggmap_plot <- function(G){
    m <- getMap("low")
        G + geom_path(data = m, aes(x=long, y=lat, group = group),
                             color = 'black') +
        coord_fixed()
#        geom_polygon(data=boundary, aes(x=x, y=y), fill='red', alpha=.2)
}


add_krig <- function(G, k, boundary, column=3){
    kd <- as.data.frame(k)
    nvar <- as.name(names(kd)[column])
    G + geom_tile(data=kd, aes_(fill=as.name(nvar), x=~x1, y=~x2)) +
                scale_fill_gradient(low="orange", high="blue") +
        scale_x_continuous(expand = c(0,0), limits=range(boundary[,1])) +
        scale_y_continuous(expand = c(0,0), limits=range(boundary[,2]))
}

add_original_points <- function(G, data, PC="PC1", selected=NULL){
    df <- as.data.frame(data)
    df$border_col <- 'black'
    df$border_col[selected] <- 'red'
    G + geom_point(data=df,
                   aes_(x=~longitude, y=~latitude, fill=as.name(PC)),
               size=8, shape=21, stroke=1.42, color=df$border_col) +
                scale_colour_gradient(low="orange", high="blue")
}


get_interpolation <- function(data, boundary, PC="PC1", n=1000, idp=4, maxdist=40,...){
    coordinates(boundary) <- ~ x + y
    extrapolation_points <- spsample(Polygon(boundary), n=n, 'regular')

    #lzn.vgm <- variogram(as.name(PC)~1, data=data)
    #lzn.fit <- fit.variogram(lzn.vgm, model=vgm(model="Mat"))
    #k <- krige(PC2 ~ 1, data, extrapolation_points, model=lzn.fit)
    res <-data.frame(coordinates(extrapolation_points),
          sapply(PC, function(P){
            fml <- as.formula(sprintf("%s ~ 1" ,P))
            x <- idw(fml, data, extrapolation_points, idp=idp, maxdist=maxdist,
                     ...)
            names(x)[names(x) == 'var1.pred'] <- P
            as.data.frame(x)[,P]
           })
          )
    coordinates(res) <- ~ x1 + x2
    res
    }


