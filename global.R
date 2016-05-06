source("vizbase/import.R")
#library(vizbase)

M <- VizMenu$new(name="DEBUG", description="minimal example analysis")
Mdata <- VizMenu$new(name="Data Tables", description="Tables of raw data files",
			 icon='table')
Mmap <- VizMenu$new(name="Maps", description="Maps of the data",
			 icon='globe')
Mpca <- VizMenu$new(name="Principal Component Analysis", description="minimal example analysis",
			 icon='arrows-alt')



FIG <- VizFigure$new(menu=M, active_on_startup=F)
FIG2 <- VizFigureDemo$new(menu=M, active_on_startup=F)
FIG3 <- VizMap$new(menu=Mmap, active_on_startup=F)
FIG4 <- VizPCASpat$new(menu=Mpca, active_on_startup=F)
FIG5 <- VizGGVScatter$new(menu=Mpca, active_on_startup=T)
FIGSEL <- VizSelectionDebug$new(menu=M, active_on_startup=T)

TEMPLATES <- list(pca=VizPCATemplate$new(id='pca', menu=Mdata),
		  meta=VizMetaTemplate$new(id='meta', menu=Mdata),
		  boundary=VizBoundaryTemplate$new(id='boundary',
			  menu=Mdata))

MENUS <- list(Mdata, Mmap, Mpca, M)
DATA_SETS = c("africa", "india", "oceania")
FIGURES <- list(FIGSEL, FIG5, FIG2, FIG3, FIG4)

FILE_TEMPLATES <- unlist(unname(lapply(TEMPLATES, function(TPL)TPL$templates)))
data.matrix <- sapply(FILE_TEMPLATES, sprintf, DATA_SETS)
rownames(data.matrix) <- DATA_SETS

FILES <- VizFileMat$new(mat=data.matrix)
