source("vizbase/import.R")
#library(vizbase)

#this is still preliminary, there may be a better way to organize
# there are 4 global variables: FIGURES, TEMPLATES, FILES and MENUS that
# define the content
# - MENUS are shown in the left sidebar; and simply allow activating figures
# - FIGURES are the main module, they consist of a main plot + some attached UI
# - FILES are the source files from which data is loaded
# - TEMPLATES specify which files are needed for which figure
# TODO:
# - DASHBOARDS: pre-arranged sets of modules that can be opened with one click
# 	ideal signature would be. Each dashboard should be its own menu (or maybe
# 	have dashboard menu in top header bar (?). Something along the lines of
#
# 	DASH_FIG_1 <- VizFigure$new( "stuff", x=0, y=0, width=12, height=5)
# 	DASH_FIG_2 <- VizFigure$new( "stuff", x=0, y=5, width=8, height=3)
# 	DASH_FIG_3 <- VizFigure$new( "stuff", x=8, y=5, width=4, height=3)
# 	D <- VizDashboard$new(list(DASH_FIG1, DASH_FIG2, DASH_FIG3))
# 	MENUS <- c(MENUS, D)


# set up menu
Mdata <- VizMenu$new(name="Data Tables", description="Tables of raw data files",
			 icon='table')
Mmap <- VizMenu$new(name="Maps", description="Maps of the data",
			 icon='globe')
Mpca <- VizMenu$new(name="Principal Component Analysis", description="minimal example analysis",
			 icon='arrows-alt')

MDEBUG <- VizMenu$new(name="DEBUG", description="minimal example analysis")
MENUS <- list(Mdata, Mmap, Mpca, MDEBUG)


# set up figures
FIG <- VizFigure$new(menu=MDEBUG, active_on_startup=F)
FIG2 <- VizFigureDemo$new(menu=MDEBUG, active_on_startup=F)
FIG3 <- VizMap$new(menu=Mmap, active_on_startup=T)
FIG4 <- VizPCASpat$new(menu=Mpca, active_on_startup=T)
FIG5 <- VizGGVScatter$new(menu=Mpca, active_on_startup=T)
FIGSEL <- VizSelectionDebug$new(menu=MDEBUG, active_on_startup=F)

FIGURES <- list(FIGSEL, FIG5, FIG2, FIG3, FIG4)


# set up templates, i.e.  setting up naming rules for files to be loaded
TEMPLATES <- list(pca=VizPCATemplate$new(id='pca', menu=Mdata),
		  meta=VizMetaTemplate$new(id='meta', menu=Mdata),
		  boundary=VizBoundaryTemplate$new(id='boundary',
			  menu=Mdata))
FILE_TEMPLATES <- unlist(unname(lapply(TEMPLATES, function(TPL)TPL$templates)))


# DATA_SETS  contain independent data that can be swtiched out on the fly
DATA_SETS = c("europe", "africa")
#data_matrix stores all file names that will be needed
data_matrix <- sapply(FILE_TEMPLATES, sprintf, DATA_SETS)
rownames(data_matrix) <- DATA_SETS

FILES <- VizFileMat$new(mat=data_matrix)
