src <- list.files("vizbase/R")
src <- paste0("vizbase/R/", src)
print(sprintf("sourcing %s\n", src))
print(getwd())
for(s in src) source(s)
