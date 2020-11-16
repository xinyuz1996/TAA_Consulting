
data <- readr::read_delim("tabular data.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

data <- read.table('tabular data.txt',sep="\t", header=TRUE) # This one is good in R, but not functioning well in Rmd

