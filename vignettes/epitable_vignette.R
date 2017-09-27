## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(epitable)
library(dplyr)

## ------------------------------------------------------------------------
head(example_data)

## ---- echo=TRUE, results='asis'------------------------------------------
freq_by(dataset = example_data, var_vector = c("color", "clarity"), by_group = "cut" )

