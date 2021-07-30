# Principal Component Analysis example:
# https://www.datacamp.com/community/tutorials/pca-analysis-r?utm_source=adwords_ppc&utm_campaignid=1565261270&utm_adgroupid=67750485268&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=332661264371&utm_targetid=dsa-429603003980&utm_loc_interest_ms=&utm_loc_physical_ms=9005923&gclid=CjwKCAjw55-HBhAHEiwARMCszhNn6dhHLtZ25tiI91BoDAXqcsJyOAurhKPMpYfqKH52hYS2dIGy_BoC-AIQAvD_BwE
library(dplyr)
library(tidyr)
library(devtools)
#install_github("vqv/ggbiplot") # select 1 and yes
library(ggbiplot)

# This example uses the built-in dataset, mtcars, without the categorical variables (vs and am)
data <- select(mtcars, -vs, -am)
head(data)
pca2 <- prcomp(data, center = FALSE, scale. = FALSE)
summary(pca2)
ggbiplot(pca2)
pca1 <- prcomp(data, center = TRUE, scale. = TRUE)
summary(pca1)
ggbiplot(pca1, labels = rownames(data))
country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(pca1, ellipse=TRUE, labels = rownames(data), groups = country)

