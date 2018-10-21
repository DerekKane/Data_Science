install.packages('tm.plugin.webmining')


# This is a basic text mining tutorial in R.
# http://michael.hahsler.net/SMU/8331/tutorials/Text_Mining_slides.pdf


library(tm)
library(tm.plugin.webmining)

# The corpus is used to organize unstructured text data in ways that can be manipulated.
# This process will allow for pulling in news sources from web sites.

corpus <- WebCorpus(GoogleNewsSource("Western Illinois University"))
corpus[[1]]