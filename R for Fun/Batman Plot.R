install.packages("Rcpp")

require(devtools)
install_github('rCharts', 'ramnathv')


require(ggplot2)
require(dplyr)
require(rCharts)
library(rvest)
library(tm)
f1u <- function(x) {ifelse ((abs(x) >  3 & abs(x) <= 7), 3*sqrt(1-(x/7)^2), 0)}
f1d <- function(x) {ifelse ((abs(x) >= 4 & abs(x) <= 7), -3*sqrt(1-(x/7)^2), 0)}
f2u <- function(x) {ifelse ((abs(x) > 0.50 & abs(x) < 0.75),  3*abs(x)+0.75, 0)}
f2d <- function(x) {ifelse ((abs(x) > -4 & abs(x) < 4), abs(x/2)-(3*sqrt(33)-7)*x^2/112-3 + sqrt(1-(abs(abs(x)-2)-1)^2), 0)}
f3u <- function(x) {ifelse ((x > -0.5 & x < 0.5), 2.25, 0)}
f4u <- function(x) {ifelse ((abs(x) >  1 & abs(x) <= 3), 6 * sqrt(10)/7 + (1.5 - 0.5 * abs(x)) * sqrt(abs(abs(x)-1)/(abs(x)-1)) - 6 * sqrt(10) * sqrt(4-(abs(x)-1)^2)/14, 0)}
f5u <- function(x) {ifelse ((abs(x) >= 0.75 & abs(x) <= 1), 9-8*abs(x), 0)}
fu <- function (x) f1u(x)+f2u(x)+f3u(x)+f4u(x)+f5u(x)
fd <- function (x) f1d(x)+f2d(x)
batman <- function(r,x) {ifelse(r%%2==0, fu(x), fd(x))}
data.frame(x=seq(from=-7, to=7, by=0.125)) %>%
  mutate(y=batman(row_number(), x)) -> df
html("https://en.wikipedia.org/wiki/Batman") %>%
  html_nodes("#bodyContent")  %>%
  html_text() %>%
  VectorSource() %>%
  Corpus() %>%
  tm_map(tolower) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%  
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c(stopwords(kind = "en"), "batman", "batmans")) %>%
  DocumentTermMatrix() %>%
  as.matrix() %>%
  colSums() %>%
  sort(decreasing=TRUE) %>%
  head(n=nrow(df)) %>%
  attr("names") -> df$word
m1=mPlot(x = "x",  y = "y",  data = df,  type = "Line")
m1$set(pointSize = 5,
       lineColors = c('black', 'black'),
       width = 900,
       height = 500,
       hoverCallback = "#! function(index, options, content)
       { var row = options.data[index]
       return '<b>' + row.word + '</b>'} !#",
       lineWidth = 2,
       grid=FALSE,
       axes=FALSE)
m1
m1$save('Batman.html', standalone = TRUE)