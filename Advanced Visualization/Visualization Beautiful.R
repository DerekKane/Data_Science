#################################################################
# Beautiful Visualization
#################################################################

# install.packages("magrittr")

library(magrittr)
library(ggplot2)
opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="violetred4"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())
seq(from=-10, to=10, by = 0.05) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
  geom_point(alpha=.1, shape=20, size=1, color="white") + opt
