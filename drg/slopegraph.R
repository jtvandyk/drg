library(ggplot2)
library(ggthemes)
library(devtools)
library(RCurl)
library(plyr)

# Slopegraph for new DRG to JvD classification
df <- build_slopegraph(slope_new, x="type", y="seed.order", group="name", method="spaced", min.space=0.05)
df <- transform(df, x=factor(x, levels=c("old","new"), 
                             labels=c("1", "2")), y=round(y))
plot_slopegraph(df) + labs(title="DRG Reclassification") +
  theme_tufte(base_size=16, ticks=F) + theme(axis.title=element_blank())

# Slopegraph for old DRG to JvD classification
df1 <- build_slopegraph(slope_old, x="type", y="seed.order", group="name", method="spaced", min.space=0.05)
df1 <- transform(df1, x=factor(x, levels=c("old","new"), 
                             labels=c("1", "2")), y=round(y))
plot_slopegraph(df1) + labs(title="DRG Reclassification") +
  theme_tufte(base_size=16, ticks=F) + theme(axis.title=element_blank())