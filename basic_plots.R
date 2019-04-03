library(ggplot2)
library(dplyr)
library(tidyr)
library(rgl)

patchy <- read.csv("data/PatchinessData_processed.csv", stringsAsFactors=F)
# head(patchy)
# 
# ratios <- patchy %>%
#   select(consumer_resource_pair, ecosystem, interaction_system, consumer_type, Fr_diff, Fr_dir, Str, Le)

Fr_breaks = 10^(-8:9)
Str_breaks = 10^(-7:4)
Le_breaks = 10^(-8:5)

ggplot(patchy) +
  geom_point(aes(x=Fr_dir, y=Str)) + 
  geom_point(aes(x=Fr_diff, y=Str), pch=1) + 
  geom_segment(aes(x=Fr_diff, y=Str, xend=Fr_dir, yend=Str), linetype=3) +
  geom_text(aes(x=Fr_dir, y=Str, label=consumer_resource_pair), hjust=-0.1, size=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-7, 10^11)) + 
  scale_y_log10(breaks=Str_breaks)

ggplot(patchy) +
  geom_point(aes(x=Fr_dir, y=Le)) + 
  geom_point(aes(x=Fr_diff, y=Le), pch=1) + 
  geom_segment(aes(x=Fr_diff, y=Le, xend=Fr_dir, yend=Le), linetype=3) +
  geom_text(aes(x=Fr_dir, y=Le, label=consumer_resource_pair), hjust=-0.1, size=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-7, 10^13)) +
  scale_y_log10(breaks=Le_breaks)

ggplot(patchy) +
  geom_point(aes(x=Str, y=Le)) + 
  geom_text(aes(x=Str, y=Le, label=consumer_resource_pair), hjust=-0.1, size=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10(breaks=Str_breaks) + scale_y_log10(breaks=Le_breaks)


with(patchy, plot3d(log10(Fr_dir), log10(Str), log10(Le), type="h"))
with(patchy, points3d(log10(Fr_dir), log10(Str), log10(Le), size=10))
planes3d(1, 0, 0, color="grey", alpha=0.5)
planes3d(0, 1, 0, color="grey", alpha=0.5)
planes3d(0, 0, 1, color="grey", alpha=0.5)
