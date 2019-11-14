library(ggplot2)
library(ggrepel)
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
  geom_point(aes(x=Fr_dir, y=Str, fill=log10(Le)), shape=21, color="black", size=3) + 
  geom_point(aes(x=Fr_diff, y=Str), pch=1) + 
  geom_segment(aes(x=Fr_diff, y=Str, xend=Fr_dir, yend=Str), linetype=3) +
  geom_text_repel(aes(x=Fr_dir, y=Str, label=consumer_resource_pair), color="dark grey",
                  direction="y", hjust=-0.1, size=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-7, 10^11),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(breaks=Str_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic() + scale_fill_gradient2()
ggsave("graphics/Fr-Str.png", w=9, h=9)

ggplot(patchy) +
  geom_point(aes(x=Fr_dir, y=Le, fill=log10(Str)), shape=21, color="black", size=3) +
  geom_point(aes(x=Fr_diff, y=Le), pch=1) + 
  geom_segment(aes(x=Fr_diff, y=Le, xend=Fr_dir, yend=Le), linetype=3) +
  geom_text_repel(aes(x=Fr_dir, y=Le, label=consumer_resource_pair), 
                  direction="y", color="dark grey", hjust=-0.1, size=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-7, 10^13), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic() + scale_fill_gradient2()
ggsave("graphics/Fr-Le.png", w=9, h=9)

ggplot(patchy) +
  geom_point(aes(x=Str, y=Le, fill=log10(Fr_dir)), shape=21, color="black", size=3) +
  geom_text_repel(aes(x=Str, y=Le, label=consumer_resource_pair), 
                  color="dark grey", hjust=-0.1, size=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10(breaks=Str_breaks, limits=c(10^-6, 10^6),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic() + scale_fill_gradient2()
ggsave("graphics/Str-Le.png", w=9, h=9)


with(patchy, plot3d(log10(Fr_dir), log10(Str), log10(Le), type="h"))
with(patchy, points3d(log10(Fr_dir), log10(Str), log10(Le), size=10, color=groups.5))
planes3d(1, 0, 0, color="grey", alpha=0.5)
planes3d(0, 1, 0, color="grey", alpha=0.5)
planes3d(0, 0, 1, color="grey", alpha=0.5)

patchy <- patchy %>%
  mutate(size.ratio = resource_body_size / consumer_body_size,
         mass.ratio = resource_body_mass / consumer_body_mass)

ggplot(patchy) +
  geom_point(aes(x=Fr_dir, y=Str, size=mass.ratio)) + 
  geom_point(aes(x=Fr_diff, y=Str, size=mass.ratio), pch=1) + 
  geom_segment(aes(x=Fr_diff, y=Str, xend=Fr_dir, yend=Str), linetype=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-7, 10^13), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
