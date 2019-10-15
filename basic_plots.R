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
  geom_point(aes(x=Fr_dir, y=Str)) + 
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
  theme_classic()
ggsave("graphics/Fr-Str.png", w=9, h=9)

ggplot(patchy) +
  geom_point(aes(x=Fr_dir, y=Le)) + 
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
  theme_classic()
ggsave("graphics/Fr-Le.png", w=9, h=9)

ggplot(patchy) +
  geom_point(aes(x=Str, y=Le)) + 
  geom_text_repel(aes(x=Str, y=Le, label=consumer_resource_pair), 
                  color="dark grey", hjust=-0.1, size=3) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1) +
  scale_x_log10(breaks=Str_breaks, limits=c(10^-6, 10^6),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
ggsave("graphics/Str-Le.png", w=9, h=9)



n <- nrow(patchy)
open3d()
light3d(45, 45)

with(patchy, plot3d(log10(Fr_dir), log10(Str), log10(Le), xlab="", ylab="", zlab="",
                    type="s", col="red", radius=0.1, box=F))
with(patchy, points3d(rep(log10(min(Fr_dir))-1, n), log10(Str), log10(Le), size=3, col="dark grey"))
with(patchy, points3d(log10(Fr_dir), rep(log10(max(Str))+1, n), log10(Le), size=3, col="dark grey"))
with(patchy, points3d(log10(Fr_dir), log10(Str), rep(log10(min(Le))-1, n), size=3, col="dark grey"))
mtext3d(expression(log10[10](Fr)), "x++", line=2)
mtext3d(expression(log[10](Str)), "y+-", line=2)
mtext3d(expression(log[10](Le)), "z++", line=2)
grid3d("x")
grid3d("y+")
grid3d("z")

planes3d(1, 0, 0, alpha=0.4, depth_mask=F)
planes3d(0, 1, 0, alpha=0.4, depth_mask=F)
planes3d(0, 0, 1, alpha=0.4, depth_mask=F)
abclines3d(x=0, y=0, z=0, a=0, b=1, c=0, alpha=0.8)
abclines3d(x=0, y=0, z=0, a=1, b=0, c=0, alpha=0.8)
abclines3d(x=0, y=0, z=0, a=0, b=0, c=1, alpha=0.8)
snapshot3d("graphics/Fr-Str-Le-3D.png")

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


xmin <- -6
xmax <- 6
ymin <- -4
ymax <- 12
pdf("graphics/patch_access.pdf")
plot.new()
plot.window(xlim=c(xmin, xmax), ylim=c(ymin, ymax), xaxs="i", yaxs="i")
for (a in seq(-12, 20, by=2)) {
  abline(a, 1, col="#00000033")
  abline(a, 2, col="#00000033", lty=3)
}
with(patchy, 
     segments(log10(turning_interval*speed), log10(turning_interval), 
              log10(patch_length_scale), log10(patch_duration),
              col="#00000077"))
points(log10(patch_duration) ~ log10(patch_length_scale), data=patchy, pch=16)
text(log10(patch_duration) ~ log10(patch_length_scale), data=patchy, labels=1:nrow(patchy),
     pos=3, offset=0.5)
points(log10(turning_interval) ~ log10(turning_interval*speed), data=patchy)
axis(1, at=xmin:xmax, labels=parse(text=paste0("10^", xmin:xmax)))
axis(2, at=seq(ymin, ymax, by=2), labels=parse(text=paste0("10^", seq(ymin, ymax, by=2))))
mtext("Patch separation scale (m)", 1, 3)
mtext("Patch duration scale (s)", 2, 3)
dev.off()
