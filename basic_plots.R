library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(rgl)

patchy <- read.csv("data/PatchinessData_processed.csv", stringsAsFactors=F)

patchy <- patchy %>%
  mutate(size.ratio = resource_body_size / consumer_body_size,
         mass.ratio = resource_body_mass / consumer_body_mass,
         label = 1:n(),
         mean.field.sum = (Fr_dir < 1) + (Str < 1) + (Le < 1),
         mean.field.ok = Fr_dir < 1 & Str < 1 & Le < 1)

patchy %>%
  select(consumer_resource_pair, Fr_dir, Str, Le, mean.field.sum) %>%
  mutate(consumer_resource_pair = substr(consumer_resource_pair, 1, 30)) %>%
  arrange(desc(mean.field.sum))

Fr_breaks = 10^(-8:9)
Str_breaks = 10^(-7:6)
Le_breaks = 10^(-8:5)

p1 <- ggplot(patchy) +
  geom_point(aes(x=Fr_dir, y=Str)) + 
  geom_point(aes(x=Fr_diff, y=Str), pch=1) + 
  geom_segment(aes(x=Fr_diff, y=Str, xend=Fr_dir, yend=Str), alpha=0.5, size=0.25) +
  geom_text_repel(aes(x=Fr_dir, y=Str, label=label), alpha=0.5, box.padding=0.3,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2) +
  geom_vline(xintercept=1, size=0.25) + geom_hline(yintercept=1, size=0.25) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-8, 10^9),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(breaks=Str_breaks, limits=c(10^-7, 10^5),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
p1
ggsave("graphics/Fr-Str.png", p1, w=5, h=5)


p2 <- ggplot(patchy) +
  geom_point(aes(x=Fr_dir, y=Le)) + 
  geom_point(aes(x=Fr_diff, y=Le), pch=1) + 
  geom_segment(aes(x=Fr_diff, y=Le, xend=Fr_dir, yend=Le), alpha=0.5, size=0.25) +
  geom_text_repel(aes(x=Fr_dir, y=Le, label=label), alpha=0.5, box.padding=0.3,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2) +
  geom_vline(xintercept=1, size=0.25) + geom_hline(yintercept=1, size=0.25) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-8, 10^9), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
p2
ggsave("graphics/Fr-Le.png", p2, w=5, h=5)

p3 <- ggplot(patchy) +
  geom_point(aes(x=Str, y=Le)) + 
  geom_text_repel(aes(x=Str, y=Le, label=label), alpha=0.5, box.padding=0.3,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2) +
  geom_vline(xintercept=1, size=0.25) + geom_hline(yintercept=1, size=0.25) +
  scale_x_log10(breaks=Str_breaks, limits=c(10^-7, 10^5),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
p3
ggsave("graphics/Str-Le.png", w=4.5, h=4)



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

summary(lm(log10(Str) ~ log10(Fr_dir), patchy))
summary(lm(log10(Fr_dir) ~ log10(Le), patchy))
summary(lm(log10(Le) ~ log10(Str), patchy))


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
png("graphics/patch_access.png", w=6, h=6, units="in", res=300)
  plot.new()
  plot.window(xlim=c(xmin, xmax), ylim=c(ymin, ymax), xaxs="i", yaxs="i")
  # Draw grid of sloped lines
  for (a in seq(-12, 20, by=2)) {
    abline(a, 1, col="#00000033")
    abline(a, 2, col="#00000033", lty=2)
  }
  x0 <- with(patchy, log10(turning_interval*speed))
  y0 <- log10(patchy$turning_interval)
  x1 <- log10(patchy$patch_length_scale)
  y1 <- log10(patchy$patch_duration)
  slopes <- atan2(y1-y0, x1-x0)
  colors <- rep("#00000077", length(x0))
  colors[slopes < pi/4] <- "red"
  segments(x0, y0, x1, y1, col=colors)
  points(x1, y1, pch=16)
  text(y1 ~ x1, labels=1:nrow(patchy), pos=3, offset=0.5, cex=0.6)
  points(log10(turning_interval) ~ log10(turning_interval*speed), data=patchy)
  axis(1, at=xmin:xmax, labels=parse(text=paste0("10^", xmin:xmax)))
  axis(2, at=seq(ymin, ymax, by=2), labels=parse(text=paste0("10^", seq(ymin, ymax, by=2))))
  mtext("Length scale (m)", 1, 3)
  mtext("Time scale (s)", 2, 3)
dev.off()
