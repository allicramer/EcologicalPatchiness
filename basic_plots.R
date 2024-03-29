library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(rgl)

patchy <- read.csv("data/PatchinessData_processed.csv", stringsAsFactors=F)

patchy <- patchy %>%
  arrange(consumer_body_mass) %>%
  mutate(size.ratio = resource_body_size / consumer_body_size,
         mass.ratio = resource_body_mass / consumer_body_mass,
         label = 1:n(),
         mean.field.sum = (Fr_dir < 0.1) + (Str < 0.1) + (Le < 0.1),
         all.ratio.gt.1 = (Fr_dir > 1) & (Str > 1) & (Le > 1),
         any.ratio.gt.10 = (Fr_dir > 10) | (Str > 10) | (Le > 10),
         any.ratio.lt.0.1 = (Fr_dir < 0.1) | (Str < 0.1) | (Le < 0.1))

patchy %>%
  select(consumer_resource_pair, Fr_dir, Str, Le, mean.field.sum) %>%
  mutate(consumer_resource_pair = substr(consumer_resource_pair, 1, 30)) %>%
  arrange(desc(mean.field.sum))

sum(patchy$all.ratio.gt.1)
sum(patchy$any.ratio.gt.10)
sum(patchy$any.ratio.lt.0.1)

classifications <- read.csv("data/Patchiness_classifications.csv")
classifications %>% count(ecosystem2)
classifications %>% count(consumer_type2)

Fr_breaks = 10^seq(-8, 10, by=2)
Str_breaks = 10^seq(-8, 4, by=2)
Le_breaks = 10^seq(-8, 6, by=2)

p1 <- ggplot(patchy) +
  geom_vline(xintercept=1, size=0.25, alpha=0.5, linetype=3) + 
  geom_hline(yintercept=1, size=0.25, alpha=0.5, linetype=3) +
  geom_segment(aes(x=Fr_diff, y=Str, xend=Fr_dir, yend=Str), alpha=0.3, size=0.25) +
  geom_point(aes(x=Fr_diff, y=Str), col="#cccccc") + 
  geom_point(aes(x=Fr_dir, y=Str)) + 
  geom_text_repel(aes(x=Fr_dir, y=Str, label=label), alpha=0.7, box.padding=0.1,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2.2) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-8, 10^9),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(breaks=Str_breaks, limits=c(10^-7, 10^5),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
p1
ggsave("graphics/Fr-Str.png", p1, w=4, h=4)


p2 <- ggplot(patchy) +
  geom_vline(xintercept=1, size=0.25, alpha=0.5, linetype=3) + 
  geom_hline(yintercept=1, size=0.25, alpha=0.5, linetype=3) +
  geom_segment(aes(x=Fr_diff, y=Le, xend=Fr_dir, yend=Le), alpha=0.3, size=0.25) +
  geom_point(aes(x=Fr_diff, y=Le), col="#cccccc") + 
  geom_point(aes(x=Fr_dir, y=Le)) + 
  geom_text_repel(aes(x=Fr_dir, y=Le, label=label), alpha=0.7, box.padding=0.1,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2.2) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-8, 10^9), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
p2
ggsave("graphics/Fr-Le.png", p2, w=4, h=4)

p3 <- ggplot(patchy) +
  geom_vline(xintercept=1, size=0.25, alpha=0.5, linetype=3) + 
  geom_hline(yintercept=1, size=0.25, alpha=0.5, linetype=3) +
  geom_point(aes(x=Str, y=Le)) + 
  geom_text_repel(aes(x=Str, y=Le, label=label), alpha=0.7, box.padding=0.1,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2.2) +
  scale_x_log10(breaks=Str_breaks, limits=c(10^-7, 10^5),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_equal() +
  theme_classic()
p3
ggsave("graphics/Str-Le.png", p3, w=3.25, h=3.25)



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

cor.test(log10(patchy$Str), log10(patchy$Fr_dir))
cor.test(log10(patchy$Fr_dir), log10(patchy$Le))
cor.test(log10(patchy$Le), log10(patchy$Str))



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


minute = 60
hour = minute*60
day = hour*24
month = day*30
year = day*365
decade = year*10
century = year*100
millenium = century * 10
h = c(minute, hour, day, month, year, decade, century, millenium)
xmin <- -4
xmax <- 6
ymin <- 1
ymax <- 11
hline_data = data.frame(x=0.5e-4, h=h, 
    label=c("Minute", "Hour", "Day", "Month", "Year", "Decade", "Century", "Millenium"))

png("graphics/patch_scales.png", w=5.5, h=4.5, units="in", res=300)
p_patch_scales <- ggplot(patchy) +
  geom_hline(yintercept=h, linetype=3, color="dark grey") +
  geom_text(aes(x=x, y=h*1.5, label=label), data=hline_data, hjust="left",
            color="dark grey") +
  geom_point(aes(x=patch_length_scale, y=patch_duration)) +
  scale_x_log10("Patch separation scale (m)", breaks=10^(xmin:xmax),
              labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10("Patch duration scale (s)", breaks=10^(ymin:ymax),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  geom_text_repel(aes(x=patch_length_scale, y=patch_duration, label=label), 
                  min.segment.length = 0.3, alpha=0.5) +
  annotation_logticks(alpha=0.5) +
  theme_classic() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.text=element_text(size=10))
p_patch_scales
dev.off()

select(patchy, label, consumer_resource_pair, patch_length_scale, patch_duration) %>%
  write.csv("data/patch_scales.csv")



patch.items <- read.csv("data/patch_items.csv")
patchy <- left_join(patchy, patch.items)

png("graphics/body_sizes.png", w=5.5, h=4.5, units="in", res=300)
p_body_sizes <- ggplot(filter(patchy, items_in_patch!="Continuous")) +
  geom_point(aes(x=resource_body_size, y=consumer_body_size)) +
  scale_x_log10("Resource body size (m)", breaks=10^(-4:3),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10("Consumer body size (m)", breaks=10^(-2:3),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  geom_text_repel(aes(x=resource_body_size, y=consumer_body_size, label=label), 
                  min.segment.length = 0.3, alpha=0.5) +
  annotation_logticks(alpha=0.5) +
  theme_classic() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.text=element_text(size=10))
p_body_sizes
dev.off()

png("graphics/Figure2.png", w=11, h=4.5, units="in", res=300)
  gridExtra::grid.arrange(p_body_sizes + ggtitle("(a)"), 
                          p_patch_scales + ggtitle("(b)"), nrow=1)
dev.off()

