# dendrogram/cluster/heatmap plot
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(rgl)
# install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
# a very complete manual: https://jokergoo.github.io/ComplexHeatmap-reference/book/
library(ComplexHeatmap)
library(circlize)
library(scales)

patchy <- read.csv("data/PatchinessData_processed.csv", stringsAsFactors=F) %>%
  arrange(consumer_body_mass) %>%
  mutate(name = paste(str_pad(1:n(), 3, "right"), consumer_resource_pair))
#classification variables
class=read.csv("./data/Patchiness_classifications.csv") %>%
  arrange(consumer_body_mass)
#see representation
table(class$consumer_type2, class$patch_movement, class$ecosystem2)

#############

# body size and body size log ratios
patchy_bm <- patchy %>%
  mutate(size.ratio = log10(resource_body_size / consumer_body_size),
         mass.ratio = log10(resource_body_mass / consumer_body_mass),
         consumer.logbm = log10(consumer_body_mass),
         resource.logbm = log10(resource_body_mass),
         Fr=log10(Fr_dir), Str=log10(Str), Le=log10(Le))
#pairs(select(patchy_bm, Fr, Str, Le, mass.ratio, consumer.logbm, resource.logbm))

# log ratios
ratios <- patchy %>%
  select(Fr_dir, Str, Le) %>% 
  transmute(Fr=log10(Fr_dir), Str=log10(Str), Le=log10(Le))
#rownames(ratios)=paste(1:nrow(patchy), class$name)
rownames(ratios) = patchy$name
ratiom=as.matrix(ratios)

#colors for ratios 
col_fun = colorRamp2(c(-10, 0, 10), c("red4", "white", "navy"))

#column annotations
annot_df = data.frame(system=class$ecosystem2, 
                      consumer_type=class$consumer_type2, 
                      patch_movement=class$patch_movement,
                      mass_ratio=patchy_bm$mass.ratio)
colnames(annot_df) = c("System", "Consumer Type", "Patch Movement", "log R/C Mass")

#colors for column annotations
annot_col = list(system = c("marine"="dodgerblue3", "terrestrial"="palegreen3", "freshwater"="lightskyblue"),
                 consumer_type = c("predator"="#E6550D", "herbivore"="#FDAE6B", "detritovore"="#FEE6CE"),
                 patch_movement = c("active"="gold","passive"="#FFFFB3","stationary"="maroon"),
                   mass_ratio = colorRamp2(c(min(annot_df$`log R/C Mass`),0,max(annot_df$`log R/C Mass`)), 
                                         c("mediumorchid4", "white", "lightseagreen")) )
names(annot_col) = c("System", "Consumer Type", "Patch Movement", "log R/C Mass")

#create object for annotations
ha <- HeatmapAnnotation(df=annot_df, col = annot_col, which="row",
                        annotation_name_gp=gpar(fontsize = 10))

#generate heatmap
# pdf("./graphics/heatmap.pdf", width = 6.25, height = 5.5)
png("./graphics/heatmap_cuttree.png", width = 6.25, height = 5.5, units = "in", res=300)
Heatmap(ratiom, name="log Ratio", col=col_fun, border = T, 
        row_split = 5,
        cluster_columns = F, right_annotation = ha,
        row_title = "Consumer-resource pair",
        row_names_gp=gpar(fontsize = 9), column_names_gp=gpar(fontsize = 10),
        row_dend_width = unit(2.5, "cm"),
        row_names_max_width = max_text_width(rownames(ratiom), gp = gpar(fontsize = 9)))
dev.off()

# Elbow plot to inform # of clusters
ss <- rep(0, 10)
for (i in 1:10) {
  km <- cluster::pam(ratiom, i)
  ss[i] <- km$objective[2]
}

png("./graphics/elbow.png")
  plot(ss, xlab="Number of clusters", ylab="Sum of dissimilarities")
dev.off()

#same heatmap, but cluster using k-medioids instead of cutree
pam = cluster::pam(ratiom, k=5)
png("./graphics/heatmap_kmedioids.png", width = 7, height = 7, units = "in", res=300)
Heatmap(ratiom, name="log ratio", col=col_fun, border = T,
        row_split = pam$clustering,
        row_title = c("Nomadic", "Semi-\nnomadic", "Pursuit", "Triple-\nMarginal", "Wind-\nfall"),
        row_title_gp=gpar(fontsize=8, fontface="bold", lineheight=0.8, lwd=NA, fill="#eeeeee"),
        row_title_side="right",
        cluster_columns = F, right_annotation = ha,
        row_names_gp=gpar(fontsize = 9), column_names_gp=gpar(fontsize = 10),
        row_dend_width = unit(2.5, "cm"),
        row_names_max_width = max_text_width(rownames(ratiom), gp = gpar(fontsize = 9)))
dev.off()

cluster.labels <- data.frame(cluster.id=1:5, 
   cluster.label = factor(c("Windfall", "Triple-marginal", "Nomadic", "Semi-nomadic", "Pursuit"),
     levels=c("Nomadic", "Semi-nomadic", "Pursuit", "Triple-marginal", "Windfall")))
patchy <- patchy %>%
  mutate(label = 1:n(), 
         cluster.id = pam$clustering) %>% 
  left_join(cluster.labels)

p1 <- ggplot(patchy) +
  geom_vline(xintercept=1, size=0.25, alpha=0.5, linetype=3) + 
  geom_hline(yintercept=1, size=0.25, alpha=0.5, linetype=3) +
  geom_segment(aes(x=Fr_diff, y=Str, xend=Fr_dir, yend=Str), alpha=0.5, size=0.25) +
  geom_point(aes(x=Fr_diff, y=Str, shape=cluster.label, fill=cluster.label, color=cluster.label),
             alpha=0.3) + 
  geom_point(aes(x=Fr_dir, y=Str, shape=cluster.label, fill=cluster.label, color=cluster.label)) + 
  geom_text_repel(aes(x=Fr_dir, y=Str, label=label), alpha=0.7, box.padding=0.1,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2.2) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-8, 10^9),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_log10(breaks=Str_breaks, limits=c(10^-7, 10^5),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_shape_manual("Cluster", values = 21:25, guide=F) +
  scale_color_discrete("Cluster", guide=F) +
  scale_fill_discrete("Cluster", guide=F) +
  coord_equal() +
  theme_classic()
p1
ggsave("graphics/Fr-Str_clustered.png", p1, h=4, w=4)


p2 <- ggplot(patchy) +
  geom_vline(xintercept=1, size=0.25, alpha=0.5, linetype=3) + 
  geom_hline(yintercept=1, size=0.25, alpha=0.5, linetype=3) +
  geom_segment(aes(x=Fr_diff, y=Le, xend=Fr_dir, yend=Le), alpha=0.3, size=0.25) +
  geom_point(aes(x=Fr_diff, y=Le, shape=cluster.label, color=cluster.label, fill=cluster.label),
             alpha=0.5) + 
  geom_point(aes(x=Fr_dir, y=Le, shape=cluster.label, color=cluster.label, fill=cluster.label)) + 
  geom_text_repel(aes(x=Fr_dir, y=Le, label=label), alpha=0.7, box.padding=0.1,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2.2) +
  scale_x_log10("Fr", breaks=Fr_breaks, limits=c(10^-8, 10^9), 
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_shape_manual("Cluster", values = 21:25, guide=F) +
  scale_color_discrete("Cluster", guide=F) +
  scale_fill_discrete("Cluster", guide=F) +
  coord_equal() +
  theme_classic()
p2
ggsave("graphics/Fr-Le_clustered.png", p2, h=4, w=4)

p3 <- ggplot(patchy) +
  geom_vline(xintercept=1, size=0.25, alpha=0.5, linetype=3) + 
  geom_hline(yintercept=1, size=0.25, alpha=0.5, linetype=3) +
  geom_point(aes(x=Str, y=Le, shape=cluster.label, color=cluster.label, fill=cluster.label)) + 
  geom_text_repel(aes(x=Str, y=Le, label=label), alpha=0.7, box.padding=0.1,
                  force=5, min.segment.length=0.2, segment.alpha=0.5, segment.size=0.25, size=2.2) +
  scale_x_log10(breaks=Str_breaks, limits=c(10^-7, 10^5),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks=Le_breaks,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_shape_manual("Cluster", values = 21:25) +
  scale_color_discrete("Cluster") +
  scale_fill_discrete("Cluster") +
  coord_equal() +
  theme_classic()
p3
ggsave("graphics/Str-Le_clustered.png", p3, h=4, w=5)

xmin <- -6
xmax <- 6
ymin <- -2
ymax <- 12
pal <- scales::hue_pal()(3)
shape <- 21:25
png("graphics/patch_access.png", w=7, h=6, units="in", res=300)
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
  colors <- rep(pal[1], length(x0))
  colors[slopes > atan2(1, 1)] <- pal[2]
  colors[slopes > atan2(2, 1)] <- pal[3]
  shapes <- shape[patchy$cluster.id]
  segments(x0, y0, x1, y1, col=colors)
  points(x1, y1, pch=shapes, bg="black")
  text(y1 ~ x1, labels=patchy$label, pos=3, offset=0.5, cex=0.6)
  points(log10(turning_interval) ~ log10(turning_interval*speed), data=patchy, pch=shapes)
  axis(1, at=xmin:xmax, labels=parse(text=paste0("10^", xmin:xmax)))
  axis(2, at=seq(ymin, ymax, by=2), labels=parse(text=paste0("10^", seq(ymin, ymax, by=2))))
  mtext("Length scale (m)", 1, 3)
  mtext("Time scale (s)", 2, 3)
  legend("topleft", legend=c("Windfall", "Triple-marginal", "Nomadic", "Semi-nomadic", "Pursuit",
                             "Slope < 1", "1 < Slope < 2", "Slope > 2"),
         pch=c(21:25, NA, NA, NA), lty=c(rep(NA, 5), 1, 1, 1), col=c(rep("black", 5), pal))
dev.off()


#basic dendrogram
clust=hclust(dist(ratios))
plot(clust)

#some alternate clustering algorithms
# library(cluster)
# clust2=diana(ratios)
# plot(clust2)
# clust3=agnes(ratios)
# plot(clust3)

#plot clusters in 3d
groups.5 = cutree(clust,5)
with(patchy, plot3d(log10(Fr_dir), log10(Str), log10(Le), type="h"))
with(patchy, points3d(log10(Fr_dir), log10(Str), log10(Le), size=10, color=groups.5))
planes3d(1, 0, 0, color="grey", alpha=0.5)
planes3d(0, 1, 0, color="grey", alpha=0.5)
planes3d(0, 0, 1, color="grey", alpha=0.5)
# rgl.snapshot("./graphics/cluster3d.png")
