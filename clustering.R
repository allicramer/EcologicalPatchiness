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
plot(ss)

#same heatmap, but cluster using k-medioids instead of cutree
pam = cluster::pam(ratiom, k=5)
png("./graphics/heatmap_kmedioids.png", width = 7, height = 6.5, units = "in", res=300)
Heatmap(ratiom, name="log ratio", col=col_fun, border = T,
        row_split = pam$clustering,
        row_title = c("Nomadic", "Semi-\nnomadic", "Pursuit", "Triple-\nMarginal", "Wind-\nfall"),
        row_title_gp=gpar(fontsize=8, lwd=-0, fill="light grey"),
        row_title_side="right",
        cluster_columns = F, right_annotation = ha,
        row_names_gp=gpar(fontsize = 9), column_names_gp=gpar(fontsize = 10),
        row_dend_width = unit(2.5, "cm"),
        row_names_max_width = max_text_width(rownames(ratiom), gp = gpar(fontsize = 9)))
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
