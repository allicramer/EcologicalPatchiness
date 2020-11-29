library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(rgl)

patchy <- read.csv("data/PatchinessData_processed.csv", stringsAsFactors=F)

#############

# body size and body size log ratios

patchy_bm <- patchy %>%
  mutate(size.ratio = log10(resource_body_size / consumer_body_size),
         mass.ratio = log10(resource_body_mass / consumer_body_mass),
         consumer.logbm = log10(consumer_body_mass),
         resource.logbm = log10(resource_body_mass),
         Fr_dir=log10(Fr_dir), Str=log10(Str), Le=log10(Le))
pairs(select(patchy_bm, Fr_dir, Str, Le, mass.ratio, consumer.logbm, resource.logbm))

# dendrogram, cluster analysis

ratios <- patchy %>%
  select(Fr_dir, Str, Le) %>% 
  transmute(Fr_dir=log10(Fr_dir), Str=log10(Str), Le=log10(Le))
rownames(ratios)=paste(1:nrow(patchy), patchy$consumer_resource_pair)

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


#fancier heatmap plus dendrogram

# install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
# a very complete manual: https://jokergoo.github.io/ComplexHeatmap-reference/book/

library(ComplexHeatmap)
library(circlize)
library(scales)

#classification variables
class=read.csv("./data/Patchiness_classifications.csv")
#check representation
table(class$consumer_type2, class$patch_movement, class$ecosystem2)

#log ratios
ratiom=as.matrix(ratios)
#colors for ratios (same as ggplot2::scale_fill_gradient2())
col_fun = colorRamp2(c(-10, 0, 10), c(muted("red"), "white", muted("blue")))
#column annotations
annot_df = data.frame(system=class$ecosystem2, 
                      consumer_type=class$consumer_type2, 
                      patch_movement=class$patch_movement,
                      mass_ratio=patchy_bm$mass.ratio)
#colors for column annotations
annot_col = list(system = c("marine"="darkblue", "terrestrial"="green3", "freshwater"="lightblue"),
                 consumer_type = c("predator"="black", "herbivore"="gray50", "detritovore"="gray90"),
                 patch_movement = c("active"="purple","passive"="lightpink","stationary"="orange"),
                 mass_ratio = colorRamp2(c(min(annot_df$mass_ratio),0,max(annot_df$mass_ratio)), 
                                         c("tomato", "white", "cornflowerblue")) )
#create object for annotations
ha <- HeatmapAnnotation(df=annot_df, col = annot_col, which="row",
                        annotation_name_gp=gpar(fontsize = 10))
#generate heatmap
Heatmap(ratiom, name="log ratio", col=col_fun, border = T, row_split = 5, 
        cluster_columns = F, right_annotation = ha,
        row_title = "Consumer-resource pair",
        row_names_gp=gpar(fontsize = 9), column_names_gp=gpar(fontsize = 10),
        row_dend_width = unit(2.5, "cm"),
        row_names_max_width = max_text_width(rownames(ratiom), gp = gpar(fontsize = 9)))

#same heatmap, but cluster using k-means instead of cutree
Heatmap(ratiom, name="log ratio", col=col_fun, border = T, row_km = 5,
        cluster_columns = F, right_annotation = ha,
        row_title = "Consumer-resource pair (kmeans clusters)",
        row_names_gp=gpar(fontsize = 9), column_names_gp=gpar(fontsize = 10),
        row_dend_width = unit(2.5, "cm"),
        row_names_max_width = max_text_width(rownames(ratiom), gp = gpar(fontsize = 9)))

