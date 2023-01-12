library(ComplexHeatmap)



df <- read.csv("~/Downloads/GeneScoreMatrix_Clusters_labeled.csv")
df <- df %>% distinct(name, .keep_all = TRUE)
rownames(df) <- df$name
df$name <- NULL
df3 <- cor(df,method = "pearson")
sorted <- colnames(df)
sorted <- sort(sorted)
colnamesss <- colnames(df3)
col = as.list(sort(colnamesss))
colours = c("#D51F26","#272E6A","#208A42","#89288F","#F47D2B", "#FEE500","#8A9FD1","#C06CAB","#E6C2DC",
                     "#90D5E4", "#89C75F","#F37B7D","#9983BD","#D24B27","#3BBCA8", "#6E4B9E","#0C727C", 
                     "#7E1416","#D8A767","#3D3D3D","#7DD06F", "#844081", "#688EC1", "#C17E73", "#484125", 
                     "#6CD3A7", "#597873","#7B6FD0", "#CF4A31", "#D0CD47","#722A2D", "#CBC594", "#D19EC4", 
                     "#5A7E36", "#D4477D","#403552", "#76D73C", "#96CED5", "#CE54D1", "#C48736","#FFB300")
names(col) <- colours


ha <- HeatmapAnnotation(
  cell_type = sorted,
  col = col,
  annotation_legend_param = list(cell_type = list(direction = "horizontal", nrow = 9,title_gp = gpar(fontsize = 8), legend_direction = "horizontal", labels_gp = gpar(fontsize = 7))))

p <- Heatmap(df3, name = "Score", column_title = "Correlation Heatmap",top_annotation = ha,
             heatmap_legend_param = list(legend_width = unit(3, "cm")))
draw(p, heatmap_legend_side = "right", annotation_legend_side = "bottom")

