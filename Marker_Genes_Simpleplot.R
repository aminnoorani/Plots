GS <- readRDS("~/Downloads/Transfers/markerList.rds")

#GS <- readRDS("~/Downloads/Transfers/markerList_C41.rds")

master_top_genes <- data.frame(cluster = character(), name = character(), FDR = numeric(), stringsAsFactors = FALSE)

# Loop over the clusters
pdf("MarkerGenes_Clusters_UglyPlot_Top10.pdf",height = 4, width =4)
for (i in 1:36) {
  # Get the data for the current cluster
  cluster_data <- GS[[paste0("C", i)]]
  cluster_data <- data.frame(cluster_data)
  
  # Extract the gene name and FDR columns into a data frame
  fdr_gene_matrix <- data.frame(cluster_data[, c("name", "FDR")], row.names = NULL)
  
  # Order the data frame by FDR values
  ordered_data <- fdr_gene_matrix[order(fdr_gene_matrix$FDR), ]
  
  # Select the top 5 genes with the lowest FDR values
  top_5_genes <- head(ordered_data, 5)
  
  # Set up custom colors
  dot_color <- "#8c564b"  # Brown color for dots
  text_color <- "#000000" # Black color for gene labels
  
  # Create a ranking plot to highlight the top 5 genes
  plot(1:5, seq_along(top_5_genes$name), 
       pch = 20,
       xlim = c(0, 6),
       ylim = c(0.5, 5.5),
       main = paste0("Top 5 Genes - Cluster ", i),
       xlab = "",
       ylab = "Genes",
       yaxt = "n",
       col = dot_color,
       bg = dot_color,
       cex = 0.5,
       cex.axis = 0.8,
       cex.lab = 1.2,
       axes = FALSE,
       frame.plot = TRUE)
  
  # Add horizontal lines for ranking
  abline(h = 1:5, col = "lightgray")
  
  # Add gene names as text labels
  text(0.5, seq_along(top_5_genes$name), labels = top_5_genes$name, pos = 4, col = text_color, cex = 1.2)
  
  # Add ranking numbers as text labels
  text(-0.3, seq_along(top_5_genes$name), labels = paste0(seq_along(top_5_genes$name), "."), pos = 4, col = text_color, cex = 1.2)
  master_top_genes <- rbind(master_top_genes, cbind(cluster = paste0("C", i), top_5_genes))
  
}
dev.off()
write.csv(master_top_genes, "MasterTopGenes_Top10.csv", row.names = FALSE)

