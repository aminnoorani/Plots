library(ArchR)
library(Seurat)

seGS <- readRDS("~/Downloads/Transfers/seGS2.rds")
#Get Gene Scores
seGS <- getGroupSE(ArchRProj=Mouse, useMatrix="GeneScoreMatrix", groupBy="Clusters2", scaleTo = 10000)
rownames(seGS) <- rowData(seGS)$name

#Get scRNA

seRNA <- readRDS("~/seRNA_Seurat.rds")
matRNA <- ArchR:::.groupMeans(mat=seRNA@assays$RNA@data, groups=seRNA$cell_type1, sparse=TRUE)

#Get Same Organized Matrix
geneIntersect <- intersect(rownames(matRNA), rowData(seGS)$name)
matRNA <- matRNA[geneIntersect, ]
matGS <- assay(seGS)[geneIntersect, ]

#Normalize
matRNA <- t(t(matRNA) / colSums(matRNA)) * 10000
matGS <- t(t(matGS) / colSums(matGS)) * 10000

#Compute top variable genes
varGS <- head(order(matrixStats::rowVars(log2(matGS+1)), decreasing=TRUE), 2500)
varRNA <- head(order(matrixStats::rowVars(log2(matRNA+1)), decreasing=TRUE), 2500)
varGenes <- unique(sort(c(varGS, varRNA)))

#Correlations
corDF <- lapply(seq_len(ncol(matRNA)), function(x){
  df <- data.frame(cor(matGS[varGenes, ], matRNA[varGenes, x], method = "spearman"))
  df
}) %>% Reduce("cbind", .)
colnames(corDF) <- colnames(matRNA)
rownames(corDF) <- rownames(corDF)

corDF[,1] <- NULL
target <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36","C37","C38","C39","C40","C41")
corDF_orderd <- corDF[as.numeric(match(target, rownames(corDF))),]


#Row Normalize
scaledDF <- ArchR:::.rowScale(as.matrix(corDF))
corDF$celltype <- rownames(corDF)
melted <- melt(corDF)

pdf("Heatmap.pdf")
pheatmap::pheatmap(melted, color =colorRampPalette(c("blue", "red"))(100),cluster_rows = T, border_color = "black")
dev.off()
