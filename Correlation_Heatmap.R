library(dyplr)





#loading data
pd <- read.csv("~/GeneScoreMatrix_Clusters_labeled.csv", header=TRUE, sep=",", row.names=NULL)

#removing duplicated gene names
pd <- pd %>% distinct(name, .keep_all = TRUE)
rownames(pd) <- pd$name
pd$name <- NULL

#sorting
target <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36","C37","C38","C39","C40","C41")
pd <- pd[,match(target, colnames(pd))]

#Finding string and add another string to separate column
ccc <- ccd %>% 
  mutate(tissue = case_when(
    rownames(ccd) == 'Oligodendrocytes' ~ 'Small_Intestine',
    rownames(ccd) == 'Leukocytes' ~ 'Lung',
    rownames(ccd) == 'Leukocytes.1' ~ 'Heart',
    rownames(ccd) == 'Astrocytes2' ~ 'Brain',
    rownames(ccd) == 'Oligodendrocyte.Precursor.Cells' ~ 'Brain',
    rownames(ccd) == 'Astrocytes' ~ 'Brain',
    rownames(ccd) == 'Pancreatic.Beta.Cells ' ~ 'Pancreas',
    rownames(ccd) == 'Neurons2' ~ 'Brain',
    rownames(ccd) == 'Neurons' ~ 'Brain',
    rownames(ccd) == 'Oligodendrocyte.Precursor.Cells2' ~ 'Brain',
    rownames(ccd) == 'Pneumocytes' ~ 'Lung',
    rownames(ccd) == 'Pneumocytes.1' ~ 'Lung',
    rownames(ccd) == 'T.Cells' ~ 'Spleen',
    rownames(ccd) == 'Collecting.Duct.Cells ' ~ 'Kidney',
    rownames(ccd) == 'Ciliated.Cells' ~ 'Lung',
    rownames(ccd) == 'Ductal.Cells' ~ 'Pancreas',
    rownames(ccd) == 'Cardiac.Muscle.Cells' ~ 'Heart',
    rownames(ccd) == 'Endothelial.Cells' ~ 'Heart',
    rownames(ccd) == 'Fibroblasts' ~ 'Heart',
    rownames(ccd) == 'Smooth.Muscle.Cells' ~ 'Heart',
    rownames(ccd) == 'Fibroblasts2' ~ 'Lung',
    rownames(ccd) == 'Goblet.Cells' ~ 'Colon',
    rownames(ccd) == 'B.Cells' ~ 'Spleen',
    rownames(ccd) == 'Epithelial.Cells4' ~ 'Colon',
    rownames(ccd) == 'Columnar.Epithelial.Cells' ~ 'Small_Intestine',
    rownames(ccd) == 'Paneth.Cells' ~ 'Small_Intestine',
    rownames(ccd) == 'Epithelial.Cells3' ~ 'Small_Intestine',
    rownames(ccd) == 'Epithelial.Cells2' ~ 'Colon',
    rownames(ccd) == 'Erythroblasts' ~ 'Small_Intestine',
    rownames(ccd) == 'Epithelial.Cells' ~'Small_Intestine',
    rownames(ccd) == 'Acinar.Cells' ~ 'Pancreas',
    rownames(ccd) == 'Acinar.Cells2' ~ 'Pancreas',
    rownames(ccd) == 'Hepatocytes' ~ 'Liver',
    rownames(ccd) == 'Immune_Cell5' ~ 'Spleen',
    rownames(ccd) == 'Tubular.Cell2' ~ 'Kidney',
    rownames(ccd) == 'Tubular.Cell' ~ 'Kidney',
    rownames(ccd) == 'Immune_Cell1' ~ 'Spleen',
    rownames(ccd) == 'Immune_Cell2' ~ 'Heart',
    rownames(ccd) == 'Immune_Cell3' ~ 'Small_Intestine',
    rownames(ccd) == 'Microglial.Cells' ~ 'Brain',
    rownames(ccd) == 'Pneumocytes.2' ~ 'Lung',
    TRUE ~ 'NA'))
    

#correlation function
cc = cor(pd, method = "pearson")

samples <- c("Brain","Colon","Heart","Kidney","Liver","Lung","Pancreas","Small_Intestine","Spleen")
colours <- c("#D51F26","#502A59", "#3D6E57","#8D2B8B", "#DE6C3E","yellow","#8E9ACD","deeppink3","#F9B712")

    
    
colourss <- c("deeppink3","yellow", "#3D6E57","#D51F26", 
"#D51F26","#D51F26","#8E9ACD","#D51F26","#D51F26","#D51F26","yellow", "yellow","#F9B712", "#8D2B8B","yellow","#8E9ACD",
"#D51F26","#D51F26","#D51F26","#D51F26", "#DE6C3E","#502A59", "#F9B712","502A59","#8E9ACD","deeppink3","#F9B712","#D51F26","#502A59", "deeppink3",
             "deeppink3", "deeppink3","#502A59","deeppink3","deeppink3","#8E9ACD","#8E9ACD","#DE6C3E", "#F9B712","#8D2B8B", "#8D2B8B")[my_group]

my_group <- as.numeric(as.factor(substr(rownames(cc), 1 , 1)))
colSide <- colorRampPalette(colourss)(length(unique(rownames(cc))))[my_group]
colMain <- colorRampPalette(brewer.pal(8, "Oranges"))(25)


heatmap.2(cc, scale="column" , RowSideColors=colSide, col=colMain,main="Pearson Correlation",margins = c(12, 12),trace = "none")
legend("topright",# location of the legend on the heatmap plot
       legend = samples, # category labels
       col = colours,  # color key
       lty= 14,   lwd = 6,           
       cex=.5)
    
    
    
