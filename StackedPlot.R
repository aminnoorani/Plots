library(dplyr)
library(ggplot2)
library(reshape)



samples <- c("Brain","Colon","Heart","Kidney","Liver","Lung","Pancreas","Small_Intestine","Spleen")
colours <- c("#D51F26","#272E6A","#208A42","#89288F","#F47D2B", "#FEE500","#8A9FD1","#C06CAB","#F9B712")

#Dendrogram
df <- read.csv("~/Cell_Type_Table2.csv", header = TRUE)
rownames(df) <- df$X
df$X <- NULL
dd <- dist(scale(df), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
ggtree_plot <- ggtree::ggtree(hc)

df <- read.csv("~/Cell_Type_Table2.csv", header = TRUE)
pcm = melt(df, id = c("X"))
pcm$X <- factor(pcm$X,levels=unique(pcm$X))
#if we want to show with Percentage
data_frame = ddply(pcm, .(X), transform,
                   percentage=value/sum(value) * 100)
#or with exact value
data_frame = ddply(pcm, .(X), transform,value)


data_frame$prcntlabel = paste0(sprintf("%.0f",
                                       data_frame$percentage)," ")

header <- c("Cell_Types","Tissue_Types", "value")
colnames(data_frame) <- header


data_frame[data_frame==0] <- NA
data2<-data_frame[complete.cases(data_frame),]
#If error discrete shows up
data2$prcntlabel <- as.numeric(unlist(data2$prcntlabel))

#In case there are cluster numbers
#data2$Cell_Types <- factor(data2$Cell_Types,levels = c("C41","C40","C39","C38","C37","C36","C35","C34","C33","C32",
                                                       "C31","C30","C29","C28","C27","C26","C25","C24",
                                                       "C23","C22","C21","C20","C19","C18","C17","C16",
                                                       "C15","C14","C13","C12","C11","C10","C9","C8",
                                                       "C7","C6","C5","C4","C3","C2","C1"))

mx = ggplot(data2, aes(x = Cell_Types, fill = Tissue_Types, y = value)) + 
  geom_bar(position = "fill",stat = "identity", colour = "black") +
  theme(axis.text.x = element_text(angle = 90, size = 14, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), legend.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = 12, face = "bold")) + 
  scale_y_continuous(labels = scales::date_format()) + 
  labs(x = "Cell Types", y = "Number of Cells", fill = "Tissues") + 
  scale_fill_manual(values = colours, limits = samples)  + coord_flip()

MXX <- mx + geom_text(aes(label=paste0(value)), position=position_fill(vjust=0.5), colour="black", size =2) + scale_y_continuous(labels = scales::percent_format()) + theme_minimal()
plot_grid(ggtree_plot, MXX, nrow = 1, rel_widths = c(0.6,2), align = 'h')




#or with different method
r_color <- sample(colors())
colours3 <- sample(head(r_color, 42))
colours3 

x <- ggplot(data_frame, mapping = aes(x = Cell_Types, y = Percentages_of_Cells, fill = Tissue_Types)) + 
  geom_bar(position= "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) + 
  coord_flip()

x + geom_text(aes(label=paste0(prcntlabel)),
              position=position_fill(vjust=0.5), colour="white", size =2)

data_frame1 <- data_frame$value[data_frame$value == 0] <- NA






