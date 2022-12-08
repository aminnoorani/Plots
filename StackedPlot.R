library(dplyr)
library(ggplot2)
library(reshape)



df <- read.csv("~/Cell_Type_Table2.csv", header = TRUE)
pcm = melt(df, id = c("X"))

samples <- c("Brain","Colon","Heart","Kidney","Liver","Lung","Pancreas","Small_Intestine","Spleen")

colours <- c("#D51F26","#502A59", "#3D6E57","#8D2B8B", "#DE6C3E","yellow","#8E9ACD","deeppink3","#F9B712")


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


x <- data_frame[data_frame==0] <- NA
data2<-data_frame[complete.cases(data_frame),]


mx = ggplot(data2, aes(x = Cell_Types, fill = Tissue_Types, y = value)) + 
  geom_bar(position = "fill",stat = "identity", colour = "black") +
  theme(axis.text.x = element_text(angle = 90, size = 14, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), legend.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = 12, face = "bold")) + 
  scale_y_continuous(labels = scales::date_format()) + 
  labs(x = "Cell Types", y = "Number of Cells", fill = "Tissues") + 
  scale_fill_manual(values = colours, limits = samples)  + coord_flip()

mx + geom_text(aes(label=paste0(value)),
               position=position_fill(vjust=0.5), colour="black", size =2) + theme_minimal()

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






