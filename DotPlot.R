library(tidyverse)
library(cowplot)
library(ggdendro)
library(patchwork) 
library(ggtree)


markers <- read.csv("~/Downloads/Markers_Top20.csv")
markers <- melt(markers)


markers2 <- markers$name %>% unique()
colnames(markers) <- c("name","variable","Gene_Score")

markers %>% sample_n(5)

#Simple Bubble
markers %>% filter(name %in% markers2) %>% 
  mutate(`% Expressing` = (Gene_Score)) %>% 
  ggplot(aes(x=variable, y = name, color = Gene_Score, size = `% Expressing`)) + 
  geom_point() 

#Nicer
markers %>% filter(name %in% markers2) %>% 
  mutate(`% Expressing` = (Gene_Score)) %>% 
  filter(Gene_Score > 0, `% Expressing` > 1) %>% 
  ggplot(aes(x=variable, y = name, color = Gene_Score, size = `% Expressing`)) + 
  geom_point() 


#More Nicer!
markers %>% filter(name %in% markers2) %>% 
  mutate(`% Expressing` = (Gene_Score)) %>% 
  filter(Gene_Score > 0, `% Expressing` > 1) %>% 
  ggplot(aes(x=variable, y = name, color = Gene_Score, size = `% Expressing`)) + 
  geom_point() + 
  scale_color_viridis_c(name = 'log2 (count + 1)') + 
  cowplot::theme_cowplot() + 
  theme(axis.line  = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('') +
  theme(axis.ticks = element_blank()) 


#with Dendogram
mat <- markers %>% 
  filter(name %in% markers2) %>% 
  pivot_wider(names_from = variable, values_from = Gene_Score) %>% 
  data.frame() # make df as tibbles -> matrix annoying
row.names(mat) <- mat$name  # put gene in `row`
mat <- mat[,-1] #drop gene column as now in rows
clust <- hclust(dist(mat %>% as.matrix())) # hclust with distance matrix
#If get this error: 
#Error in dist(mat %>% as.matrix()) : 
 # 'list' object cannot be coerced to type 'double'
#you need to check whether there are any duplications in the data with the code below:
#markers %>%
#  dplyr::group_by(name, variable) %>%
#  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#  dplyr::filter(n > 1L) 


ddgram <- as.dendrogram(clust) # create dendrogram
ggtree_plot <- ggtree::ggtree(ddgram)
ggtree_plot

dotplot <- markers %>% filter(name %in% markers2) %>% 
  mutate(`% Expressing` = (Gene_Score)) %>% 
  filter(Gene_Score > 0, `% Expressing` > 1) %>% 
  ggplot(aes(x=variable, y = name, color = Gene_Score, size = `% Expressing`)) + 
  geom_point() + 
  cowplot::theme_cowplot() + 
  theme(axis.line  = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('') +
  theme(axis.ticks = element_blank()) +
  scale_color_gradientn(colours = viridis::viridis(20), limits = c(0,4), oob = scales::squish, name = 'log2 (count + 1)')


plot_grid(ggtree_plot, dotplot, nrow = 1, rel_widths = c(0.5,2), align = 'h')

# make data square to calculate euclidean distance
mat <- markers %>% 
  filter(name %in% markers2) %>% 
  pivot_wider(names_from = variable, values_from = Gene_Score) %>% 
  data.frame() # make df as tibbles -> matrix annoying
row.names(mat) <- mat$name  # put gene in `row`
mat <- mat[,-1] #drop gene column as now in rows
v_clust <- hclust(dist(mat %>% as.matrix() %>% t())) # hclust with distance matrix

ddgram_col <- as.dendrogram(v_clust)
ggtree_plot_col <- ggtree(ddgram_col) + layout_dendrogram()

dotplot <- markers %>% filter(name %in% markers2) %>% 
  mutate(
         Gene = factor(name, levels = clust$order),
         cluster = factor(variable, levels = v_clust$labels[v_clust$order])) %>% 
  filter(Gene_Score > 0) %>% 
  ggplot(aes(x=variable, y = name, color = Gene_Score, size = Gene_Score)) + 
  geom_point() + 
  cowplot::theme_cowplot() + 
  theme(axis.line  = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('') +
  theme(axis.ticks = element_blank()) +
  scale_color_gradientn(colours = viridis::viridis(20), limits = c(0,4), oob = scales::squish, name = 'Gene Score') +
  scale_y_discrete(position = "right")
#################################################
ggtree_plot_col <- ggtree_plot_col + xlim2(dotplot)
ggtree_plot <- ggtree_plot + ylim2(dotplot)

labels <- ggplot(markers %>% 
                   mutate(`Clusters` = variable,
                          cluster = factor(variable, levels = v_clust$labels[v_clust$order])), 
                 aes(x = variable, y = 1, fill = `Clusters`)) + 
  geom_tile() + 
  scale_fill_brewer(palette = 'Set1') + 
  theme_nothing() +
  xlim2(dotplot)

legend <- plot_grid(get_legend(labels + theme(legend.position="bottom")))

plot_spacer() + plot_spacer() + ggtree_plot_col +
  plot_spacer() + plot_spacer() + labels + 
  plot_spacer() + plot_spacer() + plot_spacer() +
  ggtree_plot + plot_spacer() + dotplot + 
  plot_spacer() + plot_spacer() + legend + 
  plot_layout(ncol = 3, widths = c(0.7, -0.1, 4), heights = c(0.9, 0.1, -0.1, 4, 1))

