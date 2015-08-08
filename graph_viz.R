require(magrittr)
require(igraph)

score_mat <- read.table('./score_mat.txt') %>% as.matrix
idol_mean_score <- colMeans(score_mat)
for (i in 1:9) for (j in 1:9) {
    score_mat[ i, j ] <- score_mat[ i, j ] / mean(idol_mean_score[ c(i, j) ])
}

score_mat_tune <- score_mat
score_mat_tune <- ifelse(score_mat_tune < quantile(score_mat_tune, 0.6),
                         0, 0.75 * score_mat_tune - quantile(score_mat_tune, 0.6))
net <- graph_from_adjacency_matrix(score_mat_tune, mode = 'undirected', weighted = T)
V(net)$color <- c('#c3d825', '#ffea00', '#a22041',
                  '#ee7800', '#c0c6c9', '#192f60',
                  '#2ca9e1', '#f09199', '#aa4c8f')
V(net)$frame.color <- V(net)$color
V(net)$size <- idol_mean_score
V(net)$label.family <- 'Japan1GothicBBB'
V(net)$label.cex <- 0.5
V(net)$label.color <- c('#000000', '#000000', '#ffffff',
                        '#ffffff', '#000000', '#ffffff',
                        '#ffffff', '#ffffff', '#ffffff')
E(net)$width <- E(net)$weight * 10
E(net)$color <- '#7F7F7F88'

pdf('plot.pdf', family = 'Japan1GothicBBB')
plot(net, layout = layout_in_circle(net))
dev.off()
