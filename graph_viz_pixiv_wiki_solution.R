require(magrittr)
require(igraph)
score_mat <- read.table('./score_mat_pixiv_wiki_solution.txt') %>% as.matrix

## Scale the score with count number for each idol
idol_mean_score <- colMeans(score_mat)
for (i in 1:9) for (j in 1:9) {
    score_mat[ i, j ] <- score_mat[ i, j ] / mean(idol_mean_score[ c(i, j) ])
}

score_mat_less <- score_mat
score_mat_less <- ifelse(score_mat < quantile(score_mat, .6),
                         0, score_mat)
net <- graph_from_adjacency_matrix(score_mat_less, mode = 'undirected', weighted = T)
V(net)$color <- c('#c3d825', '#ffea00', '#a22041',
                  '#ee7800', '#c0c6c9', '#192f60',
                  '#2ca9e1', '#f09199', '#aa4c8f')
V(net)$frame.color <- V(net)$color
V(net)$size <- sqrt(idol_mean_score) * 1.4
V(net)$label.family <- 'Japan1GothicBBB'
V(net)$label.cex <- 0.5
V(net)$label.color <- c('#000000', '#000000', '#ffffff',
                        '#ffffff', '#000000', '#ffffff',
                        '#ffffff', '#ffffff', '#ffffff')
E(net)$width <- E(net)$weight * 6
E(net)$color <- paste('#7F7F7F',
                      floor(10 + 
                        (E(net)$weight - min(E(net)$weight)) * 
                            (89 / (max(E(net)$weight) - min(E(net)$weight)))),
                      sep = '')
E(net)$label <- sapply(attr(E(net), 'vnames'), function(e) {
    ft <- strsplit(e, '\\|')[[1]]
    score_mat[ft[2], ft[1]] * mean(idol_mean_score)
}) %>% round
E(net)$label.cex <- 0.4
E(net)$label.color <- 'black'

pdf('plot_pixiv_wiki_solution.pdf', family = 'Japan1GothicBBB')
plot(net, layout = layout_in_circle(net))
dev.off()
