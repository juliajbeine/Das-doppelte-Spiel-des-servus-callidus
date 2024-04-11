#Das doppelte Spiel des servus callidus:
#Eine poetologische und gesellschaftliche Reflexionsfigur auf den europäischen
#Bühnen der Frühen Neuzeit
#Part I: Roman comedies (fabulae palliatae)
#by Julia Jennifer Beine

#[1.] load necessary libraries: 
library(rdracor)
library(tidyverse)
library(igraph)
library(tidygraph)
library(data.table)
library(insight)
library(gt)
library(webshot2)
library(ggnetwork)
library(sna)
library(RColorBrewer)
library(extrafont)
library(visNetwork)
library(network)
library(scales)
library(gtExtras)
library(graphlayouts)
library(ggraph)
library(ggforce)
library(snahelper)

#[2.] get information on the version of the DraCor API
dracor_api_info()

#[3.] get information on RomDraCor, including the last update
meta <- get_dracor_meta()
##convert tibble to data frame
meta_df <- as.data.frame(meta)
show(meta_df)
##delete columns and rows not needed for quotation
filt_meta_df <- meta_df %>%
  filter(title == "Roman Drama Corpus") %>%
  select(-c(plays, characters, male, female, text, sp, stage,
            wordcount.text, wordcount.sp, wordcount.stage))
show(filt_meta_df)
##create table with gt
gt_filt_meta <- gt::gt(filt_meta_df)
show(gt_filt_meta)
##amend table
gt_filt_meta <-
  gt_filt_meta %>%
  ###add header
  tab_header(
    title = "Information on RomDraCor")
show(gt_filt_meta)
##export table
gt_filt_meta_png <-gt::gtsave(gt_filt_meta, "Info_RomDraCor.png",
                              vwidth = 2000,
                              vheight = 1000,
                              zoom = 10)

#[4.] analyse the comedies by Plautus and Terence (RomDraCor)
#which feature a cunning slave (servus callidus) in alphabetical order

#[4.1.] Plautus, Amphitruo (Plaut. Amph.)

##[4.1.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
amph <- get_play_metadata(play = "plautus-amphitruo", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(amph)
####create matrix with general values for the dramatic network
amph_gen_net_struc <- matrix(c(amph$size, amph$density, amph$diameter,
                               amph$averageClustering,
                               amph$averagePathLength, amph$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
amph_gen_net_struc_df <- as.data.frame(amph_gen_net_struc)
###specify columns and rows for the data frame
colnames(amph_gen_net_struc_df) <- c("value")
rownames(amph_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(amph_gen_net_struc_df)
###create table with gt
gt_amph_gen_net_struc <- gt::gt(amph_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_amph_gen_net_struc)
####amend table
gt_amph_gen_net_struc <-
  gt_amph_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Amphitruo",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_amph_gen_net_struc)
####export table
gtsave(gt_amph_gen_net_struc, "amph_gen_net_struc.png", zoom = 10)

##[4.1.2.] extract, calculate, and add character-specific values
amph_coocur <- get_net_cooccur_igraph(play = "plautus-amphitruo", 
                                      corpus = "rom")
class(amph_coocur)
###calculate local clustering
amph_local_clustering <- transitivity(
  amph_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
amph_coocur <- set_vertex_attr(amph_coocur, "local_clustering",
                               value = amph_local_clustering)
###calculate triangles
amph_triangles <- count_triangles(amph_coocur)
###add triangles
amph_coocur <- set_vertex_attr(amph_coocur, "triangles",
                               value = amph_triangles)
###add type manually, reference: cast list in Leo’s edition
V(amph_coocur)$name
amph_coocur <- set_vertex_attr(amph_coocur, "type",
                               value=c("deus, prologus", "servus",
                                       "deus", "matrona", "dux",
                                       "gubernator", "ancilla"))
###show data
show(amph_coocur)
class(amph_coocur)
###export as data frame
amph_char_spec_v_df <- as_data_frame(amph_coocur, what="vertices")
show(amph_char_spec_v_df)

##[4.1.3.] create table with character-specific network values
##extract table with character-specific network values
amph_char_net_df <- select(amph_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(amph_char_net_df)
####arrange rows by degree
amph_char_net_df <- arrange(amph_char_net_df, 
                            desc(degree),
                            desc(weightedDegree))
show(amph_char_net_df)
####create table with gt
gt_amph_char_net <- gt::gt(amph_char_net_df, rownames_to_stub = TRUE)
show(gt_amph_char_net)
####layout table
gt_amph_char_net <-
  gt_amph_char_net %>%
  #####add header
  tab_header(
    title = "Amphitruo",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_amph_char_net <-
  gt_amph_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the scheming god in guise of a slave (deus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Mercurius")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Mercurius"))
####show final layout
show(gt_amph_char_net)
####export table
gtsave(gt_amph_char_net, "amph_char_net.png", zoom = 10)

##[4.1.4.] create network graph

###[4.1.4.1.] layout network graph
###choose layout algorithm for graph
amph_coocur_layout <- create_layout(amph_coocur, layout = "stress")
###layout settings
amph_coocur_layout <-
  ggraph(amph_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(amph_coocur_layout)
###export graph
ggsave(amph_coocur_layout,
       file = "amph_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.2.] Plautus, Asinaria (Plaut. Asin.)

##[4.2.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
asin <- get_play_metadata(play = "plautus-asinaria", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(asin)
####create matrix with general values for the dramatic network
asin_gen_net_struc <- matrix(c(asin$size, asin$density, asin$diameter,
                               asin$averageClustering,
                               asin$averagePathLength, asin$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
asin_gen_net_struc_df <- as.data.frame(asin_gen_net_struc)
###specify columns and rows for the data frame
colnames(asin_gen_net_struc_df) <- c("value")
rownames(asin_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(asin_gen_net_struc_df)
###create table with gt
gt_asin_gen_net_struc <- gt::gt(asin_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_asin_gen_net_struc)
####amend table
gt_asin_gen_net_struc <-
  gt_asin_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Asinaria",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_asin_gen_net_struc)
####export table
gtsave(gt_asin_gen_net_struc, "asin_gen_net_struc.png", zoom = 10)

##[4.2.2.] extract, calculate, and add character-specific values
asin_coocur <- get_net_cooccur_igraph(play = "plautus-asinaria", 
                                      corpus = "rom")
class(asin_coocur)
summary(asin_coocur)
###calculate local clustering
asin_local_clustering <- transitivity(
  asin_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
asin_coocur <- set_vertex_attr(asin_coocur, "local_clustering",
                               value = asin_local_clustering)
###calculate triangles
asin_triangles <- count_triangles(asin_coocur)
###add triangles
asin_coocur <- set_vertex_attr(asin_coocur, "triangles",
                               value = asin_triangles)
###add type manually, reference: cast list in Leo’s edition
V(asin_coocur)$name
asin_coocur <- set_vertex_attr(asin_coocur, "type",
                               value=c("prologus", "servus", "senex",
                                       "adulescens", "lena", "servus",
                                       "mercator", "meretrix", "adulescens",
                                       "parasitus", "matrona", "grex"))
###show data
show(asin_coocur)
class(asin_coocur)
###export as data frame
asin_char_spec_v_df <- as_data_frame(asin_coocur, what="vertices")
show(asin_char_spec_v_df)

##[4.2.3.] create table with character-specific network values
##extract table with character-specific network values
asin_char_net_df <- select(asin_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(asin_char_net_df)
####arrange rows by degree
asin_char_net_df <- arrange(asin_char_net_df, 
                            desc(degree),
                            desc(weightedDegree))
show(asin_char_net_df)
####create table with gt
gt_asin_char_net <- gt::gt(asin_char_net_df, rownames_to_stub = TRUE)
show(gt_asin_char_net)
####layout table
gt_asin_char_net <-
  gt_asin_char_net %>%
  #####add header
  tab_header(
    title = "Asinaria",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_asin_char_net <-
  gt_asin_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the scheming slaves (servi callidi)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Libanus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Leonida")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Libanus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Leonida"))
####show final layout
show(gt_asin_char_net)
####export table
gtsave(gt_asin_char_net, "asin_char_net.png", zoom = 10)

##[4.2.4.] create network graph

###[4.2.4.1.] delete node(s) without edges
show(V(asin_coocur))
asin_coocur <- asin_coocur - c("Prologus")
show(V(asin_coocur))
####control
show(asin_coocur)

###[4.2.4.2.] layout network graph
###choose layout algorithm for graph
asin_coocur_layout <- create_layout(asin_coocur, layout = "stress")
###layout settings
asin_coocur_layout <-
  ggraph(asin_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(asin_coocur_layout)
###export graph
ggsave(asin_coocur_layout,
       file = "asin_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.3.] Plautus, Aulularia (Plaut. Aul.)

##[4.3.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
aul <- get_play_metadata(play = "plautus-aulularia", 
                         corpus = "rom",
                         full_metadata = TRUE)
show(aul)
####create matrix with general values for the dramatic network
aul_gen_net_struc <- matrix(c(aul$size, aul$density, aul$diameter,
                              aul$averageClustering,
                              aul$averagePathLength, aul$averageDegree),
                            ncol = 1, byrow = FALSE)
###convert matrix to data frame
aul_gen_net_struc_df <- as.data.frame(aul_gen_net_struc)
###specify columns and rows for the data frame
colnames(aul_gen_net_struc_df) <- c("value")
rownames(aul_gen_net_struc_df) <- c("size", "density", "diameter",
                                    "average clustering",
                                    "average path length", "average degree")
show(aul_gen_net_struc_df)
###create table with gt
gt_aul_gen_net_struc <- gt::gt(aul_gen_net_struc_df,
                               rownames_to_stub = TRUE)
show(gt_aul_gen_net_struc)
####amend table
gt_aul_gen_net_struc <-
  gt_aul_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Aulularia",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_aul_gen_net_struc)
####export table
gtsave(gt_aul_gen_net_struc, "aul_gen_net_struc.png", zoom = 10)

##[4.3.2.] extract, calculate, and add character-specific values
aul_coocur <- get_net_cooccur_igraph(play = "plautus-aulularia", 
                                     corpus = "rom")
class(aul_coocur)
###calculate local clustering
aul_local_clustering <- transitivity(
  aul_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
aul_coocur <- set_vertex_attr(aul_coocur, "local_clustering",
                              value = aul_local_clustering)
###calculate triangles
aul_triangles <- count_triangles(aul_coocur)
###add triangles
aul_coocur <- set_vertex_attr(aul_coocur, "triangles",
                              value = aul_triangles)
###add type manually, reference: cast list in Leo’s edition
V(aul_coocur)$name
aul_coocur <- set_vertex_attr(aul_coocur, "type",
                              value=c("prologus", "senex", "anus",
                                      "matrona", "senex", "servus",
                                      "cocus", "cocus", "servus",
                                      "adulescens", "puella"))
###show data
show(aul_coocur)
class(aul_coocur)
###export as data frame
aul_char_spec_v_df <- as_data_frame(aul_coocur, what="vertices")
show(aul_char_spec_v_df)

##[4.3.3.] create table with character-specific network values
##extract table with character-specific network values
aul_char_net_df <- select(aul_char_spec_v_df,
                          c(type, degree, weightedDegree,
                            closeness, betweenness,
                            local_clustering, triangles))
show(aul_char_net_df)
####arrange rows by degree
aul_char_net_df <- arrange(aul_char_net_df,
                           desc(degree),
                           desc(weightedDegree))
show(aul_char_net_df)
####create table with gt
gt_aul_char_net <- gt::gt(aul_char_net_df, rownames_to_stub = TRUE)
show(gt_aul_char_net)
####layout table
gt_aul_char_net <-
  gt_aul_char_net %>%
  #####add header
  tab_header(
    title = "Aulularia",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_aul_char_net <-
  gt_aul_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the clever slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Strobilus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Strobilus"))
####show final layout
show(gt_aul_char_net)
####export table
gtsave(gt_aul_char_net, "aul_char_net.png", zoom = 10)

##[4.3.4.] create network graph

###[4.3.4.1.] delete node(s) without edges
show(V(aul_coocur))
aul_coocur <- aul_coocur - c("Lar familiaris")
show(V(aul_coocur))
####control
show(aul_coocur)

###[4.3.4.2.] layout network graph
###choose layout algorithm for graph
aul_coocur_layout <- create_layout(aul_coocur, layout = "stress")
###layout settings
aul_coocur_layout <-
  ggraph(aul_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(aul_coocur_layout)
###export graph
ggsave(aul_coocur_layout,
       file = "aul_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.4.] Plautus, Bacchides (Plaut. Bacch.)

##[4.4.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
bacch <- get_play_metadata(play = "plautus-bacchides", 
                           corpus = "rom",
                           full_metadata = TRUE)
show(bacch)
####create matrix with general values for the dramatic network
bacch_gen_net_struc <- matrix(c(bacch$size, bacch$density, bacch$diameter,
                                bacch$averageClustering,
                                bacch$averagePathLength, bacch$averageDegree),
                              ncol = 1, byrow = FALSE)
###convert matrix to data frame
bacch_gen_net_struc_df <- as.data.frame(bacch_gen_net_struc)
###specify columns and rows for the data frame
colnames(bacch_gen_net_struc_df) <- c("value")
rownames(bacch_gen_net_struc_df) <- c("size", "density", "diameter",
                                      "average clustering",
                                      "average path length", "average degree")
show(bacch_gen_net_struc_df)
###create table with gt
gt_bacch_gen_net_struc <- gt::gt(bacch_gen_net_struc_df,
                                 rownames_to_stub = TRUE)
show(gt_bacch_gen_net_struc)
####amend table
gt_bacch_gen_net_struc <-
  gt_bacch_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Bacchides",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_bacch_gen_net_struc)
####export table
gtsave(gt_bacch_gen_net_struc, "bacch_gen_net_struc.png", zoom = 10)

##[4.4.2.] extract, calculate, and add character-specific values
bacch_coocur <- get_net_cooccur_igraph(play = "plautus-bacchides", 
                                       corpus = "rom")
class(bacch_coocur)
###calculate local clustering
bacch_local_clustering <- transitivity(
  bacch_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
bacch_coocur <- set_vertex_attr(bacch_coocur, "local_clustering",
                                value = bacch_local_clustering)
###calculate triangles
bacch_triangles <- count_triangles(bacch_coocur)
###add triangles
bacch_coocur <- set_vertex_attr(bacch_coocur, "triangles",
                                value = bacch_triangles)
###add type manually, reference: cast list in Leo’s edition
V(bacch_coocur)$name
bacch_coocur <- set_vertex_attr(bacch_coocur, "type",
                                value=c("meretrix", "adulescens", "meretrix",
                                        "paedagogus", "servus", "senex",
                                        "adulescens", "senex", "parasitus",
                                        "miles", "grex"))
###show data
show(bacch_coocur)
class(bacch_coocur)
###export as data frame
bacch_char_spec_v_df <- as_data_frame(bacch_coocur, what="vertices")
show(bacch_char_spec_v_df)

##[4.4.3.] create table with character-specific network values
##extract table with character-specific network values
bacch_char_net_df <- select(bacch_char_spec_v_df,
                            c(type, degree, weightedDegree,
                              closeness, betweenness,
                              local_clustering, triangles))
show(bacch_char_net_df)
####arrange rows by degree
bacch_char_net_df <- arrange(bacch_char_net_df,
                             desc(degree),
                             desc(weightedDegree))
show(bacch_char_net_df)
####create table with gt
gt_bacch_char_net <- gt::gt(bacch_char_net_df, rownames_to_stub = TRUE)
show(gt_bacch_char_net)
####layout table
gt_bacch_char_net <-
  gt_bacch_char_net %>%
  #####add header
  tab_header(
    title = "Bacchides",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_bacch_char_net <-
  gt_bacch_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Chrysalus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Chrysalus"))
####show final layout
show(gt_bacch_char_net)
####export table
gtsave(gt_bacch_char_net, "bacch_char_net.png", zoom = 10)

##[4.4.4.] create network graph

###[4.4.5.1.] layout network graph
###choose layout algorithm for graph
bacch_coocur_layout <- create_layout(bacch_coocur, layout = "stress")
###layout settings
bacch_coocur_layout <-
  ggraph(bacch_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(bacch_coocur_layout)
###export graph
ggsave(bacch_coocur_layout,
       file = "bacch_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.5.] Plautus, Captivi (Plaut. Capt.)

##[4.5.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
capt <- get_play_metadata(play = "plautus-captivi", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(capt)
####create matrix with general values for the dramatic network
capt_gen_net_struc <- matrix(c(capt$size, capt$density, capt$diameter,
                               capt$averageClustering,
                               capt$averagePathLength, capt$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
capt_gen_net_struc_df <- as.data.frame(capt_gen_net_struc)
###specify columns and rows for the data frame
colnames(capt_gen_net_struc_df) <- c("value")
rownames(capt_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(capt_gen_net_struc_df)
###create table with gt
gt_capt_gen_net_struc <- gt::gt(capt_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_capt_gen_net_struc)
####amend table
gt_capt_gen_net_struc <-
  gt_capt_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Captivi",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_capt_gen_net_struc)
####export table
gtsave(gt_capt_gen_net_struc, "capt_gen_net_struc.png", zoom = 10)

##[4.5.2.] extract, calculate, and add character-specific values
capt_coocur <- get_net_cooccur_igraph(play = "plautus-captivi", 
                                      corpus = "rom")
class(capt_coocur)
###calculate local clustering
capt_local_clustering <- transitivity(
  capt_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
capt_coocur <- set_vertex_attr(capt_coocur, "local_clustering",
                               value = capt_local_clustering)
###calculate triangles
capt_triangles <- count_triangles(capt_coocur)
###add triangles
capt_coocur <- set_vertex_attr(capt_coocur, "triangles",
                               value = capt_triangles)
###add type manually, reference: cast list in Leo’s edition
V(capt_coocur)$name
capt_coocur <- set_vertex_attr(capt_coocur, "type",
                               value=c("prologus", "parasitus", "senex",
                                       "lorarius", "lorarius", "servus",
                                       "adulescens", "adulescens", "servus",
                                       "puer", "adulescens", "servus",
                                       "caterva"))
###show data
show(capt_coocur)
class(capt_coocur)
###export as data frame
capt_char_spec_v_df <- as_data_frame(capt_coocur, what="vertices")
show(capt_char_spec_v_df)

##[4.5.3.] create table with character-specific network values
##extract table with character-specific network values
capt_char_net_df <- select(capt_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(capt_char_net_df)
####arrange rows by degree
capt_char_net_df <- arrange(capt_char_net_df,
                            desc(degree),
                            desc(weightedDegree))
show(capt_char_net_df)
####create table with gt
gt_capt_char_net <- gt::gt(capt_char_net_df, rownames_to_stub = TRUE)
show(gt_capt_char_net)
####layout table
gt_capt_char_net <-
  gt_capt_char_net %>%
  #####add header
  tab_header(
    title = "Captivi",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_capt_char_net <-
  gt_capt_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Tyndarus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Tyndarus"))
####show final layout
show(gt_capt_char_net)
####export table
gtsave(gt_capt_char_net, "capt_char_net.png", zoom = 10)

##[4.5.4.] create network graph

###[4.5.4.1.] delete node(s) without edges
show(V(capt_coocur))
capt_coocur <- capt_coocur - c("Prologus") - c("Puer")
show(V(capt_coocur))
####control
show(capt_coocur)

###[4.5.4.2.] layout network graph
###choose layout algorithm for graph
capt_coocur_layout <- create_layout(capt_coocur, layout = "stress")
###layout settings
capt_coocur_layout <-
  ggraph(capt_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(capt_coocur_layout)
###export graph
ggsave(capt_coocur_layout,
       file = "capt_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.6.] Plautus, Casina (Plaut. Cas.): no servi callidi

#[4.7.] Plautus, Cistellaria (Plaut. Cist.): no schemers

#[4.8.] Plautus, Curculio (Plaut. Curc.): no servi callidi

#[4.9.] Plautus, Epidicus (Plaut. Epid.)

##[4.9.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
epid <- get_play_metadata(play = "plautus-epidicus", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(epid)
####create matrix with general values for the dramatic network
epid_gen_net_struc <- matrix(c(epid$size, epid$density, epid$diameter,
                               epid$averageClustering,
                               epid$averagePathLength, epid$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
epid_gen_net_struc_df <- as.data.frame(epid_gen_net_struc)
###specify columns and rows for the data frame
colnames(epid_gen_net_struc_df) <- c("value")
rownames(epid_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(epid_gen_net_struc_df)
###create table with gt
gt_epid_gen_net_struc <- gt::gt(epid_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_epid_gen_net_struc)
####amend table
gt_epid_gen_net_struc <-
  gt_epid_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Epidicus",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_epid_gen_net_struc)
####export table
gtsave(gt_epid_gen_net_struc, "epid_gen_net_struc.png", zoom = 10)

##[4.9.2.] extract, calculate, and add character-specific values
epid_coocur <- get_net_cooccur_igraph(play = "plautus-epidicus", 
                                      corpus = "rom")
class(epid_coocur)
###calculate local clustering
epid_local_clustering <- transitivity(
  epid_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
epid_coocur <- set_vertex_attr(epid_coocur, "local_clustering",
                               value = epid_local_clustering)
###calculate triangles
epid_triangles <- count_triangles(epid_coocur)
###add triangles
epid_coocur <- set_vertex_attr(epid_coocur, "triangles",
                               value = epid_triangles)
###add type manually, reference: cast list in Leo’s edition
V(epid_coocur)$name
epid_coocur <- set_vertex_attr(epid_coocur, "type",
                               value=c("servus", "servus", "adulescens",
                                       "adulescens", "senex", "senex",
                                       "servus", "miles", "fidicina",
                                       "mulier", "fidicina", "danista",
                                       "virgo", "poeta"))
###show data
show(epid_coocur)
class(epid_coocur)
###export as data frame
epid_char_spec_v_df <- as_data_frame(epid_coocur, what="vertices")
show(epid_char_spec_v_df)

##[4.9.3.] create table with character-specific network values
##extract table with character-specific network values
epid_char_net_df <- select(epid_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(epid_char_net_df)
####arrange rows by degree
epid_char_net_df <- arrange(epid_char_net_df,
                            desc(degree),
                            desc(weightedDegree))
show(epid_char_net_df)
####create table with gt
gt_epid_char_net <- gt::gt(epid_char_net_df, rownames_to_stub = TRUE)
show(gt_epid_char_net)
####layout table
gt_epid_char_net <-
  gt_epid_char_net %>%
  #####add header
  tab_header(
    title = "Epidicus",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_epid_char_net <-
  gt_epid_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Epidicus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Epidicus"))
####show final layout
show(gt_epid_char_net)
####export table
gtsave(gt_epid_char_net, "epid_char_net.png", zoom = 10)

##[4.9.4.] create network graph

###[4.9.5.1.] layout network graph
###choose layout algorithm for graph
epid_coocur_layout <- create_layout(epid_coocur, layout = "stress")
###layout settings
epid_coocur_layout <-
  ggraph(epid_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(epid_coocur_layout)
###export graph
ggsave(epid_coocur_layout,
       file = "epid_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.10.] Plautus, Menaechmi (Plaut. Men.): no servi callidi

#[4.11.] Plautus, Mercator (Plaut. Merc.)

##[4.11.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
merc <- get_play_metadata(play = "plautus-mercator", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(merc)
####create matrix with general values for the dramatic network
merc_gen_net_struc <- matrix(c(merc$size, merc$density, merc$diameter,
                               merc$averageClustering,
                               merc$averagePathLength, merc$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
merc_gen_net_struc_df <- as.data.frame(merc_gen_net_struc)
###specify columns and rows for the data frame
colnames(merc_gen_net_struc_df) <- c("value")
rownames(merc_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(merc_gen_net_struc_df)
###create table with gt
gt_merc_gen_net_struc <- gt::gt(merc_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_merc_gen_net_struc)
####amend table
gt_merc_gen_net_struc <-
  gt_merc_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Mercator",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_merc_gen_net_struc)
####export table
gtsave(gt_merc_gen_net_struc, "merc_gen_net_struc.png", zoom = 10)

##[4.11.2.] extract, calculate, and add character-specific values
merc_coocur <- get_net_cooccur_igraph(play = "plautus-mercator", 
                                      corpus = "rom")
class(merc_coocur)
###calculate local clustering
merc_local_clustering <- transitivity(
  merc_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
merc_coocur <- set_vertex_attr(merc_coocur, "local_clustering",
                               value = merc_local_clustering)
###calculate triangles
merc_triangles <- count_triangles(merc_coocur)
###add triangles
merc_coocur <- set_vertex_attr(merc_coocur, "triangles",
                               value = merc_triangles)
###add type manually, reference: cast list in Leo’s edition
V(merc_coocur)$name
merc_coocur <- set_vertex_attr(merc_coocur, "type",
                               value=c("adulescens", "servus", "senex",
                                       "senex", "servus", "adulescens",
                                       "meretrix", "matrona", "anus",
                                       "cocus"))
###show data
show(merc_coocur)
class(merc_coocur)
###export as data frame
merc_char_spec_v_df <- as_data_frame(merc_coocur, what="vertices")
show(merc_char_spec_v_df)

##[4.11.3.] create table with character-specific network values
##extract table with character-specific network values
merc_char_net_df <- select(merc_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(merc_char_net_df)
####arrange rows by degree
merc_char_net_df <- arrange(merc_char_net_df,
                            desc(degree),
                            desc(weightedDegree))
show(merc_char_net_df)
####create table with gt
gt_merc_char_net <- gt::gt(merc_char_net_df, rownames_to_stub = TRUE)
show(gt_merc_char_net)
####layout table
gt_merc_char_net <-
  gt_merc_char_net %>%
  #####add header
  tab_header(
    title = "Mercator",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_merc_char_net <-
  gt_merc_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Acanthio")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Acanthio"))
####show final layout
show(gt_merc_char_net)
####export table
gtsave(gt_merc_char_net, "merc_char_net.png", zoom = 10)

##[4.11.4.] create network graph

###[4.11.5.1.] layout network graph
###choose layout algorithm for graph
merc_coocur_layout <- create_layout(merc_coocur, layout = "stress")
###layout settings
merc_coocur_layout <-
  ggraph(merc_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(merc_coocur_layout)
###export graph
ggsave(merc_coocur_layout,
       file = "merc_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.12.] Plautus, Miles gloriosus (Plaut. Mil.)

##[4.12.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
mil <- get_play_metadata(play = "plautus-miles-gloriosus", 
                         corpus = "rom",
                         full_metadata = TRUE)
show(mil)
####create matrix with general values for the dramatic network
mil_gen_net_struc <- matrix(c(mil$size, mil$density, mil$diameter,
                              mil$averageClustering,
                              mil$averagePathLength, mil$averageDegree),
                            ncol = 1, byrow = FALSE)
###convert matrix to data frame
mil_gen_net_struc_df <- as.data.frame(mil_gen_net_struc)
###specify columns and rows for the data frame
colnames(mil_gen_net_struc_df) <- c("value")
rownames(mil_gen_net_struc_df) <- c("size", "density", "diameter",
                                    "average clustering",
                                    "average path length", "average degree")
show(mil_gen_net_struc_df)
###create table with gt
gt_mil_gen_net_struc <- gt::gt(mil_gen_net_struc_df,
                               rownames_to_stub = TRUE)
show(gt_mil_gen_net_struc)
####amend table
gt_mil_gen_net_struc <-
  gt_mil_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Miles gloriosus",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_mil_gen_net_struc)
####export table
gtsave(gt_mil_gen_net_struc, "mil_gen_net_struc.png", zoom = 10)

##[4.12.2.] extract, calculate, and add character-specific values
mil_coocur <- get_net_cooccur_igraph(play = "plautus-miles-gloriosus", 
                                     corpus = "rom")
class(mil_coocur)
###calculate local clustering
mil_local_clustering <- transitivity(
  mil_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
mil_coocur <- set_vertex_attr(mil_coocur, "local_clustering",
                              value = mil_local_clustering)
###calculate triangles
mil_triangles <- count_triangles(mil_coocur)
###add triangles
mil_coocur <- set_vertex_attr(mil_coocur, "triangles",
                              value = mil_triangles)
###add type manually, reference: cast list in Leo’s edition
V(mil_coocur)$name
mil_coocur <- set_vertex_attr(mil_coocur, "type",
                              value=c("miles", "parasitus", "servus",
                                      "senex", "servus", "mulier",
                                      "adulescens", "puer", "meretrix",
                                      "ancilla", "puer", "cocus",
                                      "servus"))
###show data
show(mil_coocur)
class(mil_coocur)
###export as data frame
mil_char_spec_v_df <- as_data_frame(mil_coocur, what="vertices")
show(mil_char_spec_v_df)

##[4.12.3.] create table with character-specific network values
##extract table with character-specific network values
mil_char_net_df <- select(mil_char_spec_v_df,
                          c(type, degree, weightedDegree,
                            closeness, betweenness,
                            local_clustering, triangles))
show(mil_char_net_df)
####arrange rows by degree
mil_char_net_df <- arrange(mil_char_net_df,
                           desc(degree),
                           desc(weightedDegree))
show(mil_char_net_df)
####create table with gt
gt_mil_char_net <- gt::gt(mil_char_net_df, rownames_to_stub = TRUE)
show(gt_mil_char_net)
####layout table
gt_mil_char_net <-
  gt_mil_char_net %>%
  #####add header
  tab_header(
    title = "Miles gloriosus",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_mil_char_net <-
  gt_mil_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Palaestrio")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Palaestrio"))
####show final layout
show(gt_mil_char_net)
####export table
gtsave(gt_mil_char_net, "mil_char_net.png", zoom = 10)

##[4.12.4.] create network graph

###[4.12.5.1.] layout network graph
###choose layout algorithm for graph
mil_coocur_layout <- create_layout(mil_coocur, layout = "stress")
###layout settings
mil_coocur_layout <-
  ggraph(mil_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(mil_coocur_layout)
###export graph
ggsave(mil_coocur_layout,
       file = "mil_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.13.] Plautus, Mostellaria (Plaut. Most.)

##[4.13.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
most <- get_play_metadata(play = "plautus-mostellaria", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(most)
####create matrix with general values for the dramatic network
most_gen_net_struc <- matrix(c(most$size, most$density, most$diameter,
                               most$averageClustering,
                               most$averagePathLength, most$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
most_gen_net_struc_df <- as.data.frame(most_gen_net_struc)
###specify columns and rows for the data frame
colnames(most_gen_net_struc_df) <- c("value")
rownames(most_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(most_gen_net_struc_df)
###create table with gt
gt_most_gen_net_struc <- gt::gt(most_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_most_gen_net_struc)
####amend table
gt_most_gen_net_struc <-
  gt_most_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Mostellaria",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_most_gen_net_struc)
####export table
gtsave(gt_most_gen_net_struc, "most_gen_net_struc.png", zoom = 10)

##[4.13.2.] extract, calculate, and add character-specific values
most_coocur <- get_net_cooccur_igraph(play = "plautus-mostellaria", 
                                      corpus = "rom")
class(most_coocur)
###calculate local clustering
most_local_clustering <- transitivity(
  most_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
most_coocur <- set_vertex_attr(most_coocur, "local_clustering",
                               value = most_local_clustering)
###calculate triangles
most_triangles <- count_triangles(most_coocur)
###add triangles
most_coocur <- set_vertex_attr(most_coocur, "triangles",
                               value = most_triangles)
###add type manually, reference: cast list in Leo’s edition
V(most_coocur)$name
most_coocur <- set_vertex_attr(most_coocur, "type",
                               value=c("servus", "servus", "adulescens",
                                       "meretrix", "ancilla", "adulescens",
                                       "meretrix", "puer", "senex",
                                       "danista", "senex", "servus",
                                       "servus"))
###show data
show(most_coocur)
class(most_coocur)
###export as data frame
most_char_spec_v_df <- as_data_frame(most_coocur, what="vertices")
show(most_char_spec_v_df)

##[4.13.3.] create table with character-specific network values
##extract table with character-specific network values
most_char_net_df <- select(most_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(most_char_net_df)
####arrange rows by degree
most_char_net_df <- arrange(most_char_net_df,
                            desc(degree),
                            desc(weightedDegree))
show(most_char_net_df)
####create table with gt
gt_most_char_net <- gt::gt(most_char_net_df, rownames_to_stub = TRUE)
show(gt_most_char_net)
####layout table
gt_most_char_net <-
  gt_most_char_net %>%
  #####add header
  tab_header(
    title = "Mostellaria",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_most_char_net <-
  gt_most_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Tranio")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Tranio"))
####show final layout
show(gt_most_char_net)
####export table
gtsave(gt_most_char_net, "most_char_net.png", zoom = 10)

##[4.13.4.] create network graph

###[4.13.5.1.] layout network graph
###choose layout algorithm for graph
most_coocur_layout <- create_layout(most_coocur, layout = "stress")
###layout settings
most_coocur_layout <-
  ggraph(most_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(most_coocur_layout)
###export graph
ggsave(most_coocur_layout,
       file = "most_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.14.] Plautus, Persa (Plaut. Pers.)

##[4.14.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
persa <- get_play_metadata(play = "plautus-persa", 
                           corpus = "rom",
                           full_metadata = TRUE)
show(persa)
####create matrix with general values for the dramatic network
persa_gen_net_struc <- matrix(c(persa$size, persa$density, persa$diameter,
                                persa$averageClustering,
                                persa$averagePathLength, persa$averageDegree),
                              ncol = 1, byrow = FALSE)
###convert matrix to data frame
persa_gen_net_struc_df <- as.data.frame(persa_gen_net_struc)
###specify columns and rows for the data frame
colnames(persa_gen_net_struc_df) <- c("value")
rownames(persa_gen_net_struc_df) <- c("size", "density", "diameter",
                                      "average clustering",
                                      "average path length", "average degree")
show(persa_gen_net_struc_df)
###create table with gt
gt_persa_gen_net_struc <- gt::gt(persa_gen_net_struc_df,
                                 rownames_to_stub = TRUE)
show(gt_persa_gen_net_struc)
####amend table
gt_persa_gen_net_struc <-
  gt_persa_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Persa",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_persa_gen_net_struc)
####export table
gtsave(gt_persa_gen_net_struc, "persa_gen_net_struc.png", zoom = 10)

##[4.14.2.] extract, calculate, and add character-specific values
persa_coocur <- get_net_cooccur_igraph(play = "plautus-persa", 
                                       corpus = "rom")
class(persa_coocur)
###calculate local clustering
persa_local_clustering <- transitivity(
  persa_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
persa_coocur <- set_vertex_attr(persa_coocur, "local_clustering",
                                value = persa_local_clustering)
###calculate triangles
persa_triangles <- count_triangles(persa_coocur)
###add triangles
persa_coocur <- set_vertex_attr(persa_coocur, "triangles",
                                value = persa_triangles)
###add type manually, reference: cast list in Leo’s edition
V(persa_coocur)$name
persa_coocur <- set_vertex_attr(persa_coocur, "type",
                                value=c("servus", "servus", "parasitus",
                                        "ancilla", "meretrix", "puer",
                                        "virgo", "leno"))
###show data
show(persa_coocur)
class(persa_coocur)
###export as data frame
persa_char_spec_v_df <- as_data_frame(persa_coocur, what="vertices")
show(persa_char_spec_v_df)

##[4.14.3.] create table with character-specific network values
##extract table with character-specific network values
persa_char_net_df <- select(persa_char_spec_v_df,
                            c(type, degree, weightedDegree,
                              closeness, betweenness,
                              local_clustering, triangles))
show(persa_char_net_df)
####arrange rows by degree
persa_char_net_df <- arrange(persa_char_net_df,
                             desc(degree),
                             desc(weightedDegree))
show(persa_char_net_df)
####create table with gt
gt_persa_char_net <- gt::gt(persa_char_net_df, rownames_to_stub = TRUE)
show(gt_persa_char_net)
####layout table
gt_persa_char_net <-
  gt_persa_char_net %>%
  #####add header
  tab_header(
    title = "Persa",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_persa_char_net <-
  gt_persa_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Toxilus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Toxilus"))
####show final layout
show(gt_persa_char_net)
####export table
gtsave(gt_persa_char_net, "persa_char_net.png", zoom = 10)

##[4.14.4.] create network graph

###[4.14.5.1.] layout network graph
###choose layout algorithm for graph
persa_coocur_layout <- create_layout(persa_coocur, layout = "stress")
###layout settings
persa_coocur_layout <-
  ggraph(persa_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(persa_coocur_layout)
###export graph
ggsave(persa_coocur_layout,
       file = "persa_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.15.] Plautus, Poenulus (Plaut. Poen.)

##[4.15.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
poen <- get_play_metadata(play = "plautus-poenulus", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(poen)
####create matrix with general values for the dramatic network
poen_gen_net_struc <- matrix(c(poen$size, poen$density, poen$diameter,
                               poen$averageClustering,
                               poen$averagePathLength, poen$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
poen_gen_net_struc_df <- as.data.frame(poen_gen_net_struc)
###specify columns and rows for the data frame
colnames(poen_gen_net_struc_df) <- c("value")
rownames(poen_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(poen_gen_net_struc_df)
###create table with gt
gt_poen_gen_net_struc <- gt::gt(poen_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_poen_gen_net_struc)
####amend table
gt_poen_gen_net_struc <-
  gt_poen_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Poenulus",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_poen_gen_net_struc)
####export table
gtsave(gt_poen_gen_net_struc, "poen_gen_net_struc.png", zoom = 10)

##[4.15.2.] extract, calculate, and add character-specific values
poen_coocur <- get_net_cooccur_igraph(play = "plautus-poenulus", 
                                      corpus = "rom")
class(poen_coocur)
###calculate local clustering
poen_local_clustering <- transitivity(
  poen_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
poen_coocur <- set_vertex_attr(poen_coocur, "local_clustering",
                               value = poen_local_clustering)
###calculate triangles
poen_triangles <- count_triangles(poen_coocur)
###add triangles
poen_coocur <- set_vertex_attr(poen_coocur, "triangles",
                               value = poen_triangles)
###add type manually, reference: cast list in Leo’s edition
V(poen_coocur)$name
poen_coocur <- set_vertex_attr(poen_coocur, "type",
                               value=c("prologus", "adulescens", "servus",
                                       "puella", "puella", "ancilla",
                                       "leno", "miles", "advocatus",
                                       "vilicus", "servus", "Poenus",
                                       "nutrix", "puer"))
###show data
show(poen_coocur)
class(poen_coocur)
###export as data frame
poen_char_spec_v_df <- as_data_frame(poen_coocur, what="vertices")
show(poen_char_spec_v_df)

##[4.15.3.] create table with character-specific network values
##extract table with character-specific network values
poen_char_net_df <- select(poen_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(poen_char_net_df)
####arrange rows by degree
poen_char_net_df <- arrange(poen_char_net_df,
                            desc(degree),
                            desc(weightedDegree))
show(poen_char_net_df)
####create table with gt
gt_poen_char_net <- gt::gt(poen_char_net_df, rownames_to_stub = TRUE)
show(gt_poen_char_net)
####layout table
gt_poen_char_net <-
  gt_poen_char_net %>%
  #####add header
  tab_header(
    title = "Poenulus",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_poen_char_net <-
  gt_poen_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Milphio")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Milphio"))
####show final layout
show(gt_poen_char_net)
####export table
gtsave(gt_poen_char_net, "poen_char_net.png", zoom = 10)

##[4.15.4.] create network graph

###[4.15.4.1.] delete node(s) without edges
show(V(poen_coocur))
poen_coocur <- poen_coocur - c("Prologus")
show(V(poen_coocur))
####control
show(poen_coocur)

###[4.15.4.2.] layout network graph
###choose layout algorithm for graph
poen_coocur_layout <- create_layout(poen_coocur, layout = "stress")
###layout settings
poen_coocur_layout <-
  ggraph(poen_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(poen_coocur_layout)
###export graph
ggsave(poen_coocur_layout,
       file = "poen_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.16.] Plautus, Pseudolus (Plaut. Pseud.)

##[4.16.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
pseud <- get_play_metadata(play = "plautus-pseudolus", 
                           corpus = "rom",
                           full_metadata = TRUE)
show(pseud)
####create matrix with general values for the dramatic network
pseud_gen_net_struc <- matrix(c(pseud$size, pseud$density, pseud$diameter,
                                pseud$averageClustering,
                                pseud$averagePathLength, pseud$averageDegree),
                              ncol = 1, byrow = FALSE)
###convert matrix to data frame
pseud_gen_net_struc_df <- as.data.frame(pseud_gen_net_struc)
###specify columns and rows for the data frame
colnames(pseud_gen_net_struc_df) <- c("value")
rownames(pseud_gen_net_struc_df) <- c("size", "density", "diameter",
                                      "average clustering",
                                      "average path length", "average degree")
show(pseud_gen_net_struc_df)
###create table with gt
gt_pseud_gen_net_struc <- gt::gt(pseud_gen_net_struc_df,
                                 rownames_to_stub = TRUE)
show(gt_pseud_gen_net_struc)
####amend table
gt_pseud_gen_net_struc <-
  gt_pseud_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Pseudolus",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_pseud_gen_net_struc)
####export table
gtsave(gt_pseud_gen_net_struc, "pseud_gen_net_struc.png", zoom = 10)

##[4.16.2.] extract, calculate, and add character-specific values
pseud_coocur <- get_net_cooccur_igraph(play = "plautus-pseudolus", 
                                       corpus = "rom")
class(pseud_coocur)
###calculate local clustering
pseud_local_clustering <- transitivity(
  pseud_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
pseud_coocur <- set_vertex_attr(pseud_coocur, "local_clustering",
                                value = pseud_local_clustering)
###calculate triangles
pseud_triangles <- count_triangles(pseud_coocur)
###add triangles
pseud_coocur <- set_vertex_attr(pseud_coocur, "triangles",
                                value = pseud_triangles)
###add type manually, reference: cast list in Leo’s edition
V(pseud_coocur)$name
pseud_coocur <- set_vertex_attr(pseud_coocur, "type",
                                value=c("prologus", "servus", "adulescens",
                                        "leno", "servus", "senex",
                                        "senex", "servus", "adulescens",
                                        "puer", "cocus", "servus"))
###show data
show(pseud_coocur)
class(pseud_coocur)
###export as data frame
pseud_char_spec_v_df <- as_data_frame(pseud_coocur, what="vertices")
show(pseud_char_spec_v_df)

##[4.16.3.] create table with character-specific network values
##extract table with character-specific network values
pseud_char_net_df <- select(pseud_char_spec_v_df,
                            c(type, degree, weightedDegree,
                              closeness, betweenness,
                              local_clustering, triangles))
show(pseud_char_net_df)
####arrange rows by degree
pseud_char_net_df <- arrange(pseud_char_net_df,
                             desc(degree),
                             desc(weightedDegree))
show(pseud_char_net_df)
####create table with gt
gt_pseud_char_net <- gt::gt(pseud_char_net_df, rownames_to_stub = TRUE)
show(gt_pseud_char_net)
####layout table
gt_pseud_char_net <-
  gt_pseud_char_net %>%
  #####add header
  tab_header(
    title = "Pseudolus",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_pseud_char_net <-
  gt_pseud_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Pseudolus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Pseudolus"))
####show final layout
show(gt_pseud_char_net)
####export table
gtsave(gt_pseud_char_net, "pseud_char_net.png", zoom = 10)

##[4.16.4.] create network graph

###[4.16.4.1.] delete node(s) without edges
show(V(pseud_coocur))
pseud_coocur <- pseud_coocur - c("Prologus")
show(V(pseud_coocur))
####control
show(pseud_coocur)

###[4.16.4.2.] layout network graph
###choose layout algorithm for graph
pseud_coocur_layout <- create_layout(pseud_coocur, layout = "stress")
###layout settings
pseud_coocur_layout <-
  ggraph(pseud_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(pseud_coocur_layout)
###export graph
ggsave(pseud_coocur_layout,
       file = "pseud_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.17.] Plautus, Rudens (Plaut. Rud.)

##[4.17.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
rud <- get_play_metadata(play = "plautus-rudens", 
                         corpus = "rom",
                         full_metadata = TRUE)
show(rud)
####create matrix with general values for the dramatic network
rud_gen_net_struc <- matrix(c(rud$size, rud$density, rud$diameter,
                              rud$averageClustering,
                              rud$averagePathLength, rud$averageDegree),
                            ncol = 1, byrow = FALSE)
###convert matrix to data frame
rud_gen_net_struc_df <- as.data.frame(rud_gen_net_struc)
###specify columns and rows for the data frame
colnames(rud_gen_net_struc_df) <- c("value")
rownames(rud_gen_net_struc_df) <- c("size", "density", "diameter",
                                    "average clustering",
                                    "average path length", "average degree")
show(rud_gen_net_struc_df)
###create table with gt
gt_rud_gen_net_struc <- gt::gt(rud_gen_net_struc_df,
                               rownames_to_stub = TRUE)
show(gt_rud_gen_net_struc)
####amend table
gt_rud_gen_net_struc <-
  gt_rud_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Rudens",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_rud_gen_net_struc)
####export table
gtsave(gt_rud_gen_net_struc, "rud_gen_net_struc.png", zoom = 10)

##[4.17.2.] extract, calculate, and add character-specific values
rud_coocur <- get_net_cooccur_igraph(play = "plautus-rudens", 
                                     corpus = "rom")
class(rud_coocur)
###calculate local clustering
rud_local_clustering <- transitivity(
  rud_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
rud_coocur <- set_vertex_attr(rud_coocur, "local_clustering",
                              value = rud_local_clustering)
###calculate triangles
rud_triangles <- count_triangles(rud_coocur)
###add triangles
rud_coocur <- set_vertex_attr(rud_coocur, "triangles",
                              value = rud_triangles)
###add type manually, reference: cast list in Leo’s edition
V(rud_coocur)$name
rud_coocur <- set_vertex_attr(rud_coocur, "type",
                              value=c("prologus", "servus", "adulescens",
                                      "senex", "puella", "puella",
                                      "sacerdos", "piscator", "servus",
                                      "leno", "senex", "lorarius",
                                      "lorarius", "piscator"))
###show data
show(rud_coocur)
class(rud_coocur)
###export as data frame
rud_char_spec_v_df <- as_data_frame(rud_coocur, what="vertices")
show(rud_char_spec_v_df)

##[4.17.3.] create table with character-specific network values
##extract table with character-specific network values
rud_char_net_df <- select(rud_char_spec_v_df,
                          c(type, degree, weightedDegree,
                            closeness, betweenness,
                            local_clustering, triangles))
show(rud_char_net_df)
####arrange rows by degree
rud_char_net_df <- arrange(rud_char_net_df,
                           desc(degree),
                           desc(weightedDegree))
show(rud_char_net_df)
####create table with gt
gt_rud_char_net <- gt::gt(rud_char_net_df, rownames_to_stub = TRUE)
show(gt_rud_char_net)
####layout table
gt_rud_char_net <-
  gt_rud_char_net %>%
  #####add header
  tab_header(
    title = "Rudens",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_rud_char_net <-
  gt_rud_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the clever slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Gripus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Gripus"))
####show final layout
show(gt_rud_char_net)
####export table
gtsave(gt_rud_char_net, "rud_char_net.png", zoom = 10)

##[4.17.4.] create network graph

###[4.17.4.1.] delete node(s) without edges
show(V(rud_coocur))
rud_coocur <- rud_coocur - c("Arcturus")
show(V(rud_coocur))
####control
show(rud_coocur)

###[4.17.4.2.] layout network graph
###choose layout algorithm for graph
rud_coocur_layout <- create_layout(rud_coocur, layout = "stress")
###layout settings
rud_coocur_layout <-
  ggraph(rud_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(rud_coocur_layout)
###export graph
ggsave(rud_coocur_layout,
       file = "rud_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.18.] Plautus, Stichus (Plaut. Stich.): no schemers

#[4.19.] Plautus, Trinummus (Plaut. Trin.)

##[4.19.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
trin <- get_play_metadata(play = "plautus-trinummus", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(trin)
####create matrix with general values for the dramatic network
trin_gen_net_struc <- matrix(c(trin$size, trin$density, trin$diameter,
                               trin$averageClustering,
                               trin$averagePathLength, trin$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
trin_gen_net_struc_df <- as.data.frame(trin_gen_net_struc)
###specify columns and rows for the data frame
colnames(trin_gen_net_struc_df) <- c("value")
rownames(trin_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(trin_gen_net_struc_df)
###create table with gt
gt_trin_gen_net_struc <- gt::gt(trin_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_trin_gen_net_struc)
####amend table
gt_trin_gen_net_struc <-
  gt_trin_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Trinummus",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_trin_gen_net_struc)
####export table
gtsave(gt_trin_gen_net_struc, "trin_gen_net_struc.png", zoom = 10)

##[4.19.2.] extract, calculate, and add character-specific values
trin_coocur <- get_net_cooccur_igraph(play = "plautus-trinummus", 
                                      corpus = "rom")
class(trin_coocur)
###calculate local clustering
trin_local_clustering <- transitivity(
  trin_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
trin_coocur <- set_vertex_attr(trin_coocur, "local_clustering",
                               value = trin_local_clustering)
###calculate triangles
trin_triangles <- count_triangles(trin_coocur)
###add triangles
trin_coocur <- set_vertex_attr(trin_coocur, "triangles",
                               value = trin_triangles)
###add type manually, reference: cast list in Leo’s edition
###specified omnes as caterva
V(trin_coocur)$name
trin_coocur <- set_vertex_attr(trin_coocur, "type",
                               value=c("prologus", "prologus", "senex",
                                       "senex", "adulescens", "senex",
                                       "adulescens", "servus", "senex",
                                       "sycophanta", "caterva"))
###show data
show(trin_coocur)
class(trin_coocur)
###export as data frame
trin_char_spec_v_df <- as_data_frame(trin_coocur, what="vertices")
show(trin_char_spec_v_df)

##[4.19.3.] create table with character-specific network values
##extract table with character-specific network values
trin_char_net_df <- select(trin_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(trin_char_net_df)
####arrange rows by degree
trin_char_net_df <- arrange(trin_char_net_df,
                            desc(degree),
                            desc(weightedDegree))
show(trin_char_net_df)
####create table with gt
gt_trin_char_net <- gt::gt(trin_char_net_df, rownames_to_stub = TRUE)
show(gt_trin_char_net)
####layout table
gt_trin_char_net <-
  gt_trin_char_net %>%
  #####add header
  tab_header(
    title = "Trinummus",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_trin_char_net <-
  gt_trin_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Stasimus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Stasimus"))
####show final layout
show(gt_trin_char_net)
####export table
gtsave(gt_trin_char_net, "trin_char_net.png", zoom = 10)

##[4.19.4.] create network graph

###[4.19.4.1.] delete node(s) of extern prologue speakers
show(V(trin_coocur))
trin_coocur <- trin_coocur - c("Inopia") - c("Luxuria")
show(V(trin_coocur))
####control
show(trin_coocur)

###[4.19.4.2.] layout network graph
###choose layout algorithm for graph
trin_coocur_layout <- create_layout(trin_coocur, layout = "stress")
###layout settings
trin_coocur_layout <-
  ggraph(trin_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(trin_coocur_layout)
###export graph
ggsave(trin_coocur_layout,
       file = "trin_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.20.] Plautus, Truculentus (Plaut. Truc.): no servi callidi

#[4.21.] Terence, Adelphoe (Ter. Ad.)

##[4.21.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
ad <- get_play_metadata(play = "terence-adelphi", 
                        corpus = "rom",
                        full_metadata = TRUE)
show(ad)
####create matrix with general values for the dramatic network
ad_gen_net_struc <- matrix(c(ad$size, ad$density, ad$diameter,
                             ad$averageClustering,
                             ad$averagePathLength, ad$averageDegree),
                           ncol = 1, byrow = FALSE)
###convert matrix to data frame
ad_gen_net_struc_df <- as.data.frame(ad_gen_net_struc)
###specify columns and rows for the data frame
colnames(ad_gen_net_struc_df) <- c("value")
rownames(ad_gen_net_struc_df) <- c("size", "density", "diameter",
                                   "average clustering",
                                   "average path length", "average degree")
show(ad_gen_net_struc_df)
###create table with gt
gt_ad_gen_net_struc <- gt::gt(ad_gen_net_struc_df,
                              rownames_to_stub = TRUE)
show(gt_ad_gen_net_struc)
####amend table
gt_ad_gen_net_struc <-
  gt_ad_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Adelphoe",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_ad_gen_net_struc)
####export table
gtsave(gt_ad_gen_net_struc, "ad_gen_net_struc.png", zoom = 10)

##[4.21.2.] extract, calculate, and add character-specific values
ad_coocur <- get_net_cooccur_igraph(play = "terence-adelphi", 
                                    corpus = "rom")
class(ad_coocur)
###calculate local clustering
ad_local_clustering <- transitivity(
  ad_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
ad_coocur <- set_vertex_attr(ad_coocur, "local_clustering",
                             value = ad_local_clustering)
###calculate triangles
ad_triangles <- count_triangles(ad_coocur)
###add triangles
ad_coocur <- set_vertex_attr(ad_coocur, "triangles",
                             value = ad_triangles)
###add type manually, reference: cast list in Parry’s edition
###changed adolescens to adulescens, mater to matrona, amica to virgo,
###omnes to caterva
###note: V. 172c (omitte mulierem) is attributed to Aeschinus by Parry
###while Lindsay attributes it to Parmeno
V(ad_coocur)$name
ad_coocur <- set_vertex_attr(ad_coocur, "type",
                             value=c("prologus", "senex", "senex",
                                     "leno", "adulescens", "servus",
                                     "adulescens", "matrona", "nutrix",
                                     "servus", "senex", "virgo",
                                     "servus", "caterva"))
###show data
show(ad_coocur)
class(ad_coocur)
###export as data frame
ad_char_spec_v_df <- as_data_frame(ad_coocur, what="vertices")
show(ad_char_spec_v_df)

##[4.21.3.] create table with character-specific network values
##extract table with character-specific network values
ad_char_net_df <- select(ad_char_spec_v_df,
                         c(type, degree, weightedDegree,
                           closeness, betweenness,
                           local_clustering, triangles))
show(ad_char_net_df)
####arrange rows by degree
ad_char_net_df <- arrange(ad_char_net_df,
                          desc(degree),
                          desc(weightedDegree))
show(ad_char_net_df)
####create table with gt
gt_ad_char_net <- gt::gt(ad_char_net_df, rownames_to_stub = TRUE)
show(gt_ad_char_net)
####layout table
gt_ad_char_net <-
  gt_ad_char_net %>%
  #####add header
  tab_header(
    title = "Adelphoe",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_ad_char_net <-
  gt_ad_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Syrus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Syrus"))
####show final layout
show(gt_ad_char_net)
####export table
gtsave(gt_ad_char_net, "ad_char_net.png", zoom = 10)

##[4.21.4.] create network graph

###[4.21.4.1.] delete node(s) without edges
show(V(ad_coocur))
ad_coocur <- ad_coocur - c("Prologus")
show(V(ad_coocur))
####control
show(ad_coocur)

###[4.21.4.2.] layout network graph
###choose layout algorithm for graph
ad_coocur_layout <- create_layout(ad_coocur, layout = "stress")
###layout settings
ad_coocur_layout <-
  ggraph(ad_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(ad_coocur_layout)
###export graph
ggsave(ad_coocur_layout,
       file = "ad_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.22.] Terence, Andria (Ter. Andr.)

##[4.22.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
andr <- get_play_metadata(play = "terence-andria", 
                          corpus = "rom",
                          full_metadata = TRUE)
show(andr)
####create matrix with general values for the dramatic network
andr_gen_net_struc <- matrix(c(andr$size, andr$density, andr$diameter,
                               andr$averageClustering,
                               andr$averagePathLength, andr$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
andr_gen_net_struc_df <- as.data.frame(andr_gen_net_struc)
###specify columns and rows for the data frame
colnames(andr_gen_net_struc_df) <- c("value")
rownames(andr_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(andr_gen_net_struc_df)
###create table with gt
gt_andr_gen_net_struc <- gt::gt(andr_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_andr_gen_net_struc)
####amend table
gt_andr_gen_net_struc <-
  gt_andr_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Andria",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_andr_gen_net_struc)
####export table
gtsave(gt_andr_gen_net_struc, "andr_gen_net_struc.png", zoom = 10)

##[4.22.2.] extract, calculate, and add character-specific values
andr_coocur <- get_net_cooccur_igraph(play = "terence-andria", 
                                      corpus = "rom")
class(andr_coocur)
###calculate local clustering
andr_local_clustering <- transitivity(
  andr_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
andr_coocur <- set_vertex_attr(andr_coocur, "local_clustering",
                               value = andr_local_clustering)
###calculate triangles
andr_triangles <- count_triangles(andr_coocur)
###add triangles
andr_coocur <- set_vertex_attr(andr_coocur, "triangles",
                               value = andr_triangles)
###add type manually, reference: cast list in Parry’s edition,
###changed adolescens to adulescens, hospes to senex (see Lindsay),
###meretrix to virgo (see Lindsay), omnes to caterva
V(andr_coocur)$name
andr_coocur <- set_vertex_attr(andr_coocur, "type",
                               value=c("prologus", "senex", "libertus",
                                       "servus", "ancilla", "adulescens",
                                       "adulescens", "servus", "obstetrix",
                                       "virgo", "senex", "senex",
                                       "lorarius", "caterva"))
###show data
show(andr_coocur)
class(andr_coocur)
###export as data frame
andr_char_spec_v_df <- as_data_frame(andr_coocur, what="vertices")
show(andr_char_spec_v_df)

##[4.22.3.] create table with character-specific network values
##extract table with character-specific network values
andr_char_net_df <- select(andr_char_spec_v_df,
                           c(type, degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(andr_char_net_df)
####arrange rows by degree
andr_char_net_df <- arrange(andr_char_net_df,
                            desc(degree),
                            desc(weightedDegree))
show(andr_char_net_df)
####create table with gt
gt_andr_char_net <- gt::gt(andr_char_net_df, rownames_to_stub = TRUE)
show(gt_andr_char_net)
####layout table
gt_andr_char_net <-
  gt_andr_char_net %>%
  #####add header
  tab_header(
    title = "Andria",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_andr_char_net <-
  gt_andr_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Davus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Davus"))
####show final layout
show(gt_andr_char_net)
####export table
gtsave(gt_andr_char_net, "andr_char_net.png", zoom = 10)

##[4.22.4.] create network graph

###[4.22.4.1.] delete node(s) without edges
show(V(andr_coocur))
andr_coocur <- andr_coocur - c("Prologus")
show(V(andr_coocur))
####control
show(andr_coocur)

###[4.22.4.2.] layout network graph
###choose layout algorithm for graph
andr_coocur_layout <- create_layout(andr_coocur, layout = "stress")
###layout settings
andr_coocur_layout <-
  ggraph(andr_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(andr_coocur_layout)
###export graph
ggsave(andr_coocur_layout,
       file = "andr_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.23.] Terence, Eunuchus (Ter. Eun.)

##[4.23.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
eun <- get_play_metadata(play = "terence-eunuchus", 
                         corpus = "rom",
                         full_metadata = TRUE)
show(eun)
####create matrix with general values for the dramatic network
eun_gen_net_struc <- matrix(c(eun$size, eun$density, eun$diameter,
                              eun$averageClustering,
                              eun$averagePathLength, eun$averageDegree),
                            ncol = 1, byrow = FALSE)
###convert matrix to data frame
eun_gen_net_struc_df <- as.data.frame(eun_gen_net_struc)
###specify columns and rows for the data frame
colnames(eun_gen_net_struc_df) <- c("value")
rownames(eun_gen_net_struc_df) <- c("size", "density", "diameter",
                                    "average clustering",
                                    "average path length", "average degree")
show(eun_gen_net_struc_df)
###create table with gt
gt_eun_gen_net_struc <- gt::gt(eun_gen_net_struc_df,
                               rownames_to_stub = TRUE)
show(gt_eun_gen_net_struc)
####amend table
gt_eun_gen_net_struc <-
  gt_eun_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Eunuchus",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_eun_gen_net_struc)
####export table
gtsave(gt_eun_gen_net_struc, "eun_gen_net_struc.png", zoom = 10)

##[4.23.2.] extract, calculate, and add character-specific values
eun_coocur <- get_net_cooccur_igraph(play = "terence-eunuchus", 
                                     corpus = "rom")
class(eun_coocur)
###calculate local clustering
eun_local_clustering <- transitivity(
  eun_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
eun_coocur <- set_vertex_attr(eun_coocur, "local_clustering",
                              value = eun_local_clustering)
###calculate triangles
eun_triangles <- count_triangles(eun_coocur)
###add triangles
eun_coocur <- set_vertex_attr(eun_coocur, "triangles",
                              value = eun_triangles)
###add type manually, reference: cast list in Parry’s edition
###changed adolescens to adulescens, omnes to caterva
V(eun_coocur)$name
eun_coocur <- set_vertex_attr(eun_coocur, "type",
                              value=c("prologus", "adulescens", "servus",
                                      "meretrix", "parasitus", "adulescens",
                                      "miles", "ancilla", "adulescens",
                                      "adulescens", "ancilla", "eunuchus",
                                      "lixa", "nutrix", "senex", "caterva"))
###show data
show(eun_coocur)
class(eun_coocur)
###export as data frame
eun_char_spec_v_df <- as_data_frame(eun_coocur, what="vertices")
show(eun_char_spec_v_df)

##[4.23.3.] create table with character-specific network values
##extract table with character-specific network values
eun_char_net_df <- select(eun_char_spec_v_df,
                          c(type, degree, weightedDegree,
                            closeness, betweenness,
                            local_clustering, triangles))
show(eun_char_net_df)
####arrange rows by degree
eun_char_net_df <- arrange(eun_char_net_df,
                           desc(degree),
                           desc(weightedDegree))
show(eun_char_net_df)
####create table with gt
gt_eun_char_net <- gt::gt(eun_char_net_df, rownames_to_stub = TRUE)
show(gt_eun_char_net)
####layout table
gt_eun_char_net <-
  gt_eun_char_net %>%
  #####add header
  tab_header(
    title = "Eunuchus",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_eun_char_net <-
  gt_eun_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (ancilla callida)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Pythias")) %>%
  ####bold row showing the scheming slave (ancilla callida)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Pythias"))
####show final layout
show(gt_eun_char_net)
####export table
gtsave(gt_eun_char_net, "eun_char_net.png", zoom = 10)

##[4.23.4.] create network graph

###[4.23.4.1.] delete node(s) without edges
show(V(eun_coocur))
eun_coocur <- eun_coocur - c("Prologus")
show(V(eun_coocur))
####control
show(eun_coocur)

###[4.23.4.2.] layout network graph
###choose layout algorithm for graph
eun_coocur_layout <- create_layout(eun_coocur, layout = "stress")
###layout settings
eun_coocur_layout <-
  ggraph(eun_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(eun_coocur_layout)
###export graph
ggsave(eun_coocur_layout,
       file = "eun_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.24.] Terence, Heautontimoroumenos (Ter. Heaut.)

##[4.24.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
heaut <- get_play_metadata(play = "terence-heautontimorumenos", 
                           corpus = "rom",
                           full_metadata = TRUE)
show(heaut)
####create matrix with general values for the dramatic network
heaut_gen_net_struc <- matrix(c(heaut$size, heaut$density, heaut$diameter,
                                heaut$averageClustering,
                                heaut$averagePathLength, heaut$averageDegree),
                              ncol = 1, byrow = FALSE)
###convert matrix to data frame
heaut_gen_net_struc_df <- as.data.frame(heaut_gen_net_struc)
###specify columns and rows for the data frame
colnames(heaut_gen_net_struc_df) <- c("value")
rownames(heaut_gen_net_struc_df) <- c("size", "density", "diameter",
                                      "average clustering",
                                      "average path length", "average degree")
show(heaut_gen_net_struc_df)
###create table with gt
gt_heaut_gen_net_struc <- gt::gt(heaut_gen_net_struc_df,
                                 rownames_to_stub = TRUE)
show(gt_heaut_gen_net_struc)
####amend table
gt_heaut_gen_net_struc <-
  gt_heaut_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Heautontimoroumenos",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_heaut_gen_net_struc)
####export table
gtsave(gt_heaut_gen_net_struc, "heaut_gen_net_struc.png", zoom = 10)

##[4.24.2.] extract, calculate, and add character-specific values
heaut_coocur <- get_net_cooccur_igraph(play = "terence-heautontimorumenos", 
                                       corpus = "rom")
class(heaut_coocur)
###calculate local clustering
heaut_local_clustering <- transitivity(
  heaut_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
heaut_coocur <- set_vertex_attr(heaut_coocur, "local_clustering",
                                value = heaut_local_clustering)
###calculate triangles
heaut_triangles <- count_triangles(heaut_coocur)
###add triangles
heaut_coocur <- set_vertex_attr(heaut_coocur, "triangles",
                                value = heaut_triangles)
###add type manually, reference: cast list in Parry’s edition
###changed pater to senex, filius to adulescens,
###amica  to meretrix for Bacchis (see Lindsay),
###amica to virgo for Antiphila ( see Lindsay),
###uxor to matrona (see Lindsay), omnes to caterva
V(heaut_coocur)$name
heaut_coocur <- set_vertex_attr(heaut_coocur, "type",
                                value=c("prologus", "senex", "senex",
                                        "adulescens", "adulescens", "servus",
                                        "servus", "meretrix", "virgo",
                                        "matrona", "nutrix", "ancilla",
                                        "caterva"))
###show data
show(heaut_coocur)
class(heaut_coocur)
###export as data frame
heaut_char_spec_v_df <- as_data_frame(heaut_coocur, what="vertices")
show(heaut_char_spec_v_df)

##[4.24.3.] create table with character-specific network values
##extract table with character-specific network values
heaut_char_net_df <- select(heaut_char_spec_v_df,
                            c(type, degree, weightedDegree,
                              closeness, betweenness,
                              local_clustering, triangles))
show(heaut_char_net_df)
####arrange rows by degree
heaut_char_net_df <- arrange(heaut_char_net_df,
                             desc(degree),
                             desc(weightedDegree))
show(heaut_char_net_df)
####create table with gt
gt_heaut_char_net <- gt::gt(heaut_char_net_df, rownames_to_stub = TRUE)
show(gt_heaut_char_net)
####layout table
gt_heaut_char_net <-
  gt_heaut_char_net %>%
  #####add header
  tab_header(
    title = "Heautontimoroumenos",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_heaut_char_net <-
  gt_heaut_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Syrus")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Syrus"))
####show final layout
show(gt_heaut_char_net)
####export table
gtsave(gt_heaut_char_net, "heaut_char_net.png", zoom = 10)

##[4.24.4.] create network graph

###[4.24.4.1.] delete node(s) without edges
show(V(heaut_coocur))
heaut_coocur <- heaut_coocur - c("Prologus")
show(V(heaut_coocur))
####control
show(heaut_coocur)

###[4.24.4.2.] layout network graph
###choose layout algorithm for graph
heaut_coocur_layout <- create_layout(heaut_coocur, layout = "stress")
###layout settings
heaut_coocur_layout <-
  ggraph(heaut_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(heaut_coocur_layout)
###export graph
ggsave(heaut_coocur_layout,
       file = "heaut_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[4.25.] Terence, Hecyra (Ter. Hec.): no schemers

#[4.26.] Terence, Phormio (Ter. Phorm.)

##[4.26.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
phorm <- get_play_metadata(play = "terence-phormio", 
                           corpus = "rom",
                           full_metadata = TRUE)
show(phorm)
####create matrix with general values for the dramatic network
phorm_gen_net_struc <- matrix(c(phorm$size, phorm$density, phorm$diameter,
                                phorm$averageClustering,
                                phorm$averagePathLength, phorm$averageDegree),
                              ncol = 1, byrow = FALSE)
###convert matrix to data frame
phorm_gen_net_struc_df <- as.data.frame(phorm_gen_net_struc)
###specify columns and rows for the data frame
colnames(phorm_gen_net_struc_df) <- c("value")
rownames(phorm_gen_net_struc_df) <- c("size", "density", "diameter",
                                      "average clustering",
                                      "average path length", "average degree")
show(phorm_gen_net_struc_df)
###create table with gt
gt_phorm_gen_net_struc <- gt::gt(phorm_gen_net_struc_df,
                                 rownames_to_stub = TRUE)
show(gt_phorm_gen_net_struc)
####amend table
gt_phorm_gen_net_struc <-
  gt_phorm_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Phormio",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_phorm_gen_net_struc)
####export table
gtsave(gt_phorm_gen_net_struc, "phorm_gen_net_struc.png", zoom = 10)

##[4.26.2.] extract, calculate, and add character-specific values
phorm_coocur <- get_net_cooccur_igraph(play = "terence-phormio", 
                                       corpus = "rom")
class(phorm_coocur)
###calculate local clustering
phorm_local_clustering <- transitivity(
  phorm_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
phorm_coocur <- set_vertex_attr(phorm_coocur, "local_clustering",
                                value = phorm_local_clustering)
###calculate triangles
phorm_triangles <- count_triangles(phorm_coocur)
###add triangles
phorm_coocur <- set_vertex_attr(phorm_coocur, "triangles",
                                value = phorm_triangles)
###add type manually, reference: cast list in Parry’s edition
###changed adolescens to adulescens, omnes to caterva
V(phorm_coocur)$name
phorm_coocur <- set_vertex_attr(phorm_coocur, "type",
                                value=c("prologus", "servus", "servus",
                                        "adulescens", "adulescens", "senex",
                                        "parasitus", "advocatus", "advocatus",
                                        "advocatus", "leno", "senex",
                                        "nutrix", "matrona", "caterva"))
###show data
show(phorm_coocur)
class(phorm_coocur)
###export as data frame
phorm_char_spec_v_df <- as_data_frame(phorm_coocur, what="vertices")
show(phorm_char_spec_v_df)

##[4.26.3.] create table with character-specific network values
##extract table with character-specific network values
phorm_char_net_df <- select(phorm_char_spec_v_df,
                            c(type, degree, weightedDegree,
                              closeness, betweenness,
                              local_clustering, triangles))
show(phorm_char_net_df)
####arrange rows by degree
phorm_char_net_df <- arrange(phorm_char_net_df,
                             desc(degree),
                             desc(weightedDegree))
show(phorm_char_net_df)
####create table with gt
gt_phorm_char_net <- gt::gt(phorm_char_net_df, rownames_to_stub = TRUE)
show(gt_phorm_char_net)
####layout table
gt_phorm_char_net <-
  gt_phorm_char_net %>%
  #####add header
  tab_header(
    title = "Phormio",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_phorm_char_net <-
  gt_phorm_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Geta")) %>%
  #####bold row showing the scheming slave (servus callidus)
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Geta"))
####show final layout
show(gt_phorm_char_net)
####export table
gtsave(gt_phorm_char_net, "phorm_char_net.png", zoom = 10)

##[4.26.4.] create network graph

###[4.26.4.1.] delete node(s) without edges
show(V(phorm_coocur))
phorm_coocur <- phorm_coocur - c("Prologus")
show(V(phorm_coocur))
####control
show(phorm_coocur)

###[4.26.4.2.] layout network graph
###choose layout algorithm for graph
phorm_coocur_layout <- create_layout(phorm_coocur, layout = "stress")
###layout settings
phorm_coocur_layout <-
  ggraph(phorm_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(phorm_coocur_layout)
###export graph
ggsave(phorm_coocur_layout,
       file = "phorm_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")
