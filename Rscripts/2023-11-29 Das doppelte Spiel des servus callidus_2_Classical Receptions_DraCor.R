#Das doppelte Spiel des servus callidus:
#Eine poetologische und gesellschaftliche Reflexionsfigur auf den europäischen
#Bühnen der Frühen Neuzeit
#Part II: Classical receptions (via DraCor)
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
library(RColorBrewer)
library(extrafont)
library(visNetwork)
library(network)
library(scales)
library(gtExtras)
library(graphlayouts)
library(ggraph)
library(ggforce)
library(readr)
library(sna)
library(intergraph)

#[2.] get information on the version of the DraCor API
dracor_api_info()

#[3.] get information about the corpora used, including the last update
meta <- get_dracor_meta()
##convert tibble to data frame
meta_df <- as.data.frame(meta)
show(meta_df)
##delete columns and rows not needed for quotation
filt_meta_df <- meta_df %>%
  filter(title == "French Drama Corpus") %>%
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
    title = "Information on FreDraCor")
show(gt_filt_meta)
##export table
gt_filt_meta_png <-gt::gtsave(gt_filt_meta, "Info_FreDraCor.png",
                              vwidth = 2000,
                              vheight = 1000,
                              zoom = 10)

#[4.] analyse the selected classical receptions (in chronological order)

#[4.1.] Molière, Amphitryon (1668)

##[4.1.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
mol_amphitryon <- get_play_metadata(play = "moliere-amphitryon", 
                             corpus = "fre",
                             full_metadata = TRUE)
show(mol_amphitryon)
####create matrix with general values for the dramatic network
mol_amphitryon_gen_net_struc <- matrix(c(mol_amphitryon$size,
                                         mol_amphitryon$density,
                                         mol_amphitryon$diameter,
                                         mol_amphitryon$averageClustering,
                                         mol_amphitryon$averagePathLength,
                                         mol_amphitryon$averageDegree),
                                ncol = 1, byrow = FALSE)
###convert matrix to data frame
mol_amphitryon_gen_net_struc_df <- as.data.frame(mol_amphitryon_gen_net_struc)
###specify columns and rows for the data frame
colnames(mol_amphitryon_gen_net_struc_df) <- c("value")
rownames(mol_amphitryon_gen_net_struc_df) <- c("size", "density", "diameter",
                                        "average clustering",
                                        "average path length",
                                        "average degree")
show(mol_amphitryon_gen_net_struc_df)
###create table with gt
gt_mol_amphitryon_gen_net_struc <- gt::gt(mol_amphitryon_gen_net_struc_df,
                                   rownames_to_stub = TRUE)
show(gt_mol_amphitryon_gen_net_struc)
####amend table
gt_mol_amphitryon_gen_net_struc <-
  gt_mol_amphitryon_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Molière: Amphitryon",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_mol_amphitryon_gen_net_struc)
####export table
gtsave(gt_mol_amphitryon_gen_net_struc,
       "mol_amphitryon_gen_net_struc.png", zoom = 10)

##[4.1.2.] extract, calculate, and add character-specific values
mol_amphitryon_coocur <- get_net_cooccur_igraph(play =
                                           "moliere-amphitryon",
                                         corpus = "fre")
class(mol_amphitryon_coocur)
###calculate local clustering
mol_amphitryon_local_clustering <- transitivity(
  mol_amphitryon_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
mol_amphitryon_coocur <- set_vertex_attr(mol_amphitryon_coocur,
                                         "local_clustering",
                                  value = mol_amphitryon_local_clustering)
###calculate triangles
mol_amphitryon_triangles <- count_triangles(mol_amphitryon_coocur)
###add triangles
mol_amphitryon_coocur <- set_vertex_attr(mol_amphitryon_coocur, "triangles",
                                  value = mol_amphitryon_triangles)
###show data
show(mol_amphitryon_coocur)
class(mol_amphitryon_coocur)
###export as data frame
mol_amphitryon_char_spec_v_df <- as_data_frame(mol_amphitryon_coocur,
                                               what="vertices")
show(mol_amphitryon_char_spec_v_df)

##[4.1.3.] create table with character-specific network values
###extract table with character-specific network values
mol_amphitryon_char_net_df <- select(mol_amphitryon_char_spec_v_df,
                              c(degree, weightedDegree,
                                closeness, betweenness,
                                local_clustering, triangles))
show(mol_amphitryon_char_net_df)
####arrange rows by degree
mol_amphitryon_char_net_df <- arrange(mol_amphitryon_char_net_df, 
                               desc(degree),
                               desc(weightedDegree))
show(mol_amphitryon_char_net_df)
####create table with gt
gt_mol_amphitryon_char_net <- gt::gt(mol_amphitryon_char_net_df,
                                     rownames_to_stub = TRUE)
show(gt_mol_amphitryon_char_net)
####layout table
gt_mol_amphitryon_char_net <-
  gt_mol_amphitryon_char_net %>%
  #####add header
  tab_header(
    title = "Molière: Amphitryon",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_mol_amphitryon_char_net <-
  gt_mol_amphitryon_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Mercure")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Mercure"))
####show final layout
show(gt_mol_amphitryon_char_net)
#####set margin to prevent cut off
gt_mol_amphitryon_char_net <-
  gt_mol_amphitryon_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_mol_amphitryon_char_net)
####export table
gtsave(gt_mol_amphitryon_char_net, "mol_amphitryon_char_net.png", zoom = 10)

##[4.1.4.] create network graph

###[4.1.4.1.] layout network graph
###choose layout algorithm for graph
mol_amphitryon_coocur_layout <- create_layout(mol_amphitryon_coocur,
                                              layout = "stress")
###layout settings
mol_amphitryon_coocur_layout <-
  ggraph(mol_amphitryon_coocur_layout) +
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
show(mol_amphitryon_coocur_layout)
###export graph
ggsave(mol_amphitryon_coocur_layout,
       file = "mol_amphitryon_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.2.] Regnard, Le retour imprévu (1700)

##[4.2.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
retour <- get_play_metadata(play = "regnard-retour-imprevu", 
                            corpus = "fre",
                            full_metadata = TRUE)
show(retour)
####create matrix with general values for the dramatic network
retour_gen_net_struc <- matrix(c(retour$size, retour$density,
                                 retour$diameter, retour$averageClustering,
                                 retour$averagePathLength,
                                 retour$averageDegree),
                               ncol = 1, byrow = FALSE)
###convert matrix to data frame
retour_gen_net_struc_df <- as.data.frame(retour_gen_net_struc)
###specify columns and rows for the data frame
colnames(retour_gen_net_struc_df) <- c("value")
rownames(retour_gen_net_struc_df) <- c("size", "density", "diameter",
                                       "average clustering",
                                       "average path length", "average degree")
show(retour_gen_net_struc_df)
###create table with gt
gt_retour_gen_net_struc <- gt::gt(retour_gen_net_struc_df,
                                  rownames_to_stub = TRUE)
show(gt_retour_gen_net_struc)
####amend table
gt_retour_gen_net_struc <-
  gt_retour_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Regnard: Le retour imprévu",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_retour_gen_net_struc)
####export table
gtsave(gt_retour_gen_net_struc, "retour_gen_net_struc.png", zoom = 10)

##[4.2.2.] extract, calculate, and add character specific values
retour_coocur <- get_net_cooccur_igraph(play =
                                          "regnard-retour-imprevu",
                                        corpus = "fre")
class(retour_coocur)
###calculate local clustering
retour_local_clustering <- transitivity(
  retour_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
retour_coocur <- set_vertex_attr(retour_coocur, "local_clustering",
                                 value = retour_local_clustering)
###calculate triangles
retour_triangles <- count_triangles(retour_coocur)
###add triangles
retour_coocur <- set_vertex_attr(retour_coocur, "triangles",
                                 value = retour_triangles)
###show data
show(retour_coocur)
class(retour_coocur)
###export as data frame
retour_char_spec_v_df <- as_data_frame(retour_coocur, what="vertices")
show(retour_char_spec_v_df)

##[4.2.3.] create table with character specific network values
###extract table with character specific network values
retour_char_net_df <- select(retour_char_spec_v_df,
                             c(degree, weightedDegree,
                               closeness, betweenness,
                               local_clustering, triangles))
show(retour_char_net_df)
####arrange rows by degree
retour_char_net_df <- arrange(retour_char_net_df, 
                              desc(degree),
                              desc(weightedDegree))
show(retour_char_net_df)
####create table with gt
gt_retour_char_net <- gt::gt(retour_char_net_df, rownames_to_stub = TRUE)
show(gt_retour_char_net)
####layout table
gt_retour_char_net <-
  gt_retour_char_net %>%
  #####add header
  tab_header(
    title = "Regnard: Le retour imprévu",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_retour_char_net <-
  gt_retour_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Merlin")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Merlin"))
####show final layout
show(gt_retour_char_net)
#####set margin to prevent cut off
gt_retour_char_net <-
  gt_retour_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_retour_char_net)
####export table
gtsave(gt_retour_char_net, "retour_char_net.png", zoom = 10)

##[4.2.4.] create network graph

###[4.2.4.1.] layout network graph
###choose layout algorithm for graph
retour_coocur_layout <- create_layout(retour_coocur, layout = "stress")
###layout settings
retour_coocur_layout <-
  ggraph(retour_coocur_layout) +
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
show(retour_coocur_layout)
###export graph
ggsave(retour_coocur_layout,
       file = "retour_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")