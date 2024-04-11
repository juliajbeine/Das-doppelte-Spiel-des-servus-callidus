#Das doppelte Spiel des servus callidus:
#Eine poetologische und gesellschaftliche Reflexionsfigur auf den europäischen
#Bühnen der Frühen Neuzeit
#Part III: Classical receptions (via ezlinavis)
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

#[2.] analyse the selected classical receptions (in chronological order)

##[2.1.] Macropedius, Bassarus (1540)

###[2.1.1.] prepare data
####import data from ezlinavis
####note: change the headings in the files to lower case
####(source, type, target, weight) to make the calculations of the centralities
####work correctly (they do not take "Weight" for "weight")
bassarus_edges <-
  read_csv("2024-04-10 Ezlinavis/Macropedius_Bassarus_ezlinavis_lower case.csv")
show(bassarus_edges)
####delete edge type
bassarus_edges_new_df <- bassarus_edges[, -2]
####create igraph
bassarus_coocur <- graph_from_data_frame(bassarus_edges_new_df,
                                         directed = FALSE)
show(bassarus_coocur)
class(bassarus_coocur)
####add characters without edges that were not included by ezlinavis:
####Prologus, Chorus ex morionibus
bassarus_coocur <- bassarus_coocur %>%
  igraph::add.vertices(1, name = "Prologus") %>%
  igraph::add.vertices(1, name = "Chorus ex morionibus")
E(bassarus_coocur)$weight
####add type manually, reference: cast list
####changed uxor to matrona changed liberi to filius respectively
V(bassarus_coocur)$name
bassarus_coocur <- igraph::set_vertex_attr(bassarus_coocur, "type",
                                           value=c(
                                             "aerarius", "matrona", "custos",
                                              "puella", "parasitus", "serva",
                                              "servus", "praeses", "anus",
                                              "filius", "pastor", "matrona",
                                              "filius", "prologus", "chorus"))

###[2.1.2.] calculate character-specific network data
####calculate degree centrality
bassarus_degree <- igraph::degree(bassarus_coocur)
####add degree centrality
bassarus_coocur <- igraph::set_vertex_attr(bassarus_coocur, "degree",
                                           value = bassarus_degree)
V(bassarus_coocur)$degree
####calculate weighted degree centrality
bassarus_weighted_degree <- igraph::strength(bassarus_coocur)
####add weighted degree centrality
bassarus_coocur <- igraph::set_vertex_attr(bassarus_coocur, "weighted_degree",
                                           value = bassarus_weighted_degree)
V(bassarus_coocur)$weighted_degree
####calculate closeness centrality with sna (Freeman)
bassarus_closeness_sna <-
  sna::closeness(intergraph::asNetwork(bassarus_coocur))
show(bassarus_closeness_sna)
####add closeness centrality
bassarus_coocur <- igraph::set_vertex_attr(bassarus_coocur, "closeness",
                                           value = bassarus_closeness_sna)
V(bassarus_coocur)$closeness
####calculate betweenness centrality with sna (Brandes)
####no rescale because all values are 0
bassarus_betweenness_sna <-
  sna::betweenness(intergraph::asNetwork(bassarus_coocur))
show(bassarus_betweenness_sna)
####add betweenness centrality
bassarus_coocur <- igraph::set_vertex_attr(bassarus_coocur, "betweenness",
                                           value = bassarus_betweenness_sna)
####calculate local clustering
bassarus_local_clustering <- igraph::transitivity(
  bassarus_coocur,
  type = "local",
  isolates = c("NaN"))
####add local clustering
bassarus_coocur <- igraph::set_vertex_attr(bassarus_coocur, "local_clustering",
                                           value = bassarus_local_clustering)
####calculate triangles
bassarus_triangles <- igraph::count_triangles(bassarus_coocur)
####add triangles
bassarus_coocur <- igraph::set_vertex_attr(bassarus_coocur, "triangles",
                                           value = bassarus_triangles)
####show data
show(bassarus_coocur)
class(bassarus_coocur)
####export as data frame
bassarus_char_spec_v_df <- as_data_frame(bassarus_coocur, what="vertices")
show(bassarus_char_spec_v_df)

###[2.1.3.] calculate general network data
####calculate size
bassarus_size <- igraph::vcount(bassarus_coocur)
####calculate density
bassarus_density <- igraph::edge_density(bassarus_coocur)
####calculate diameter
bassarus_diameter <- igraph::diameter(bassarus_coocur, weights = NA)
####calculate average clustering
bassarus_av_cc <- igraph::transitivity(bassarus_coocur, type = "average")
####calculate average path length
bassarus_av_pl <- igraph::average.path.length(bassarus_coocur, weights = NA)
####calculate average degree
bassarus_av_d <- mean(bassarus_degree)

###[2.1.4.] create table with general network data
####create matrix with general values for the dramatic network
bassarus_gen_net_struc <- matrix(c(bassarus_size, bassarus_density,
                                   bassarus_diameter, bassarus_av_cc,
                                   bassarus_av_pl, bassarus_av_d),
                                 ncol = 1, byrow = FALSE)
###convert matrix to data frame
bassarus_gen_net_struc_df <- as.data.frame(bassarus_gen_net_struc)
###specify columns and rows for the data frame
colnames(bassarus_gen_net_struc_df) <- c("value")
rownames(bassarus_gen_net_struc_df) <- c("size", "density", "diameter",
                                         "average clustering",
                                         "average path length",
                                         "average degree")
show(bassarus_gen_net_struc_df)
####create table with gt
gt_bassarus_gen_net_struc <- gt::gt(bassarus_gen_net_struc_df,
                                    rownames_to_stub = TRUE)
show(gt_bassarus_gen_net_struc)
####amend table
gt_bassarus_gen_net_struc <-
  gt_bassarus_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Macropedius: Bassarus",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_bassarus_gen_net_struc)
####export table
gtsave(gt_bassarus_gen_net_struc, "bassarus_gen_net_struc.png", zoom = 10)

###[2.1.5.] create table with character-specific network data
###extract table with character-specific network values
bassarus_char_net_df <- bassarus_char_spec_v_df
####arrange rows by degree
bassarus_char_net_df <- arrange(bassarus_char_net_df, 
                                desc(degree),
                                desc(weighted_degree))
show(bassarus_char_net_df)
####create table with gt
gt_bassarus_char_net <- gt::gt(bassarus_char_net_df, rownames_to_stub = FALSE)
show(gt_bassarus_char_net)
####layout table
gt_bassarus_char_net <-
  gt_bassarus_char_net %>%
  #####add header
  tab_header(
    title = "Macropedius: Bassarus",
    subtitle = "network measures") %>%
  #####relabel columns
  cols_label(name = "character",
             degree = "d.c.",
             weighted_degree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "type",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_bassarus_char_net <-
  gt_bassarus_char_net %>%
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
              rows = 1)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = 1))
####show final layout
show(gt_bassarus_char_net)
#####set margin to prevent cut off
gt_bassarus_char_net <-
  gt_bassarus_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_bassarus_char_net)
####export table
gtsave(gt_bassarus_char_net, "bassarus_char_net.png", zoom = 10)

##[2.1.6.] create network graph

###[2.1.6.1.] delete node(s) without edges
show(V(bassarus_coocur))
bassarus_coocur <- bassarus_coocur - c("Prologus", "Chorus ex morionibus")
show(V(bassarus_coocur))
####control
show(bassarus_coocur)

###[2.1.6.2.] layout network graph
###choose layout algorithm for graph
bassarus_coocur_layout <- create_layout(bassarus_coocur, layout = "stress")
###layout settings
bassarus_coocur_layout <-
  ggraph(bassarus_coocur_layout) +
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
                      size = weighted_degree)) +
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
                     size = (weighted_degree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(bassarus_coocur_layout)
###export graph
ggsave(bassarus_coocur_layout,
       file = "bassarus_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

##[2.2.] Dryden, Amphitryon (1690)

###[2.2.1.] prepare data
####import data from ezlinavis
####note: change the headings in the files to lower case
####(source, type, target, weight) to make the calculations of the centralities
####work correctly (they do not take "Weight" for "weight")
dry_amphitryon_edges <-
  read_csv("2024-04-10 Ezlinavis/Dryden_Amphitryon_ezlinavis_lower case.csv")
show(dry_amphitryon_edges)
####delete edge type
dry_amphitryon_edges_new_df <- dry_amphitryon_edges[, -2]
####create igraph
dry_amphitryon_coocur <- graph_from_data_frame(dry_amphitryon_edges_new_df,
                                         directed = FALSE)
show(dry_amphitryon_coocur)
class(dry_amphitryon_coocur)

###[2.2.2.] calculate character-specific network data
####calculate degree centrality
dry_amphitryon_degree <- igraph::degree(dry_amphitryon_coocur)
####add degree centrality
dry_amphitryon_coocur <- igraph::set_vertex_attr(dry_amphitryon_coocur,
                                                 "degree",
                                           value = dry_amphitryon_degree)
V(dry_amphitryon_coocur)$degree
####calculate weighted degree centrality
dry_amphitryon_weighted_degree <- igraph::strength(dry_amphitryon_coocur)
####add weighted degree centrality
dry_amphitryon_coocur <- igraph::set_vertex_attr(dry_amphitryon_coocur,
                                                 "weighted_degree",
                                           value = 
                                             dry_amphitryon_weighted_degree)
V(dry_amphitryon_coocur)$weighted_degree
####calculate closeness centrality with sna (Freeman)
dry_amphitryon_closeness_sna <-
  sna::closeness(intergraph::asNetwork(dry_amphitryon_coocur))
show(dry_amphitryon_closeness_sna)
####add closeness centrality
dry_amphitryon_coocur <- igraph::set_vertex_attr(dry_amphitryon_coocur,
                                                 "closeness",
                                           value = dry_amphitryon_closeness_sna)
V(dry_amphitryon_coocur)$closeness
####calculate betweenness centrality with sna (Brandes)
dry_amphitryon_betweenness_sna <-
  sna::betweenness(intergraph::asNetwork(dry_amphitryon_coocur), rescale = TRUE)
show(dry_amphitryon_betweenness_sna)
####add betweenness centrality
dry_amphitryon_coocur <- igraph::set_vertex_attr(dry_amphitryon_coocur,
                                                 "betweenness",
                                           value = 
                                             dry_amphitryon_betweenness_sna)
####calculate local clustering
dry_amphitryon_local_clustering <- igraph::transitivity(
  dry_amphitryon_coocur,
  type = "local",
  isolates = c("NaN"))
####add local clustering
dry_amphitryon_coocur <- igraph::set_vertex_attr(dry_amphitryon_coocur,
                                                 "local_clustering",
                                           value = 
                                             dry_amphitryon_local_clustering)
####calculate triangles
dry_amphitryon_triangles <- igraph::count_triangles(dry_amphitryon_coocur)
####add triangles
dry_amphitryon_coocur <- igraph::set_vertex_attr(dry_amphitryon_coocur,
                                                 "triangles",
                                           value = dry_amphitryon_triangles)
####show data
show(dry_amphitryon_coocur)
class(dry_amphitryon_coocur)
####export as data frame
dry_amphitryon_char_spec_v_df <- as_data_frame(dry_amphitryon_coocur,
                                               what="vertices")
show(dry_amphitryon_char_spec_v_df)

###[2.2.3.] calculate general network data
####calculate size
dry_amphitryon_size <- igraph::vcount(dry_amphitryon_coocur)
####calculate density
dry_amphitryon_density <- igraph::edge_density(dry_amphitryon_coocur)
####calculate diameter
dry_amphitryon_diameter <- igraph::diameter(dry_amphitryon_coocur,
                                            weights = NA)
####calculate average clustering
dry_amphitryon_av_cc <- igraph::transitivity(dry_amphitryon_coocur,
                                             type = "average")
####calculate average path length
dry_amphitryon_av_pl <- igraph::average.path.length(dry_amphitryon_coocur,
                                                    weights = NA)
####calculate average degree
dry_amphitryon_av_d <- mean(dry_amphitryon_degree)

###[2.2.4.] create table with general network data
####create matrix with general values for the dramatic network
dry_amphitryon_gen_net_struc <- matrix(c(dry_amphitryon_size,
                                         dry_amphitryon_density,
                                         dry_amphitryon_diameter,
                                         dry_amphitryon_av_cc,
                                         dry_amphitryon_av_pl,
                                         dry_amphitryon_av_d),
                                 ncol = 1, byrow = FALSE)
###convert matrix to data frame
dry_amphitryon_gen_net_struc_df <- as.data.frame(dry_amphitryon_gen_net_struc)
###specify columns and rows for the data frame
colnames(dry_amphitryon_gen_net_struc_df) <- c("value")
rownames(dry_amphitryon_gen_net_struc_df) <- c("size", "density", "diameter",
                                         "average clustering",
                                         "average path length",
                                         "average degree")
show(dry_amphitryon_gen_net_struc_df)
####create table with gt
gt_dry_amphitryon_gen_net_struc <- gt::gt(dry_amphitryon_gen_net_struc_df,
                                    rownames_to_stub = TRUE)
show(gt_dry_amphitryon_gen_net_struc)
####amend table
gt_dry_amphitryon_gen_net_struc <-
  gt_dry_amphitryon_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Dryden: Amphitryon",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_dry_amphitryon_gen_net_struc)
####export table
gtsave(gt_dry_amphitryon_gen_net_struc, "dry_amphitryon_gen_net_struc.png",
       zoom = 10)

###[2.2.5.] create table with character-specific network data
###extract table with character-specific network values
dry_amphitryon_char_net_df <- dry_amphitryon_char_spec_v_df
####arrange rows by degree
dry_amphitryon_char_net_df <- arrange(dry_amphitryon_char_net_df, 
                                desc(degree),
                                desc(weighted_degree))
show(dry_amphitryon_char_net_df)
####create table with gt
gt_dry_amphitryon_char_net <- gt::gt(dry_amphitryon_char_net_df,
                                     rownames_to_stub = FALSE)
show(gt_dry_amphitryon_char_net)
####layout table
gt_dry_amphitryon_char_net <-
  gt_dry_amphitryon_char_net %>%
  #####add header
  tab_header(
    title = "Dryden: Amphitryon",
    subtitle = "network measures") %>%
  #####relabel columns
  cols_label(name = "character",
             degree = "d.c.",
             weighted_degree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "name",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_dry_amphitryon_char_net <-
  gt_dry_amphitryon_char_net %>%
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
              rows = 2)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = 2))
####show final layout
show(gt_dry_amphitryon_char_net)
#####set margin to prevent cut off
gt_dry_amphitryon_char_net <-
  gt_dry_amphitryon_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_dry_amphitryon_char_net)
####export table
gtsave(gt_dry_amphitryon_char_net, "dry_amphitryon_char_net.png", zoom = 10)

##[2.2.6.] create network graph

###[2.2.6.1.] layout network graph
###choose layout algorithm for graph
dry_amphitryon_coocur_layout <- create_layout(dry_amphitryon_coocur,
                                              layout = "stress")
###layout settings
dry_amphitryon_coocur_layout <-
  ggraph(dry_amphitryon_coocur_layout) +
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
                      size = weighted_degree)) +
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
                     size = (weighted_degree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(dry_amphitryon_coocur_layout)
###export graph
ggsave(dry_amphitryon_coocur_layout,
       file = "dry_amphitryon_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

##[2.3.] Fielding, The Intriguing Chambermaid (1734)

###[2.3.1.] prepare data
####import data from ezlinavis
####note: change the headings in the files to lower case
####(source, type, target, weight) to make the calculations of the centralities
####work correctly (they do not take "Weight" for "weight")
chambermaid_edges <-
  read_csv("2024-04-10 Ezlinavis/Fielding_Chambermaid_ezlinavis_lower case.csv")
show(chambermaid_edges)
####delete edge type
chambermaid_edges_new_df <- chambermaid_edges[, -2]
####create igraph
chambermaid_coocur <- graph_from_data_frame(chambermaid_edges_new_df,
                                               directed = FALSE)
show(chambermaid_coocur)
class(chambermaid_coocur)

###[2.3.2.] calculate character-specific network data
####calculate degree centrality
chambermaid_degree <- igraph::degree(chambermaid_coocur)
####add degree centrality
chambermaid_coocur <- igraph::set_vertex_attr(chambermaid_coocur,
                                                 "degree",
                                                 value = chambermaid_degree)
V(chambermaid_coocur)$degree
####calculate weighted degree centrality
chambermaid_weighted_degree <- igraph::strength(chambermaid_coocur)
####add weighted degree centrality
chambermaid_coocur <- igraph::set_vertex_attr(chambermaid_coocur,
                                                 "weighted_degree",
                                                 value = 
                                                   chambermaid_weighted_degree)
V(chambermaid_coocur)$weighted_degree
####calculate closeness centrality with sna (Freeman)
chambermaid_closeness_sna <-
  sna::closeness(intergraph::asNetwork(chambermaid_coocur))
show(chambermaid_closeness_sna)
####add closeness centrality
chambermaid_coocur <- igraph::set_vertex_attr(chambermaid_coocur,
                                                 "closeness",
                                                 value = 
                                                chambermaid_closeness_sna)
V(chambermaid_coocur)$closeness
####calculate betweenness centrality with sna (Brandes)
chambermaid_betweenness_sna <-
  sna::betweenness(intergraph::asNetwork(chambermaid_coocur), rescale = TRUE)
show(chambermaid_betweenness_sna)
####add betweenness centrality
chambermaid_coocur <- igraph::set_vertex_attr(chambermaid_coocur,
                                                 "betweenness",
                                                 value = 
                                                   chambermaid_betweenness_sna)
####calculate local clustering
chambermaid_local_clustering <- igraph::transitivity(
  chambermaid_coocur,
  type = "local",
  isolates = c("NaN"))
####add local clustering
chambermaid_coocur <- igraph::set_vertex_attr(chambermaid_coocur,
                                                 "local_clustering",
                                                 value = 
                                                   chambermaid_local_clustering)
####calculate triangles
chambermaid_triangles <- igraph::count_triangles(chambermaid_coocur)
####add triangles
chambermaid_coocur <- igraph::set_vertex_attr(chambermaid_coocur,
                                                 "triangles",
                                                 value = chambermaid_triangles)
####show data
show(chambermaid_coocur)
class(chambermaid_coocur)
####export as data frame
chambermaid_char_spec_v_df <- as_data_frame(chambermaid_coocur,
                                               what="vertices")
show(chambermaid_char_spec_v_df)

###[2.3.3.] calculate general network data
####calculate size
chambermaid_size <- igraph::vcount(chambermaid_coocur)
####calculate density
chambermaid_density <- igraph::edge_density(chambermaid_coocur)
####calculate diameter
chambermaid_diameter <- igraph::diameter(chambermaid_coocur,
                                            weights = NA)
####calculate average clustering
chambermaid_av_cc <- igraph::transitivity(chambermaid_coocur,
                                             type = "average")
####calculate average path length
chambermaid_av_pl <- igraph::average.path.length(chambermaid_coocur,
                                                    weights = NA)
####calculate average degree
chambermaid_av_d <- mean(chambermaid_degree)

###[2.3.4.] create table with general network data
####create matrix with general values for the dramatic network
chambermaid_gen_net_struc <- matrix(c(chambermaid_size,
                                         chambermaid_density,
                                         chambermaid_diameter,
                                         chambermaid_av_cc,
                                         chambermaid_av_pl,
                                         chambermaid_av_d),
                                       ncol = 1, byrow = FALSE)
###convert matrix to data frame
chambermaid_gen_net_struc_df <- as.data.frame(chambermaid_gen_net_struc)
###specify columns and rows for the data frame
colnames(chambermaid_gen_net_struc_df) <- c("value")
rownames(chambermaid_gen_net_struc_df) <- c("size", "density", "diameter",
                                               "average clustering",
                                               "average path length",
                                               "average degree")
show(chambermaid_gen_net_struc_df)
####create table with gt
gt_chambermaid_gen_net_struc <- gt::gt(chambermaid_gen_net_struc_df,
                                          rownames_to_stub = TRUE)
show(gt_chambermaid_gen_net_struc)
####amend table
gt_chambermaid_gen_net_struc <-
  gt_chambermaid_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Fielding: The Intriguing Chambermaid",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_chambermaid_gen_net_struc)
####export table
gtsave(gt_chambermaid_gen_net_struc, "chambermaid_gen_net_struc.png",
       zoom = 10)

###[2.3.5.] create table with character-specific network data
###extract table with character-specific network values
chambermaid_char_net_df <- chambermaid_char_spec_v_df
####arrange rows by degree
chambermaid_char_net_df <- arrange(chambermaid_char_net_df, 
                                      desc(degree),
                                      desc(weighted_degree))
show(chambermaid_char_net_df)
####create table with gt
gt_chambermaid_char_net <- gt::gt(chambermaid_char_net_df,
                                     rownames_to_stub = FALSE)
show(gt_chambermaid_char_net)
####layout table
gt_chambermaid_char_net <-
  gt_chambermaid_char_net %>%
  #####add header
  tab_header(
    title = "Fielding: The Intriguing Chambermaid",
    subtitle = "network measures") %>%
  #####relabel columns
  cols_label(name = "character",
             degree = "d.c.",
             weighted_degree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.") %>%
  #####set vertical line
  gt_add_divider(columns = "name",
                 color = "lightgrey",
                 style = "solid")
#####colour the table
gt_chambermaid_char_net <-
  gt_chambermaid_char_net %>%
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
              rows = 2)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = 2))
####show final layout
show(gt_chambermaid_char_net)
#####set margin to prevent cut off
gt_chambermaid_char_net <-
  gt_chambermaid_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_chambermaid_char_net)
####export table
gtsave(gt_chambermaid_char_net, "chambermaid_char_net.png", zoom = 10)

##[2.3.6.] create network graph

###[2.3.6.1.] layout network graph
###choose layout algorithm for graph
chambermaid_coocur_layout <- create_layout(chambermaid_coocur,
                                              layout = "stress")
###layout settings
chambermaid_coocur_layout <-
  ggraph(chambermaid_coocur_layout) +
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
                      size = weighted_degree)) +
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
                     size = (weighted_degree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(chambermaid_coocur_layout)
###export graph
ggsave(chambermaid_coocur_layout,
       file = "chambermaid_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")
