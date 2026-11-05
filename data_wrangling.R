# Util Funs ----

tg_with_centrality <- function(
    nodes,
    edges
){
  if("entity" %in% names(nodes)){
    nodes <- nodes %>%
      dplyr::rename(name = entity)
  }

  if("ego" %in% names(edges)){
    edges <- edges %>%
      dplyr::rename(
        from = ego,
        to = alter
      )
  }

  if("count" %in% names(edges)){
    edges <- edges %>%
      dplyr::rename(
        weight = count
      )
  }

  g <- tidygraph::as_tbl_graph(x = edges,
                               # adding 'nodes' arg doesn't seem to do anything.
                               # all the composition attributes are dropped.
                               # and no 'id' column is created.
                               # nodes = nodes_,
                               directed = TRUE,
                               node_key = "name")

  g <- g %>%
    tidygraph::activate("nodes") %>%
    tidygraph::mutate(
      # docs: https://igraph.r-universe.dev/igraph/doc/manual.html#degree
      centrality_degree_norm = tidygraph::centrality_degree(
        mode = "all",
        normalized = TRUE,
        weights = NULL
      ),
      centrality_degree = tidygraph::centrality_degree(
        mode = "all",
        normalized = FALSE,
        weights = NULL
      ),
      centrality_strength = tidygraph::centrality_degree(
        mode = "all",
        normalized = FALSE,
        weights = weight
      ),
      centrality_eigen = tidygraph::centrality_eigen(
        directed = TRUE,
        weights = weight
        ),
      centrality_pagerank = tidygraph::centrality_pagerank(
        directed = TRUE,
        weights = weight
        ),
      centrality_betweenness = tidygraph::centrality_betweenness(
        weights = weight
      )
      ) %>%
    # rankings
    tidygraph::mutate(
      centrality_rank_degree_norm = dplyr::min_rank(dplyr::desc(centrality_degree_norm)),
      centrality_rank_strength = dplyr::min_rank(dplyr::desc(centrality_strength)),
      centrality_rank_eigen = dplyr::min_rank(dplyr::desc(centrality_eigen)),
      centrality_rank_pagerank = dplyr::min_rank(dplyr::desc(centrality_pagerank)),
      centrality_rank_betweenness = dplyr::min_rank(dplyr::desc(centrality_betweenness))
    ) %>%
    dplyr::left_join(
      y = nodes,
      by = "name"
    )

  gnodes <- g %>%
    tibble::as_tibble()

  cols_ <- names(gnodes)[!(names(gnodes) %in% names(nodes))]

  gnodes <- gnodes %>%
    dplyr::select(tidyselect::all_of(c("name", cols_)))

  nodes <- nodes %>%
    dplyr::left_join(
      y = gnodes,
      by = "name"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(centrality_rank = mean(
      dplyr::c_across(c(centrality_rank_eigen,
                        centrality_rank_betweenness)), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(centrality_rank = dplyr::min_rank(centrality_rank))

  # return
  list(
    nodes = nodes,
    edges = edges,
    g = g
  )
}

net_summary <- function(tg){
  g <- tg$g
  tb <- g %>%
    tidygraph::activate(nodes) %>%
    tibble::as_tibble()

  tibble::tribble(
    ~metric, ~value, ~`metric description`,
    "Nodes (n)", nrow(tg$nodes), "Number of organizations",
    "Observed Nodes (n)", igraph::vcount(g), "Number of observed organizations.",
    "Edges (n)", igraph::ecount(g), "Number of connections between organizations.",
    "Diameter", igraph::diameter(g), "\"The diameter of a connected network is the length of the longest shortest path\" (Shafie and Schoch)",
    "Distance (µ)", igraph::mean_distance(g), "\"The mean distance is the average shortest-path length over all reachable pairs of nodes. [Mean distance and diameter] quantify how \'spread out\' a network is: small values indicate that every node is a few steps from every other, a property often associated with small-world networks\" (Shafie and Schoch).",
    "Degree (µ)", mean(tb$centrality_degree, na.rm = TRUE), "\"The degree of a node is the number of its neighbors\" (Shafie and Schoch).",
    "Strength (µ)", mean(tb$centrality_strength, na.rm = TRUE), "Weighted degree (based on repeated respondent ties by organization).",
    "Eigenvector (µ)", mean(tb$centrality_eigen, na.rm = TRUE), "Is a node connected to other well-connected nodes? A node may have few ties, but if those ties are to important nodes, it is central by association (Shafie and Schoch).",
    "Density", igraph::edge_density(g, loops = TRUE), "\"The density of a network is the fraction of possible edges that are actually present. It lies between 0 (empty graph) and 1 (complete graph)\" (Shafie and Schoch).",
    "Transitivity", igraph::transitivity(g, type = "global"), "\"[T]ransitivity provides an intuitive interpretation of clustering: it reflects the tendency of networks to form cohesive, locally dense structures where indirect relationships become direct ties\" (Shafie and Schoch).",
  "Degree Assortativity", igraph::assortativity(g, igraph::degree(g), directed = TRUE), "\"Assortativity measures the tendency of nodes in a network to connect to other nodes that are similar to them according to some attribute. It provides insight into whether a network is structured around similarity (homophily) or difference (heterophily), which has important implications for processes such as information flow, contagion, and group formation... A negative value indicates disassortative mixing, meaning that highly connected nodes tend to connect to nodes with fewer connections, while a positive value indicates that nodes tend to connect to others with similar degree.\" (Shafie and Schoch).",
  "Strength Assortativity", igraph::assortativity(g, igraph::strength(g, mode = "all", loops = TRUE, weights = igraph::E(g)$weight), directed = TRUE), "Assortativity on strength (weighted degree).",
    "Nominal Assortativity of Organization Type", igraph::assortativity_nominal(g, igraph::V(g)$org_type_detail, directed = TRUE), "\"Assortativity can also be computed based on categorical (nominal) attributes\" (Shafie and Schoch). For this network, we measure the nominal assortativity of organization type, of which there are 24 categories (eg. State Government, Service Provider, etc).",
    "Reciprocity", igraph::reciprocity(g), "\"[T]he proportion of directed edges for which the reverse edge is also present. Reciprocity is a central concept in social exchange theory, where it reflects mutual exchange relationships between actors. Social interactions are often shaped by considerations of costs and benefits, and reciprocated ties indicate balanced or mutually beneficial relationships\" (Shafie and Schoch)."
  ) %>%
    dplyr::mutate(`value (3 sf)` = purrr::map_chr(
      .x = value,
      .f = ~formatC(.x, format = "fg", digits = 3))) %>%
    dplyr::select(metric, value, `value (3 sf)`, `metric description`)
}

# Useful Data Objects ----

area_key <- c(
  AG = "Agriculture",
  ECO = "Ecosystem",
  FL = "Flood Risk",
  WSR = "Water Supply Reliability"
)

pal_area <- palette.colors(
  palette = "Tableau 10"
) %>%
  `[`(c(7, 4, 8, 2)) %>%
  setNames(c("AG", "ECO", "FL", "WSR"))

pal_strategy <- c(
  AG = "Purp",
  ECO = "Teal",
  FL = "Burg",
  WSR = "Peach"
)

strategy_colors <- dat$scenario_scores_by_action %>%
  dplyr::select(strategy_id) %>%
  dplyr::mutate(area = stringr::str_extract(strategy_id, "^[:alpha:]{2,3}")) %>%
  dplyr::distinct() %>%
  dplyr::group_by(area) %>%
  dplyr::mutate(
    strategy_color = hcl.colors(
      n = dplyr::n(),
      palette = pal_strategy[which(names(pal_strategy) %in% dplyr::cur_group())]
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-area)

cb <- data.frame(
  level = c(
    "Directly Performed",
    "Partly Performed",
    "Primes/Supplements",
    "Needs Alteration",
    "Indirectly Related"
  ),
  score = c(
    1,
    0.75,
    0.5,
    0.25,
    0.2
  )
) %>%
  dplyr::arrange(score) %>%
  dplyr::mutate(level = forcats::fct(level))

# Remove alternate names from nodes (eg. "State Lands Commission" is removed
# because the full title is "California State Lands Commission")
jt <- dat$jt_nodes %>%
  dplyr::filter(is_alt_name %in% c(NA, FALSE)) %>%
  # some entities have multiple org_types, so pick first one.
  dplyr::mutate(
    dplyr::across(.cols = tidyselect::starts_with("org_type"),
                  .fns = function(vec){
                    purrr::map_chr(.x = vec,
                                   .f = function(x){
                                     stringr::str_split_1(x, "\\;") %>%
                                       `[`(1)
                                   })
                  })
  ) %>%
  dplyr::select(-is_alt_name)

jt_edges <- dat$pozzi_edges %>%
  dplyr::mutate(from = purrr::map_chr(.x = ego,
                                      .f = function(x){
                                        jt$entity[jt$jtid == x]
                                      })) %>%
  dplyr::mutate(to = purrr::map_chr(.x = alter,
                                    .f = function(x){
                                      jt$entity[jt$jtid == x]
                                    })) %>%
  dplyr::rename(weight = count) %>%
  dplyr::select(from, to, weight)

# adaptation plan scenario scores w/ org names
sc <- dat$scenario_scores_by_jtid %>%
  dplyr::left_join(y = jt,
                   by = "jtid") %>%
  dplyr::arrange(desc(jtda_score))

max_points <- dat$scenario_scores_by_action %>%
  dplyr::summarize(.by = strategy_id, max_points = dplyr::n()) %>%
  dplyr::mutate(strategy_id = forcats::as_factor(strategy_id))

tg1 <- tg_with_centrality(nodes = jt, edges = jt_edges)

# table of jt actions only.
jt_actions_only <- dat$scenario_scores_by_action %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`Total JT Score` = dplyr::c_across(tidyselect::all_of(snm)) |>
                  sum(na.rm = TRUE)
  ) %>%
  dplyr::filter(`Total JT Score` > 0)

# General Network descriptives ----

g_summary <- net_summary(tg1)

# strategies by scenarios ----
strat_by_scenario <- dat$scenario_scores_by_action %>%
  dplyr::summarize(.by = "strategy_id",
                   dplyr::across(
                     .cols = tidyselect::all_of(snm),
                     .fns = ~sum(.x, na.rm = TRUE)
                   )
  )

strat_roles <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    df <- strat_by_scenario %>%
      dplyr::select(tidyselect::all_of(c("strategy_id", scenario_))) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(scenario_),
        names_to = "scenario",
        values_to = "jtda_score"
      )

    dat$da_strategies_orgs %>%
      dplyr::select(-strategy_desc, -parenthetical, -entity) %>%
      dplyr::left_join(
        y = df,
        by = "strategy_id"
      ) %>%
      dplyr::filter(jtda_score > 0) %>%
      dplyr::summarize(
        .by = c(strategy_role_type, jtid),
        jtda_score = sum(jtda_score, na.rm = TRUE)
      ) %>%
      dplyr::arrange(jtid)
  }
) %>%
  setNames(snm)

# Top Actions ----
# top quartile of actions of SCENARIO
top_actions_of <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    df <- dat$scenario_scores_by_action %>%
      dplyr::rename(score := !!sym(scenario_)) %>%
      dplyr::mutate(min_rank = dplyr::min_rank(dplyr::desc(score)))

    threshold <- df$score %>%
      quantile() %>%
      `[`(4)

    df <- df %>%
      dplyr::filter(score >= threshold)

    df %>%
      dplyr::arrange(min_rank) %>%
      dplyr::select(action, score, min_rank) %>%
      dplyr::filter(score > 0)
  }
) %>%
  setNames(snm)

# top quartile of actions of ALL OTHER SCENARIOS
top_actions_of_others_relative_to <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    snm <- snm[snm != scenario_]

    ta_ <- dat$scenario_scores_by_action %>%
      dplyr::select(-{{scenario_}}) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        total_score = sum(dplyr::c_across(tidyselect::all_of(snm)), na.rm = FALSE),
        sd = sd(dplyr::c_across(tidyselect::all_of(snm)), na.rm = FALSE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(total_score, sd) %>%
      dplyr::mutate(min_rank = dplyr::min_rank(dplyr::desc(total_score)))

    cutoff <- ta_$total_score %>%
      quantile() %>%
      `[`(4)

    ta_ <- ta_ %>%
      dplyr::filter(total_score >= cutoff)

    ta_ %>%
      dplyr::arrange(min_rank) %>%
      dplyr::select(action, total_score, min_rank) %>%
      dplyr::filter(total_score > 0)
  }
) %>%
  setNames(snm)

# unique actions for scenario
unique_actions <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    # top actions of other all other scenarios
    top_o <- top_actions_of_others_relative_to[[scenario_]]
    # top actions of this scenario
    top_sc <- top_actions_of[[scenario_]]

    top_sc %>%
      dplyr::filter(!(action %in% top_o$action))  %>%
      dplyr::filter(score > 0)
  }
) %>%
  setNames(snm)

# absent actions for scenario
absent_actions <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    # top actions of other all other scenarios
    top_o <- top_actions_of_others_relative_to[[scenario_]]
    # top actions of this scenario
    top_sc <- top_actions_of[[scenario_]]

    top_o %>%
      dplyr::filter(!(action %in% top_sc$action)) %>%
      dplyr::filter(total_score > 0)
  }
) %>%
  setNames(snm)

# ranking of actions by scenario
actions_min_ranked <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    dat$scenario_scores_by_action %>%
      dplyr::select(action, tidyselect::all_of(scenario_)) %>%
      dplyr::rename(score := !!sym(scenario_)) %>%
      dplyr::arrange(dplyr::desc(score)) %>%
      dplyr::mutate(min_rank = dplyr::min_rank(dplyr::desc(score)))
  }
) %>%
  setNames(snm)

# overall top actions
overall_top_actions <- dat$scenario_scores_by_action %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    total_score = sum(dplyr::c_across(tidyselect::all_of(snm)), na.rm = FALSE),
    sd = sd(dplyr::c_across(tidyselect::all_of(snm)), na.rm = FALSE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(total_score, sd)


cutoff <- overall_top_actions$total_score %>%
  quantile() %>%
  `[`(4)

overall_top_actions <- overall_top_actions %>%
  dplyr::filter(total_score >= cutoff)

overall_top_actions <- overall_top_actions %>%
  dplyr::mutate(min_rank = dplyr::min_rank(dplyr::desc(total_score))) %>%
  dplyr::arrange(min_rank) %>%
  dplyr::select(action, total_score, sd, min_rank)

# top actions by scenario

# Top Actors ----

top_quintile_actors_of_scenario <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    dat$scenario_scores_by_jtid %>%
      dplyr::filter(scenario == scenario_) %>%
      dplyr::filter(jtda_score > quantile(jtda_score,
                                          probs = seq(0, 1, 0.2))[5]) %>%
      dplyr::left_join(jt, by = "jtid")
  }) %>%
  setNames(snm)

top_quintile_actors_of_others <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    scenarios_ <- snm[snm != scenario_]

    dat$scenario_scores_by_jtid %>%
      dplyr::filter(scenario %in% scenarios_) %>%
      dplyr::summarize(.by = jtid,
                       mean = mean(jtda_score, na.rm = TRUE)) %>%
      dplyr::filter(mean > quantile(mean,
                                          probs = seq(0, 1, 0.2))[5]) %>%
      dplyr::left_join(jt, by = "jtid")  %>%
      dplyr::arrange(dplyr::desc(mean)) %>%
      dplyr::mutate(scenario = paste0("All Excluding ", scenario_))
  }) %>%
  setNames(snm)

# first-order network ----
all_first_order <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    sc_sub <- sc %>%
      dplyr::filter(scenario == scenario_)

    # get network subset
    edges <- jt_edges %>%
      dplyr::filter(from %in% sc_sub$entity & to %in% sc_sub$entity)
    nodes <- jt %>%
      dplyr::filter(jtid %in% sc_sub$jtid)

    tg <- tg_with_centrality(nodes = nodes, edges = edges)

    # tgnodes <- tg$g %>%
    #   tibble::as_tibble() %>%
    #   dplyr::select(name, tidyselect::starts_with("centrality")) %>%
    #   dplyr::rename(entity = name)

    # grab more complete list,
    # even though they're missing a bunch of centrality values.
    cols_ <- names(tg$nodes)[!(names(tg$nodes) %in% names(nodes))]
    tgnodes <- tg$nodes %>%
      dplyr::select(tidyselect::all_of(c("name", cols_))) %>%
      dplyr::rename(entity = name)

    nodes <- nodes %>%
      dplyr::left_join(y = tgnodes, by = "entity")

    # return
    list(
      edges = edges,
      nodes = nodes,
      g = tg$g
    )
  }
) %>%
  setNames(snm)

first_order_summary <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    all_first_order[[scenario_]] %>%
      net_summary() %>%
      dplyr::select(metric, value) %>%
      dplyr::mutate(scenario = scenario_)
  }
) %>%
  dplyr::bind_rows(
    g_summary %>%
      dplyr::select(metric, value) %>%
      dplyr::mutate(scenario = "full survey")
  ) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::arrange(dplyr::desc(`Nodes (n)`)) %>%
  dplyr::mutate(
    `Nodes (n)` = as.integer(`Nodes (n)`),
    `Observed Nodes (n)` = as.integer(`Observed Nodes (n)`),
    `Edges (n)` = as.integer(`Edges (n)`),
    `Diameter` = as.integer(`Diameter`),
                )

# second-order network ----
all_second_order <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    sc_sub <- sc %>%
      dplyr::filter(scenario == scenario_)

    # survey respondents vector
    respondents_ <- jt_edges$from %>% unique()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # narrow (ie. without secondary intra-connections)
    edges <- jt_edges %>%
      dplyr::filter(from %in% sc_sub$entity | to %in% sc_sub$entity) %>%
      dplyr::select(from, to, weight)

    entities_of_interest <- unique(c(edges$from, edges$to))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # expanded (ie. with secondary intra-connections)
    # Orgs we don't want.
    avoid_ <- jt %>%
      dplyr::filter(!(entity %in% entities_of_interest)) %>%
      dplyr::pull(entity)

    # Get edges we desire by negation.
    edges_expanded <- jt_edges %>%
      # remove orgs we want to avoid.
      dplyr::filter(
        !(from %in% avoid_) &
          !(to %in% avoid_)
      )  %>%
      dplyr::select(from, to, weight)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Nodes
    nodes <- jt %>%
      dplyr::filter(entity %in% entities_of_interest) %>%
      dplyr::mutate(id = entity) %>%
      # color first-order and second-order differently
      dplyr::mutate(color = dplyr::case_when(
        jtid %in% sc_sub$jtid ~ tol_colors[1],
        .default = tol_colors[2]
      )) %>%
      dplyr::mutate(scenario_role_type = dplyr::case_when(
        entity %in% sc_sub$entity ~ "first order",
        .default = "second order"
      )) %>%
      dplyr::mutate(is_respondent = entity %in% respondents_)

    tg <- tg_with_centrality(nodes = nodes, edges = edges_expanded)

    # tgnodes <- tg$g %>%
    #   tibble::as_tibble() %>%
    #   dplyr::select(name, tidyselect::starts_with("centrality")) %>%
    #   dplyr::rename(entity = name)

    # grab more complete list,
    # even though they're missing a bunch of centrality values.
    cols_ <- names(tg$nodes)[!(names(tg$nodes) %in% names(nodes))]
    tgnodes <- tg$nodes %>%
      dplyr::select(tidyselect::all_of(c("name", cols_))) %>%
      dplyr::rename(entity = name)

    nodes <- nodes %>%
      dplyr::left_join(y = tgnodes, by = "entity")

    # return
    list(
      nodes = nodes,
      edges = edges,
      edges_expanded = edges_expanded,
      g = tg$g
    )
  }
) %>%
  setNames(snm)

second_order_summary <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    all_second_order[[scenario_]] %>%
      net_summary() %>%
      dplyr::select(metric, value) %>%
      dplyr::mutate(scenario = scenario_)
  }
) %>%
  dplyr::bind_rows(
    g_summary %>%
      dplyr::select(metric, value) %>%
      dplyr::mutate(scenario = "full survey")
  ) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value) %>%
  dplyr::mutate(
    `Nodes (n)` = as.integer(`Nodes (n)`),
    `Observed Nodes (n)` = as.integer(`Observed Nodes (n)`),
    `Edges (n)` = as.integer(`Edges (n)`),
    `Diameter` = as.integer(`Diameter`),
  )

# quintile second-order network ----
# get all first order and top quintile of second order
quintile_second_order <- purrr::map(
  .x = snm,
  .f = function(scenario_){

  }
) %>%
  setNames(snm)


# scenario organization roles ----
org_roles <- purrr::map(
  .x = snm,
  .f = function(scenario_){
    scores <- dat$scenario_scores_by_action %>%
      dplyr::select(strategy_id, action_id, !!scenario_) %>%
      dplyr::rename(score := !!sym(scenario_)) %>%
      dplyr::filter(score > 0) %>%
      summarize(score = sum(score), .by = "strategy_id")

    leads <- dat$da_strategies_orgs %>%
      dplyr::select(strategy_id, strategy_role_type, jtid) %>%
      dplyr::filter(strategy_role_type == "lead" &
                      strategy_id %in% scores$strategy_id) %>%
      dplyr::left_join(scores, by = "strategy_id") %>%
      dplyr::summarise(score = sum(score), .by = "jtid") %>%
      dplyr::arrange(score) %>%
      dplyr::mutate(strategy_role_type = "lead")

    partners <- dat$da_strategies_orgs %>%
      dplyr::select(strategy_id, strategy_role_type, jtid) %>%
      dplyr::filter(strategy_role_type == "partners" &
                      strategy_id %in% scores$strategy_id) %>%
      dplyr::left_join(scores, by = "strategy_id") %>%
      dplyr::summarise(score = sum(score), .by = "jtid") %>%
      dplyr::arrange(score) %>%
      dplyr::mutate(strategy_role_type = "partners")

    leads %>%
      dplyr::bind_rows(partners) %>%
      dplyr::mutate(scenario = scenario_)
  }
) %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(
    y = jt %>%
      dplyr::select(-source),
    by = "jtid"
  ) %>%
  dplyr::select(scenario, entity, score, strategy_role_type, dplyr::everything())

# scenario_scores_by_area ----
sscores <- purrr::map(
  .x = snm,
  .f = ~scenario_scores_by_area(.x)
) %>%
  setNames(snm)

# sce_unique ----
sunique <- purrr::map(
  .x = snm,
  .f = ~sce_unique(.x)
) %>%
  setNames(snm)
