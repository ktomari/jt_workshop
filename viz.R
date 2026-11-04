scenario_scores_by_area <- function(
    sce
){
  if(is.character(sce)){
    sda <- dat$scenario_scores_by_action %>%
      dplyr::select(action, action_id, strategy_id, !!sce) %>%
      dplyr::rename(score := !!sym(sce))
  } else {
    sda <- sce
  }
  sda <- sda %>%
    dplyr::filter(score > 0) %>%
    dplyr::mutate(area = stringr::str_extract(strategy_id,
                                              "^[:alpha:]{2,3}")) %>%
    dplyr::mutate(area_color = purrr::map_chr(
      area,
      ~pal_area[which(names(pal_area) %in% .x)])) %>%
    dplyr::mutate(area = purrr::map_chr(
      area,
      ~area_key[names(area_key) == .x]
    )) %>%
    dplyr::arrange(score) %>%
    dplyr::mutate(hovertext = stringr::str_remove(
      action,
      "^[:alpha:]{2,3}\\-[:digit:]{1,2}\\-[:digit:]{1,2}\\:?\\s"
      )
    )
  # TODO
    # split colors along gradients of darkness for higher scores.
    # dplyr::group_by(area, score) %>%
    # dplyr::mutate(score_color = hcl.colors(
    #   n = dplyr::n(),
    #   palette =
    # ))

  order_ <- sda %>%
    dplyr::summarise(score = sum(score), .by = "area") %>%
    dplyr::arrange(score) %>%
    dplyr::mutate(area_fct = forcats::as_factor(area)) %>%
    dplyr::select(area, area_fct)

  sda %>%
    dplyr::left_join(order_, by = "area") %>%
    dplyr::select(-area) %>%
    dplyr::rename(area = area_fct)
}

#' Create barplot using plotly.
#'
#' @param data data.frame
#' @param categorical character. Name of column in data with categories.
#' @param x_ character. Name of column for x-axis
#' @param y_ character. Name of column for y-axis
#' @param color_ character. Name of color column.
#'
#' @returns plotly object
#' @export
viz_counts <- function(
    data,
    categorical,
    x_,
    y_,
    color_ = NA,
    show_legend = TRUE
    ){
  stopifnot("data.frame" %in% class(data))
  stopifnot(identical(class(categorical), "character"))
  stopifnot(identical(class(x_), "character"))
  stopifnot(identical(class(y_), "character"))

  # Create plot by iterating through each entity
  fig <- plotly::plot_ly()

  # Get unique categories
  cats <- unique(data[[categorical]])

  if("hovertext" %in% names(data)){
    data <- data %>%
      dplyr::mutate(hovertext = paste0(
        !!sym(categorical),
        " (",
        !!sym(x_),
        ")<br>",
        stringr::str_wrap(hovertext)
      )
      )
  } else {
    data <- data %>%
      dplyr::mutate(hovertext = paste0(!!sym(categorical),
                                       " (",
                                       !!sym(x_),
                                       ")"))
  }

  for (i in seq_along(cats)) {

    item <- cats[i]

    data_filtered <- data %>%
      dplyr::filter(!!sym(categorical) == item)

    if(nrow(data_filtered) <= 0){
      message("data_filtered nrow 0")
    }

    if(!is.na(color_)){
      marker_ <- list(
        color = data_filtered[[color_]][1],
        line = list(color = "#fff", width = 0.25)
      )
    } else {
      marker_ <- list(
        line = list(color = "#fff", width = 0.25)
      )
    }

    fig <- fig %>%
      plotly::add_trace(
        x = as.formula(paste0("~", x_)),
        y = as.formula(paste0("~", y_)),
        name = item,
        type = "bar",
        data = data_filtered,
        marker = marker_,
        hovertext = ~hovertext,
        hoverinfo = "text"
      )
  }

  fig <- fig %>%
    plotly::layout(
      barmode = "stack",
      # bargap = 0.5,
      # xaxis = list(title = x_),
      # yaxis = list(title = y_),
      hovermode = "closest",
      legend = list(visible = show_legend)
    )

  fig
}

sce_actions_data <- function(
    sce
){

  sda <- dat$scenario_scores_by_action %>%
    dplyr::select(strategy_id, action_id, !!sce, action) %>%
    dplyr::rename(score := !!sym(sce)) %>%
    dplyr::filter(score > 0) %>%
    dplyr::left_join(y = cb, by = "score") %>%
    dplyr::mutate(area = stringr::str_extract(strategy_id, "[:alpha:]+"))

  areas <- sda$area %>% unique()

  pal <- sda %>%
    dplyr::select(area, strategy_id) %>%
    dplyr::distinct() %>%
    dplyr::group_by(area) %>%
    dplyr::mutate(
      color = hcl.colors(
        n = dplyr::n(),
        palette = pal_strategy[which(names(pal_strategy) %in% dplyr::cur_group())]
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-area)

  sda <- sda %>%
    dplyr::left_join(y = pal, by = "strategy_id")

  sda
}

sce_unique <- function(
    sce,
    color_by_area = TRUE,
    score_threshold = 0.5
){
  sda <- unique_actions[[sce]] %>%
    dplyr::filter(score >= score_threshold)

  if(nrow(sda) == 0){
    return(NULL)
  }

  sda <- sda %>%
    dplyr::select(-min_rank) %>%
    dplyr::left_join(y = cb, by = "score") %>%
    dplyr::left_join(y = dat$scenario_scores_by_action %>%
                       dplyr::select(strategy_id, action, action_id),
                     by = "action") %>%
    dplyr::mutate(area = stringr::str_extract(strategy_id, "[:alpha:]+"))

  areas <- sda$area %>% unique()

  if(color_by_area){
    pal <- sda %>%
      dplyr::select(area, strategy_id) %>%
      dplyr::distinct() %>%
      dplyr::group_by(area) %>%
      dplyr::mutate(
        color = purrr::map_chr(
        area,
        ~pal_area[which(names(pal_area) %in% .x)])) %>%
      dplyr::ungroup() %>%
      dplyr::select(-area)
  } else {
    pal <- sda %>%
      dplyr::select(area, strategy_id) %>%
      dplyr::distinct() %>%
      dplyr::group_by(area) %>%
      dplyr::mutate(
        color = hcl.colors(
          n = dplyr::n(),
          palette = pal_strategy[which(names(pal_strategy) %in% dplyr::cur_group())]
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-area)
  }

  sda <- sda %>%
    dplyr::left_join(y = pal, by = "strategy_id")

  sda
}

viz_counts_adv <- function(
    data,
    categorical_col = "strategy_id",
    chunk_col = "action_id",
    x_ = "score",
    y_ = "level",
    hovertext_col = "action"
){
  if(identical(data, NULL)){
    return(NULL)
  }

  # Create plot by iterating through each entity
  fig <- plotly::plot_ly()

  # Get unique categories
  cats <- unique(data[[categorical_col]])
  chunks <- unique(data[[chunk_col]])

  data <- data %>%
    dplyr::mutate(hovertext := !!sym(hovertext_col)) %>%
    dplyr::mutate(hovertext = stringr::str_wrap(hovertext))

  for (i in seq_along(chunks)) {
    cur_chunk <- chunks[i]

    data_filtered <- data %>%
      dplyr::filter(!!sym(chunk_col) == cur_chunk)

    color_ <- data_filtered$color %>%
      unique()
    #
    # outline_ <- data_filtered$outline_color %>%
    #   unique()

    fig <- fig %>%
      plotly::add_trace(
        x = as.formula(paste0("~", x_)),
        y = as.formula(paste0("~", y_)),
        name = cur_chunk,
        type = "bar",
        data = data_filtered,
        marker = list(
          color = color_,
          line = list(color = "#FFF", width = 0.25)
        ),
        hovertext = ~hovertext,
        hoverinfo = "text"
      )

  }

  fig <- fig %>%
    plotly::layout(
      barmode = "stack",
      # bargap = 0.5,
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      hovermode = "closest",
      legend = list(visible = FALSE)
    )

  fig
}

top_actors <- function(
    scenario_
){
  rm_scenarios <- snm[snm != scenario_]

  df <- dat$scenario_scores_by_action %>%
    dplyr::select(-tidyselect::all_of(rm_scenarios)) %>%
    dplyr::filter(!!scenario_ > 0) %>%
    dplyr::rename(jtda_score := !!scenario_) %>%
    dplyr::mutate(strategy_id = strategy_id %>%
                    forcats::as_factor() %>%
                    forcats::fct_rev()
    ) %>%
    dplyr::mutate(hovertext = action %>%
                    stringr::str_wrap(
                      width = 50
                    ) %>%
                    stringr::str_replace_all("\n", "<br>")
    )

  df
}

#' Create bar graph of scenario actions by strategy.
#'
#' @param scenario_ character. Name of scenario.
#'
#' @returns plotly graph
#' @export
bar_actions <- function(
    scenario_
    ){

  sc_act <- top_actors(scenario_)

  vals <- sc_act %>%
    dplyr::pull(jtda_score) %>%
    unique() %>%
    sort()

  pal <- rev(tol_seq_colors)[1:length(vals)] %>%
    rev()

  pal <- data.frame(jtda_score = vals, color = pal)

  sc_act <- sc_act %>%
    dplyr::left_join(y = pal, by = "jtda_score")

  # Create plot by iterating through each entity
  fig <- plot_ly(orientation = "h")

  # Get unique entities in order
  stackables <- unique(sc_act$action_id)

  for (i in seq_along(stackables)) {
    item <- stackables[i]
    item_dat <- sc_act %>%
      dplyr::filter(action_id == item)

    # if(i %% 2 == 0){
    #   color_ <- "#333366"
    # } else {
    #   color_ <- "#334466"
    # }
    #
    fig <- fig %>%
      plotly::add_trace(
        x = ~jtda_score,
        y = ~strategy_id,
        name = item,
        type = "bar",
        data = item_dat,
        marker = list(color = ~color,
                      line = list(color = "#fff", width = 1) ),
        hovertext = ~hovertext,
        hoverinfo = "text"
      )
  }

  anno_dat <- dat$scenario_scores_by_action %>%
    dplyr::filter(strategy_id %in% unique(sc_act$strategy_id)) %>%
    dplyr::select(strategy_id, `key targets`, strategy_desc) %>%
    dplyr::filter(!is.na(strategy_desc))

  annotations_ <- purrr::map(
    .x = 1:nrow(anno_dat),
    .f = function(i){
      row_ <- anno_dat[i,]
      list(
        x = -0.03,
        y = row_$strategy_id,
        opacity = 0,
        text = "  ",
        showarrow = FALSE,
        xref = "paper",
        yref = "y",
        hovertext = paste0(row_$strategy_id, ". ", row_$strategy_desc) %>%
          stringr::str_wrap(
            width = 50
          ) %>%
          stringr::str_replace_all("\n", "<br>"),
        hoverlabel = list(bgcolor = "lightgray"
        )
      )
    }
  )

  fig <- fig %>%
    layout(
      barmode = "stack",
      bargap = 0,
      xaxis = list(title = "Score"),
      yaxis = list(title = "Strategy"),
      hovermode = "closest",
      legend = list(visible = FALSE),
      annotations = annotations_
    )

  fig
}

top_quintile_comparison <- function(
    scenario_
){
  stopifnot("character" %in% class(scenario_))
  category_col <- "entity"
  # x_ <- "scenario"
  # y_ <- "score"

  sc_ <- top_quintile_actors_of_scenario[[scenario_]] %>%
    dplyr::select(scenario, jtda_score, entity) %>%
    dplyr::rename(score = jtda_score)
  otro_ <- top_quintile_actors_of_others[[scenario_]] %>%
    dplyr::select(scenario, mean, entity) %>%
    dplyr::rename(score = mean)
  data <- sc_ %>%
    dplyr::bind_rows(otro_)

  # Normalize to percentages within each scenario
  df_normalized <- data %>%
    group_by(scenario) %>%
    mutate(
      total = sum(score, na.rm = TRUE),
      percentage = (score / total) * 100,
      hover_text = paste0(
        entity, "<br>",
        "Score: ", round(score, 2), "<br>",
        "Percentage: ", round(percentage, 1), "%"
      )
    ) %>%
    ungroup()

  # plot
  fig <- plotly::plot_ly()

  # Get unique cats in order
  cats_ <- unique(df_normalized[[category_col]])

  for (i in seq_along(cats_)) {
    cat_ <- cats_[i]
    single_cat_df <- df_normalized %>%
      dplyr::filter(!!sym(category_col) == cat_)

    fig <- fig %>%
      plotly::add_trace(
        x = ~scenario,
        y = ~percentage,
        name = cat_,
        type = "bar",
        data = single_cat_df,
        marker = list(color = tol_colors[i]),
        hovertext = ~hover_text,
        hoverinfo = "text"
      )
  }

  fig <- fig %>%
    plotly::layout(
      barmode = "stack",
      bargap = 0,
      xaxis = list(title = "Scenario"),
      yaxis = list(title = "Percentage (%)"),
      hovermode = "closest",
      legend = list(visible = FALSE)
    )

  fig

}

type_comparison <- function(
    data,
    categorical_col = "org_type_detail",
    show_legend = FALSE
){
  # category_col <- "entity"
  # x_ <- "scenario"
  # y_ <- "score"

  data <- data %>%
    dplyr::select(scenario, score, !!categorical_col)

  scenario_fct <- data %>%
    dplyr::summarise(score = sum(score), .by = "scenario") %>%
    dplyr::arrange(score) %>%
    dplyr::mutate(scenario = forcats::as_factor(scenario)) %>%
    dplyr::pull(scenario)

  cat_fct <- data %>%
    dplyr::summarise(score = sum(score),
                     .by = !!sym(categorical_col)) %>%
    dplyr::arrange(score) %>%
    dplyr::mutate(!!categorical_col :=
                    forcats::as_factor(!!sym(categorical_col))) %>%
    dplyr::pull(!!categorical_col)

  if(length(cat_fct) <= 10){
    pal <- palette.colors(palette = "Tableau 10")
  } else {
    pal <- palette.colors(palette = "Polychrome 36")
  }

  data <- data %>%
    dplyr::mutate(scenario = factor(
      x = scenario,
      levels = snm
    )) %>%
    dplyr::mutate(!!categorical_col := factor(
      x = !!sym(categorical_col),
      levels = cat_fct
    )) %>%
    dplyr::arrange(!!categorical_col)

  # Normalize to percentages within each scenario
  df_normalized <- data %>%
    group_by(scenario) %>%
    mutate(
      total = sum(score, na.rm = TRUE),
      percentage = (score / total) * 100,
      hover_text = paste0(
        !!sym(categorical_col), "<br>",
        "Score: ", round(score, 2), "<br>",
        "Percentage: ", round(percentage, 1), "%"
      )
    ) %>%
    ungroup()

  # plot
  fig <- plotly::plot_ly()

  # Get unique cats in order
  cats_ <- df_normalized %>%
    dplyr::pull(!!categorical_col) %>%
    levels()

  for (i in seq_along(cats_)) {
    cat_ <- cats_[i]
    single_cat_df <- df_normalized %>%
      dplyr::filter(!!sym(categorical_col) == cat_)

    fig <- fig %>%
      plotly::add_trace(
        x = ~scenario,
        y = ~percentage,
        name = cat_,
        type = "bar",
        data = single_cat_df,
        marker = list(color = pal[i]),
        hovertext = ~hover_text,
        hoverinfo = "text"
      )
  }

  fig <- fig %>%
    plotly::layout(
      barmode = "stack",
      bargap = 0,
      xaxis = list(title = "Scenario"),
      yaxis = list(title = "Percentage (%)"),
      hovermode = "closest",
      legend = list(visible = show_legend)
    )

  fig

}

# network map ----
make_net <- function(nodes, edges){
  net <- visNetwork::visNetwork(
    nodes,
    edges,
    height = "900px",
    width =  "100%"
    # main = list(
    #   text = network_name,
    #   style = "font-family:Helvetica;font-size:10px;text-align:left;"
    # )
  ) %>%
    visNetwork::visEdges(
      arrows = "to",
      smooth = TRUE,
      arrowStrikethrough = FALSE
    ) %>%
    # visNetwork::visNodes(
    #   # default value
    #   value = 1,
    #   scaling = list(
    #     min = 5,
    #     max = 35,
    #     label = list(
    #       enabled = FALSE
    #     )
    #   )
    # ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      multiselect = TRUE
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = FALSE
      )
      #   list(
      #   enabled = TRUE,
      #   hover = FALSE,
      #   algorithm = "hierarchical",
      #   hideColor = "200,200,200,0.2",
      #   degree = 1L,
      #   labelOnly = TRUE
      # )
    ) %>%
    # ~~~~~~~~~~~~~~~~~~~~~~
    # Allow nodes to be moved around
    visNetwork::visEvents(
      dragStart = "function(params) {
      var network = this;
      var dragging = params.nodes || [];
      var updates = dragging.map(function(id){ return {id: id, fixed: {x: false, y: false}}; });
      if (updates.length) network.body.data.nodes.update(updates);
    }",
      dragEnd = "function(params) {
      var network = this;
      var dragged = params.nodes || [];
      var pos = network.getPositions(dragged);
      var updates = dragged.map(function(id){ return {id: id, x: pos[id].x, y: pos[id].y, fixed: {x: true, y: true}}; });
      network.body.data.nodes.update(updates);
    }") %>%
    # ~~~~~~~~~~~~~~~~~~~~~~
    # get node ID
    visNetwork::visEvents(
      select = "function(properties) {
    Shiny.setInputValue('graphnetwork_selected', properties.nodes[0]);
  }",
      click = "function(properties) {
    if (properties.nodes.length === 0) {
      Shiny.setInputValue('graphnetwork_selected', null);
    }
  }"
    ) %>%
    # Highlight node and reduce opacity
    visNetwork::visEvents(
      selectNode = "
      function(params) {
        var selected = params.nodes[0];
        var connected = this.getConnectedNodes(selected);
        connected.push(selected);

        var updates = [];

        this.body.data.nodes.forEach(function(node) {
          var isConnected = connected.indexOf(node.id) !== -1;

          updates.push({
            id: node.id,
            opacity: isConnected ? 1.0 : 0.05
          });
        });

        this.body.data.nodes.update(updates);
      }
    ",

      deselectNode = "
      function(params) {
        var updates = [];

        this.body.data.nodes.forEach(function(node) {
          updates.push({
            id: node.id,
            opacity: 1.0
          });
        });

        this.body.data.nodes.update(updates);
      }
    "
    )
  # # ~~~~~~~~~~~~~~~~~~~~~~
  # # get network positions
  # visNetwork::visEvents(
  #   dragEnd = "
  #     function(params) {
  #       var positions = this.getPositions();
  #
  #       Shiny.setInputValue(
  #         'network_positions',
  #         positions,
  #         {priority: 'event'}
  #       );
  #     }
  #   "
  # )
  #
  # if("y" %in% names(nodes)){
  #   net <- net %>%
  #     visNetwork::visPhysics(enabled = is.null(positions))
  # } else {
  net <- net %>%
    visNetwork::visIgraphLayout(
      randomSeed = 62016
      # layout = "layout_in_circle"
      ) %>%
    visNetwork::visPhysics(
      enabled = TRUE,
      solver = "barnesHut"
    #   # solver = "forceAtlas2Based",
    #   # forceAtlas2Based = list(
    #   #   springLength = 100
    #   # ),
    #   # maxVelocity = 5
    )
  # }

  # if("group" %in% names(nodes)){
  #   net <- net %>%
  #     visNetwork::visOptions(selectedBy = "group") %>%
  #     visNetwork::visGroups(
  #       groupname = "lead",
  #       shape = "star"
  #     ) %>%
  #     visNetwork::visGroups(
  #       groupname = "partners",
  #       shape = "diamond"
  #     ) %>%
  #     visNetwork::visGroups(
  #       groupname = "secondary",
  #       shape = "dot",
  #       shadow = list(enabled = FALSE)
  #     )
  # }

  # return
  net
}

sankey_actions <- function(){
  # create network structure
  edges <- purrr::map(
    .x = snm,
    .f = function(scenario_){
      dat$scenario_scores_by_action %>%
        dplyr::select(action_id, !!scenario_) %>%
        dplyr::filter(!!sym(scenario_) > 0) %>%
        # colnames from ?networkD3::sankeyNetwork
        dplyr::mutate(from = scenario_) %>%
        dplyr::rename(to = action_id,
                      value := !!sym(scenario_))
    }
  ) %>%
    dplyr::bind_rows()

  nodes <- dat$scenario_scores_by_action %>%
    dplyr::select(action_id, action) %>%
    dplyr::filter(action_id %in% edges$to) %>%
    dplyr::rename(name = action_id,
                  desc = action) %>%
    dplyr::mutate(group = stringr::str_extract(name, "^[:alpha:]+")) %>%
    tibble::add_row(name = snm, group = snm, desc = "")

  pal <- tibble(
    group = nodes$group %>%
      unique(),
    color = hcl.colors(n =  group %>%
                         length(),
                       palette = "Set 2")
  )

  nodes <- nodes %>%
    dplyr::left_join(y = pal, by = "group")

  # # Convert to numeric indices (0-indexed for javascript)
  edges <- edges %>%
    dplyr::mutate(
      source = match(from, nodes$name) - 1,
      target = match(to, nodes$name) - 1
    ) %>%
    dplyr::left_join(
      y = nodes %>%
        dplyr::select(name, color),
      by = c(from = "name")
    ) %>%
    dplyr::arrange(target)

  fig <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    arrangement = "perpendicular",
    node = list(
      label = nodes$name,
      color = nodes$color,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.25
      ),
      customdata = stringr::str_wrap(nodes$desc),
      hovertemplate = "<b>%{label}</b><br>Value: %{value}<br>Description: %{customdata}<extra></extra>"
    ),
    link = list(
      source = edges$source,
      target = edges$target,
      value = edges$value,
      color = edges$color
    ),
    valueformat = ",.2f",      # Format as number with 2 decimals
    valuesuffix = ""
  )
  fig
}

