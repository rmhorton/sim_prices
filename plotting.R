plot_true_demand_curves <- function(offer_df, sample_customers) {
  plot_df <- offer_df %>%
    filter(customer_id %in% sample_customers)

  if ("offered_price" %in% names(plot_df)) {
    plot_df <- plot_df %>%
      mutate(plot_price = offered_price)
  } else {
    plot_df <- plot_df %>%
      mutate(plot_price = price)
  }

  ggplot(
    plot_df,
    aes(
      x = plot_price,
      y = purchase_probability,
      group = customer_id,
      color = latent_segment
    )
  ) +
    geom_line(alpha = 0.7) +
    theme_minimal(base_size = 14) +
    labs(
      title = "True Latent Demand Curves",
      x = "Price",
      y = "Purchase Probability"
    )
}

plot_naive_demand_curve <- function(naive_df) {
  ggplot(
    naive_df,
    aes(
      x = price_bin,
      y = purchase_rate
    )
  ) +
    geom_point(size = 3, color = "firebrick") +
    geom_smooth(
      method = "loess",
      se = FALSE,
      color = "navy"
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Naive Aggregate Demand Curve",
      x = "Price",
      y = "Observed Purchase Rate"
    )
}

plot_profit_curves <- function(comparison_df) {
  ggplot(
    comparison_df,
    aes(
      x = price,
      y = expected_profit,
      color = model
    )
  ) +
    geom_line(linewidth = 1.2) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Competing Profit Curves",
      x = "Price",
      y = "Expected Profit"
    )
}

plot_regret <- function(regret_df) {
  ggplot(
    regret_df,
    aes(
      x = Strategy,
      y = Regret,
      fill = Strategy
    )
  ) +
    geom_col(alpha = 0.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Decision Regret",
      y = "Profit Regret"
    )
}

plot_revenue_curve_comparison <- function(revenue_curve_df) {
  ggplot(
    revenue_curve_df,
    aes(
      x = price,
      y = expected_revenue,
      color = model,
      linetype = curve_type
    )
  ) +
    geom_line(linewidth = 1.1) +
    theme_minimal(base_size = 14) +
    labs(
      title = "True Revenue Curve vs Estimated Revenue Curves",
      x = "Price",
      y = "Expected Revenue"
    )
}

plot_optimal_price_comparison <- function(oracle_metrics_df) {
  price_df <- oracle_metrics_df %>%
    select(model, oracle_optimal_price, realized_price) %>%
    pivot_longer(
      cols = c(oracle_optimal_price, realized_price),
      names_to = "price_type",
      values_to = "price"
    ) %>%
    mutate(
      price_type = recode(
        price_type,
        oracle_optimal_price = "True optimal price",
        realized_price = "Estimated optimal price"
      )
    )

  ggplot(
    price_df,
    aes(
      x = model,
      y = price,
      fill = price_type
    )
  ) +
    geom_col(position = "dodge", alpha = 0.85) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    labs(
      title = "True Optimal Price vs Estimated Optimal Price",
      x = "Analytical Model",
      y = "Price",
      fill = NULL
    )
}

plot_regret_distributions <- function(oracle_metrics_df) {
  regret_df <- oracle_metrics_df %>%
    select(model, profit_regret, decision_regret) %>%
    pivot_longer(
      cols = c(profit_regret, decision_regret),
      names_to = "regret_type",
      values_to = "regret"
    ) %>%
    mutate(
      regret_type = recode(
        regret_type,
        profit_regret = "Profit regret",
        decision_regret = "Decision regret"
      )
    )

  ggplot(
    regret_df,
    aes(
      x = model,
      y = regret,
      fill = regret_type
    )
  ) +
    geom_col(position = "dodge", alpha = 0.85) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    labs(
      title = "Regret by Analytical Model",
      x = "Analytical Model",
      y = "Regret",
      fill = NULL
    )
}

plot_inventory_trajectory <- function(dynamic_results_df) {
  ggplot(
    dynamic_results_df,
    aes(
      x = time_step,
      y = inventory_end,
      color = policy
    )
  ) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Inventory Trajectory",
      x = "Time Step",
      y = "Ending Inventory"
    )
}

plot_dynamic_prices <- function(dynamic_results_df) {
  ggplot(
    dynamic_results_df,
    aes(
      x = time_step,
      y = price,
      color = policy
    )
  ) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Dynamic Prices Over Time",
      x = "Time Step",
      y = "Price"
    )
}

plot_cumulative_regret <- function(dynamic_results_df) {
  ggplot(
    dynamic_results_df,
    aes(
      x = time_step,
      y = cumulative_regret,
      color = policy
    )
  ) +
    geom_line(linewidth = 1.1) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Cumulative Regret",
      x = "Time Step",
      y = "Cumulative Profit Regret"
    )
}

plot_stockout_frequency <- function(dynamic_results_df) {
  stockout_df <- dynamic_results_df %>%
    group_by(policy) %>%
    summarize(
      stockout_frequency = mean(stockout),
      .groups = "drop"
    )

  ggplot(
    stockout_df,
    aes(
      x = policy,
      y = stockout_frequency,
      fill = policy
    )
  ) +
    geom_col(alpha = 0.85) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "none"
    ) +
    labs(
      title = "Stockout Frequency",
      x = "Pricing Policy",
      y = "Share of Time Steps Stocked Out"
    )
}
