plot_true_demand_curves <- function(offer_df, sample_customers) {
  plot_df <- offer_df %>%
    filter(customer_id %in% sample_customers)

  ggplot(
    plot_df,
    aes(
      x = offered_price,
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
