compute_dynamic_oracle_price <- function(
    customer_df,
    price_grid,
    inventory,
    unit_cost) {
  oracle_df <- tibble(price = price_grid) %>%
    rowwise() %>%
    mutate(
      expected_demand = sum(plogis(customer_df$true_alpha + customer_df$true_beta * price)),
      expected_sales = min(expected_demand, inventory),
      expected_profit = (price - unit_cost) * expected_sales
    ) %>%
    ungroup()

  oracle_df %>%
    slice_max(expected_profit, n = 1, with_ties = FALSE)
}

compute_dynamic_regret <- function(
    dynamic_results_df,
    customer_df,
    price_grid,
    unit_cost) {
  dynamic_results_df %>%
    rowwise() %>%
    mutate(
      oracle_price = compute_dynamic_oracle_price(
        customer_df,
        price_grid,
        inventory_start,
        unit_cost
      )$price,
      oracle_expected_profit = compute_dynamic_oracle_price(
        customer_df,
        price_grid,
        inventory_start,
        unit_cost
      )$expected_profit,
      step_regret = pmax(oracle_expected_profit - profit, 0),
      price_decision_regret = abs(price - oracle_price)
    ) %>%
    ungroup() %>%
    group_by(policy) %>%
    mutate(cumulative_regret = cumsum(step_regret)) %>%
    ungroup()
}

summarize_dynamic_policy_performance <- function(dynamic_results_df) {
  dynamic_results_df %>%
    group_by(policy) %>%
    summarize(
      total_profit = sum(profit),
      total_sales = sum(sales),
      final_inventory = last(inventory_end),
      stockout_frequency = mean(stockout),
      cumulative_regret = last(cumulative_regret),
      mean_price = mean(price),
      .groups = "drop"
    ) %>%
    arrange(cumulative_regret, desc(total_profit))
}
