compute_true_profit_curve <- function(
    customer_df,
    price_grid,
    unit_cost,
    inventory_limit) {
  tibble(
    price = price_grid
  ) %>%
    rowwise() %>%
    mutate(
      expected_demand =
        sum(
          plogis(
            customer_df$true_alpha +
              customer_df$true_beta * price
          )
        ),

      constrained_sales =
        min(expected_demand, inventory_limit),

      expected_profit =
        (price - unit_cost) *
          constrained_sales
    ) %>%
    ungroup()
}

compute_naive_profit_curve <- function(
    naive_df,
    N_customers,
    unit_cost,
    inventory_limit) {
  naive_df %>%
    mutate(
      expected_demand =
        purchase_rate * N_customers,

      constrained_sales =
        pmin(
          expected_demand,
          inventory_limit
        ),

      expected_profit =
        (price_bin - unit_cost) *
          constrained_sales
    ) %>%
    rename(price = price_bin)
}

compute_bayes_profit_curve <- function(
    price_grid,
    N_customers,
    mu_alpha_post,
    mu_beta_post,
    unit_cost,
    inventory_limit) {
  tibble(
    price = price_grid
  ) %>%
    mutate(
      expected_demand =
        N_customers *
          plogis(
            mu_alpha_post +
              mu_beta_post * price
          ),

      constrained_sales =
        pmin(
          expected_demand,
          inventory_limit
        ),

      expected_profit =
        (price - unit_cost) *
          constrained_sales
    )
}

compare_profit_curves <- function(
    true_profit_df,
    naive_profit_df,
    bayes_profit_df) {
  bind_rows(
    true_profit_df %>%
      mutate(model = "True World"),

    naive_profit_df %>%
      mutate(model = "Naive Aggregate"),

    bayes_profit_df %>%
      mutate(model = "Hierarchical Bayesian")
  )
}

compute_regret <- function(
    true_profit_df,
    naive_profit_df,
    bayes_profit_df) {
  true_optimal_price <-
    true_profit_df$price[
      which.max(true_profit_df$expected_profit)
    ]

  naive_optimal_price <-
    naive_profit_df$price[
      which.max(naive_profit_df$expected_profit)
    ]

  bayes_optimal_price <-
    bayes_profit_df$price[
      which.max(bayes_profit_df$expected_profit)
    ]

  true_optimal_profit <-
    max(true_profit_df$expected_profit)

  naive_realized_profit <-
    true_profit_df %>%
    filter(price == naive_optimal_price) %>%
    pull(expected_profit)

  bayes_realized_profit <-
    true_profit_df %>%
    filter(price == bayes_optimal_price) %>%
    pull(expected_profit)

  regret_df <- tibble(
    Strategy = c(
      "Naive Aggregate",
      "Hierarchical Bayesian"
    ),

    Realized_Profit = c(
      naive_realized_profit,
      bayes_realized_profit
    ),

    Regret = c(
      true_optimal_profit -
        naive_realized_profit,

      true_optimal_profit -
        bayes_realized_profit
    )
  )

  list(
    true_optimal_price = true_optimal_price,
    naive_optimal_price = naive_optimal_price,
    bayes_optimal_price = bayes_optimal_price,
    true_optimal_profit = true_optimal_profit,
    naive_realized_profit = naive_realized_profit,
    bayes_realized_profit = bayes_realized_profit,
    regret_df = regret_df
  )
}
