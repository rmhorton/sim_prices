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

compute_misspecified_profit_curve <- function(
    price_grid,
    N_customers,
    misspecified_fit,
    unit_cost,
    inventory_limit) {
  tibble(
    price = price_grid
  ) %>%
    mutate(
      expected_demand =
        N_customers *
          pmin(
            1,
            pmax(
              0,
              misspecified_fit$alpha_hat +
                misspecified_fit$beta_hat * price
            )
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
    bayes_profit_df,
    misspecified_profit_df = NULL) {
  model_curves <- list(
    true_profit_df %>%
      mutate(model = "True World"),

    naive_profit_df %>%
      mutate(model = "Naive Aggregate"),

    bayes_profit_df %>%
      mutate(model = "Hierarchical Bayesian")
  )

  if (!is.null(misspecified_profit_df)) {
    model_curves <- append(
      model_curves,
      list(
        misspecified_profit_df %>%
          mutate(model = "Misspecified Linear")
      )
    )
  }

  bind_rows(model_curves)
}

summarize_ground_truth_parameters <- function(customer_df) {
  tibble(
    true_alpha = mean(customer_df$true_alpha),
    true_beta = mean(customer_df$true_beta),
    true_willingness_to_pay = mean(
      customer_df$true_willingness_to_pay[
        is.finite(customer_df$true_willingness_to_pay)
      ]
    )
  )
}

compare_inferred_parameters <- function(inferred_parameters_df, customer_df) {
  truth <- summarize_ground_truth_parameters(customer_df)

  inferred_parameters_df %>%
    mutate(
      true_alpha = truth$true_alpha,
      true_beta = truth$true_beta,
      alpha_error = inferred_alpha - true_alpha,
      beta_error = inferred_beta - true_beta
    )
}

realized_profit_at_price <- function(true_profit_df, chosen_price) {
  if (chosen_price %in% true_profit_df$price) {
    true_profit_df %>%
      filter(price == chosen_price) %>%
      slice(1) %>%
      pull(expected_profit)
  } else {
    approx(
      x = true_profit_df$price,
      y = true_profit_df$expected_profit,
      xout = chosen_price,
      rule = 2
    )$y
  }
}

compute_decision_summary <- function(true_profit_df, model_profit_curves) {
  true_optimal_price <-
    true_profit_df$price[
      which.max(true_profit_df$expected_profit)
    ]

  true_optimal_profit <-
    max(true_profit_df$expected_profit)

  bind_rows(
    lapply(
      names(model_profit_curves),
      function(model_name) {
        model_profit_df <- model_profit_curves[[model_name]]
        chosen_price <- model_profit_df$price[
          which.max(model_profit_df$expected_profit)
        ]
        realized_profit <- realized_profit_at_price(
          true_profit_df,
          chosen_price
        )

        tibble(
          Strategy = model_name,
          Chosen_Price = chosen_price,
          True_Optimal_Price = true_optimal_price,
          Realized_Profit = realized_profit,
          True_Optimal_Profit = true_optimal_profit,
          Regret = true_optimal_profit - realized_profit
        )
      }
    )
  )
}

compute_regret <- function(
    true_profit_df,
    naive_profit_df,
    bayes_profit_df,
    misspecified_profit_df = NULL) {
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

  misspecified_optimal_price <-
    if (!is.null(misspecified_profit_df)) {
      misspecified_profit_df$price[
        which.max(misspecified_profit_df$expected_profit)
      ]
    } else {
      NULL
    }

  true_optimal_profit <-
    max(true_profit_df$expected_profit)

  naive_realized_profit <-
    realized_profit_at_price(true_profit_df, naive_optimal_price)

  bayes_realized_profit <-
    realized_profit_at_price(true_profit_df, bayes_optimal_price)

  misspecified_realized_profit <-
    if (!is.null(misspecified_profit_df)) {
      realized_profit_at_price(true_profit_df, misspecified_optimal_price)
    } else {
      NULL
    }

  regret_df <- tibble(
    Strategy = c(
      "Naive Aggregate",
      "Hierarchical Bayesian",
      if (!is.null(misspecified_profit_df)) "Misspecified Linear"
    ),

    Realized_Profit = c(
      naive_realized_profit,
      bayes_realized_profit,
      misspecified_realized_profit
    ),

    Regret = c(
      true_optimal_profit -
        naive_realized_profit,

      true_optimal_profit -
        bayes_realized_profit,

      if (!is.null(misspecified_profit_df)) {
        true_optimal_profit -
          misspecified_realized_profit
      }
    )
  )

  list(
    true_optimal_price = true_optimal_price,
    naive_optimal_price = naive_optimal_price,
    bayes_optimal_price = bayes_optimal_price,
    misspecified_optimal_price = misspecified_optimal_price,
    true_optimal_profit = true_optimal_profit,
    naive_realized_profit = naive_realized_profit,
    bayes_realized_profit = bayes_realized_profit,
    misspecified_realized_profit = misspecified_realized_profit,
    regret_df = regret_df
  )
}
