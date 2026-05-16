compute_oracle_optimal_price <- function(true_profit_df) {
  true_profit_df %>%
    slice_max(expected_profit, n = 1, with_ties = FALSE) %>%
    transmute(
      oracle_optimal_price = price,
      oracle_optimal_profit = expected_profit
    )
}

summarize_oracle_truth <- function(customer_df) {
  customer_df %>%
    group_by(latent_segment) %>%
    summarize(
      segment_customers = n(),
      segment_share = n() / nrow(customer_df),
      mean_true_alpha = mean(true_alpha),
      mean_true_beta = mean(true_beta),
      mean_true_willingness_to_pay = mean(
        true_willingness_to_pay[is.finite(true_willingness_to_pay)]
      ),
      .groups = "drop"
    )
}

compute_oracle_decision_metrics <- function(
    true_profit_df,
    model_profit_curves) {
  oracle <- compute_oracle_optimal_price(true_profit_df)

  bind_rows(
    lapply(
      names(model_profit_curves),
      function(model_name) {
        model_profit_df <- model_profit_curves[[model_name]]
        selected_row <- model_profit_df %>%
          slice_max(expected_profit, n = 1, with_ties = FALSE)

        realized_price <- selected_row$price
        realized_profit <- realized_profit_at_price(
          true_profit_df,
          realized_price
        )

        tibble(
          model = model_name,
          oracle_optimal_price = oracle$oracle_optimal_price,
          realized_price = realized_price,
          oracle_optimal_profit = oracle$oracle_optimal_profit,
          realized_profit = realized_profit,
          profit_regret = oracle$oracle_optimal_profit - realized_profit,
          decision_regret = abs(realized_price - oracle$oracle_optimal_price)
        )
      }
    )
  )
}

predict_naive_probabilities <- function(analyst_df, naive_df) {
  analyst_df %>%
    mutate(price_bin = round(offered_price)) %>%
    left_join(naive_df, by = "price_bin") %>%
    mutate(predicted_probability = pmin(1, pmax(0, purchase_rate))) %>%
    pull(predicted_probability)
}

predict_bayes_probabilities <- function(analyst_df, bayes_fit) {
  plogis(
    bayes_fit$mu_alpha_post +
      bayes_fit$mu_beta_post * analyst_df$offered_price
  )
}

predict_misspecified_probabilities <- function(analyst_df, misspecified_fit) {
  pmin(
    1,
    pmax(
      0,
      misspecified_fit$alpha_hat +
        misspecified_fit$beta_hat * analyst_df$offered_price
    )
  )
}

compute_calibration_metrics <- function(
    analyst_df,
    offer_df,
    naive_df,
    bayes_fit,
    misspecified_fit) {
  observed_df <- analyst_df %>%
    left_join(
      offer_df %>%
        select(customer_id, offer_id, true_probability = purchase_probability),
      by = c("customer_id", "offer_id")
    )

  prediction_df <- bind_rows(
    tibble(
      model = "Naive Aggregate",
      observed = observed_df$purchase,
      true_probability = observed_df$true_probability,
      predicted_probability = predict_naive_probabilities(analyst_df, naive_df)
    ),
    tibble(
      model = "Hierarchical Bayesian",
      observed = observed_df$purchase,
      true_probability = observed_df$true_probability,
      predicted_probability = predict_bayes_probabilities(analyst_df, bayes_fit)
    ),
    tibble(
      model = "Misspecified Linear",
      observed = observed_df$purchase,
      true_probability = observed_df$true_probability,
      predicted_probability = predict_misspecified_probabilities(
        analyst_df,
        misspecified_fit
      )
    )
  ) %>%
    mutate(
      predicted_probability = pmin(
        1 - 1e-6,
        pmax(1e-6, predicted_probability)
      )
    )

  prediction_df %>%
    group_by(model) %>%
    summarize(
      brier_score = mean((predicted_probability - observed)^2),
      log_loss = -mean(
        observed * log(predicted_probability) +
          (1 - observed) * log(1 - predicted_probability)
      ),
      calibration_bias = mean(predicted_probability - observed),
      oracle_probability_rmse = sqrt(
        mean((predicted_probability - true_probability)^2)
      ),
      .groups = "drop"
    )
}

compute_posterior_uncertainty_coverage <- function(
    bayes_fit,
    customer_df,
    probability = 0.95) {
  alpha <- (1 - probability) / 2
  posterior_matrix <- bayes_fit$posterior_matrix

  coverage_df <- tibble(
    parameter = c("mu_alpha", "mu_beta"),
    truth = c(
      mean(customer_df$true_alpha),
      mean(customer_df$true_beta)
    ),
    lower = c(
      quantile(posterior_matrix[, "mu_alpha"], alpha),
      quantile(posterior_matrix[, "mu_beta"], alpha)
    ),
    upper = c(
      quantile(posterior_matrix[, "mu_alpha"], 1 - alpha),
      quantile(posterior_matrix[, "mu_beta"], 1 - alpha)
    )
  ) %>%
    mutate(
      covered = truth >= lower & truth <= upper,
      interval_width = upper - lower,
      coverage_probability = probability,
      model = "Hierarchical Bayesian"
    )

  coverage_df
}

prepare_revenue_curve_comparison <- function(
    true_profit_df,
    model_profit_curves) {
  bind_rows(
    true_profit_df %>%
      transmute(
        model = "Oracle Truth",
        curve_type = "True revenue curve",
        price,
        expected_revenue = price * constrained_sales
      ),
    bind_rows(
      lapply(
        names(model_profit_curves),
        function(model_name) {
          model_profit_curves[[model_name]] %>%
            transmute(
              model = model_name,
              curve_type = "Estimated revenue curve",
              price,
              expected_revenue = price * constrained_sales
            )
        }
      )
    )
  )
}

summarize_best_decision_models <- function(oracle_metrics_df) {
  oracle_metrics_df %>%
    arrange(profit_regret, decision_regret) %>%
    mutate(rank = row_number()) %>%
    select(
      rank,
      model,
      realized_price,
      oracle_optimal_price,
      realized_profit,
      oracle_optimal_profit,
      profit_regret,
      decision_regret
    )
}
