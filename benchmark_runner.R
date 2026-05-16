suppressPackageStartupMessages({
  library(tidyverse)
  library(rjags)
  library(coda)
})

source("R/load_scenario.R")
source("R/export_external.R")
source("generative_models/world_generation.R")
source("generative_models/dynamic_inventory.R")
source("analytical_models/model_fitting.R")
source("pricing_policies/dynamic_pricing_policies.R")
source("policy_evaluation.R")
source("evaluation/oracle_evaluation.R")
source("evaluation/dynamic_evaluation.R")
source("plotting.R")

default_benchmark_config <- function() {
  list(
    scenario_dir = Sys.getenv("BENCHMARK_SCENARIO_DIR", "scenarios"),
    output_dir = Sys.getenv("BENCHMARK_OUTPUT_DIR", "outputs"),
    external_export_dir = Sys.getenv(
      "BENCHMARK_EXTERNAL_EXPORT_DIR",
      file.path(Sys.getenv("BENCHMARK_OUTPUT_DIR", "outputs"), "external")
    ),
    write_external_exports = as.logical(Sys.getenv("BENCHMARK_WRITE_EXTERNAL_EXPORTS", "TRUE")),
    seeds = parse_benchmark_seeds(Sys.getenv("BENCHMARK_SEEDS", "2028:2032")),
    parallel = as.logical(Sys.getenv("BENCHMARK_PARALLEL", "FALSE")),
    workers = as.integer(Sys.getenv("BENCHMARK_WORKERS", "2")),
    jags = list(
      n.chains = as.integer(Sys.getenv("BENCHMARK_JAGS_CHAINS", "4")),
      n.adapt = as.integer(Sys.getenv("BENCHMARK_JAGS_ADAPT", "2000")),
      n.update = as.integer(Sys.getenv("BENCHMARK_JAGS_UPDATE", "4000")),
      n.iter = as.integer(Sys.getenv("BENCHMARK_JAGS_ITER", "8000"))
    )
  )
}

parse_benchmark_seeds <- function(seed_spec) {
  if (grepl(":", seed_spec, fixed = TRUE)) {
    bounds <- as.integer(strsplit(seed_spec, ":", fixed = TRUE)[[1]])
    return(seq(bounds[[1]], bounds[[2]]))
  }

  as.integer(strsplit(seed_spec, ",", fixed = TRUE)[[1]])
}

ensure_benchmark_dirs <- function(output_dir) {
  dirs <- c(
    output_dir,
    file.path(output_dir, "figures"),
    file.path(output_dir, "metrics"),
    file.path(output_dir, "external")
  )

  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

discover_scenario_files <- function(scenario_dir) {
  list.files(
    scenario_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )
}

analytical_model_registry <- function() {
  tibble(
    model = c(
      "Naive Aggregate",
      "Hierarchical Bayesian",
      "Misspecified Linear"
    ),
    backend = c("R", "JAGS", "R"),
    status = c("active", "active", "active"),
    extension_notes = c(
      "Replace or extend with pooled GLM variants.",
      "Can be swapped for Stan by returning the same posterior summary interface.",
      "Use as a baseline for deliberate misspecification tests."
    )
  )
}

policy_registry <- function() {
  tibble(
    policy = c(
      "Static Pricing",
      "Inventory-Aware Pricing",
      "Greedy Revenue Maximization",
      "Conservative Inventory Preservation"
    ),
    backend = "R",
    status = "active"
  )
}

scenario_id_from_path <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

build_static_model_curves <- function(
    analyst_df,
    customer_df,
    price_grid,
    unit_cost,
    inventory_limit,
    jags_config) {
  naive_df <- fit_naive_aggregate_model(analyst_df)
  misspecified_fit <- fit_misspecified_model(analyst_df)
  bayes_fit <- fit_hierarchical_bayesian_model(
    analyst_df = analyst_df,
    n.chains = jags_config$n.chains,
    n.adapt = jags_config$n.adapt,
    n.update = jags_config$n.update,
    n.iter = jags_config$n.iter
  )

  n_customers_observed <- n_distinct(analyst_df$customer_id)

  list(
    fits = list(
      naive_df = naive_df,
      bayes_fit = bayes_fit,
      misspecified_fit = misspecified_fit
    ),
    curves = list(
      "Naive Aggregate" = compute_naive_profit_curve(
        naive_df,
        n_customers_observed,
        unit_cost,
        inventory_limit
      ),
      "Hierarchical Bayesian" = compute_bayes_profit_curve(
        price_grid,
        n_customers_observed,
        bayes_fit$mu_alpha_post,
        bayes_fit$mu_beta_post,
        unit_cost,
        inventory_limit
      ),
      "Misspecified Linear" = compute_misspecified_profit_curve(
        price_grid,
        n_customers_observed,
        misspecified_fit,
        unit_cost,
        inventory_limit
      )
    )
  )
}

run_single_benchmark <- function(
    scenario_path,
    seed,
    jags_config,
    external_export_dir = NULL,
    write_external_exports = FALSE) {
  scenario <- load_scenario(scenario_path)
  scenario_name <- scenario$name
  scenario_id <- scenario_id_from_path(scenario_path)
  price_grid <- scenario_price_grid(scenario)
  unit_cost <- scenario$unit_cost
  inventory_limit <- scenario$inventory_limits$total
  market_size <- scenario$market_size
  simulation_horizon <- scenario$simulation_horizon
  demand_shocks_enabled <- scenario$demand_shock_settings$enabled

  customer_df <- generate_customer_world(
    N_customers = market_size,
    seed = seed,
    segment_proportions = scenario$customer_segment_proportions,
    price_elasticity_parameters = scenario$price_elasticity_parameters,
    lifetime_value_parameters = scenario$lifetime_value
  )

  offer_df <- simulate_purchase_opportunities(
    customer_df = customer_df,
    N_offers = simulation_horizon,
    offer_price_range = scenario$offer_price_range,
    inventory_limit = inventory_limit,
    noise_levels = scenario$noise_levels,
    demand_shock_settings = scenario$demand_shock_settings,
    promotional_effects = scenario$promotional_effects
  )

  analyst_df <- observe_analyst_data(offer_df)
  true_profit_df <- compute_true_profit_curve(
    customer_df,
    price_grid,
    unit_cost,
    inventory_limit
  )

  external_export_manifest_df <- tibble(
    export = character(),
    role = character(),
    path = character()
  )

  if (write_external_exports && !is.null(external_export_dir)) {
    external_export_manifest_df <- write_external_analysis_exports(
      export_dir = file.path(
        external_export_dir,
        scenario_id,
        paste0("seed_", seed)
      ),
      scenario = scenario,
      scenario_path = scenario_path,
      seed = seed,
      analyst_df = analyst_df,
      customer_df = customer_df,
      true_profit_df = true_profit_df
    ) %>%
      mutate(
        scenario = scenario_name,
        scenario_id = scenario_id,
        seed = seed,
        .before = export
      )
  }

  static_results <- build_static_model_curves(
    analyst_df,
    customer_df,
    price_grid,
    unit_cost,
    inventory_limit,
    jags_config
  )

  oracle_metrics_df <- compute_oracle_decision_metrics(
      true_profit_df,
      static_results$curves
  ) %>%
    mutate(
      scenario = scenario_name,
      scenario_id = scenario_id,
      seed = seed,
      market_size = market_size,
      simulation_horizon = simulation_horizon,
      observations = nrow(analyst_df),
      observations_per_customer = nrow(analyst_df) / market_size,
      .before = model
    )

  calibration_metrics_df <- compute_calibration_metrics(
    analyst_df = analyst_df,
    offer_df = offer_df,
    naive_df = static_results$fits$naive_df,
    bayes_fit = static_results$fits$bayes_fit,
    misspecified_fit = static_results$fits$misspecified_fit
  ) %>%
    mutate(
      scenario = scenario_name,
      scenario_id = scenario_id,
      seed = seed,
      .before = model
    )

  inferred_parameters_df <- summarize_inferred_parameters(
    analyst_df = analyst_df,
    bayes_fit = static_results$fits$bayes_fit,
    misspecified_fit = static_results$fits$misspecified_fit
  )
  parameter_comparison_df <- compare_inferred_parameters(
    inferred_parameters_df,
    customer_df
  ) %>%
    mutate(
      scenario = scenario_name,
      scenario_id = scenario_id,
      seed = seed,
      .before = model
    )

  dynamic_policies <- build_dynamic_pricing_policies(
    estimated_profit_df = static_results$curves[["Hierarchical Bayesian"]],
    static_price = select_greedy_price(
      static_results$curves[["Hierarchical Bayesian"]]
    )
  )
  dynamic_results_df <- simulate_dynamic_pricing_policies(
    customer_df = customer_df,
    policies = dynamic_policies,
    horizon = simulation_horizon,
    initial_inventory = inventory_limit,
    price_grid = price_grid,
    unit_cost = unit_cost,
    demand_shock_settings = scenario$demand_shock_settings,
    replenishment_settings = scenario$replenishment,
    inventory_behavior = scenario$inventory_behavior,
    seed = seed
  ) %>%
    compute_dynamic_regret(
      customer_df = customer_df,
      price_grid = price_grid,
      unit_cost = unit_cost
  ) %>%
    mutate(
      scenario = scenario_name,
      scenario_id = scenario_id,
      seed = seed,
      .before = policy
    )

  dynamic_summary_df <- summarize_dynamic_policy_performance(
    dynamic_results_df
  ) %>%
    mutate(
      scenario = scenario_name,
      scenario_id = scenario_id,
      seed = seed,
      demand_shocks_enabled = demand_shocks_enabled,
      .before = policy
    )

  list(
    oracle_metrics = oracle_metrics_df,
    calibration_metrics = calibration_metrics_df,
    parameter_metrics = parameter_comparison_df,
    dynamic_metrics = dynamic_summary_df,
    dynamic_trace = dynamic_results_df,
    external_exports = external_export_manifest_df
  )
}

run_benchmark_grid <- function(config = default_benchmark_config()) {
  ensure_benchmark_dirs(config$output_dir)

  scenario_paths <- discover_scenario_files(config$scenario_dir)
  experiment_grid <- tidyr::crossing(
    scenario_path = scenario_paths,
    seed = config$seeds
  )
  jobs <- split(experiment_grid, seq_len(nrow(experiment_grid)))

  run_job <- function(job) {
    run_single_benchmark(
      scenario_path = job$scenario_path,
      seed = job$seed,
      jags_config = config$jags,
      external_export_dir = config$external_export_dir,
      write_external_exports = config$write_external_exports
    )
  }

  results <- if (config$parallel && .Platform$OS.type != "windows") {
    parallel::mclapply(
      jobs,
      run_job,
      mc.cores = min(config$workers, length(jobs))
    )
  } else {
    lapply(jobs, run_job)
  }

  combined <- list(
    oracle_metrics = bind_rows(lapply(results, `[[`, "oracle_metrics")),
    calibration_metrics = bind_rows(lapply(results, `[[`, "calibration_metrics")),
    parameter_metrics = bind_rows(lapply(results, `[[`, "parameter_metrics")),
    dynamic_metrics = bind_rows(lapply(results, `[[`, "dynamic_metrics")),
    dynamic_trace = bind_rows(lapply(results, `[[`, "dynamic_trace")),
    external_exports = bind_rows(lapply(results, `[[`, "external_exports"))
  )

  save_benchmark_outputs(combined, config$output_dir)
  combined
}

summarize_aggregated_regret <- function(oracle_metrics_df) {
  scored_df <- oracle_metrics_df %>%
    group_by(scenario_id, seed) %>%
    mutate(best_in_run = profit_regret == min(profit_regret)) %>%
    ungroup()

  scored_df %>%
    group_by(scenario_id, scenario, model) %>%
    summarize(
      runs = n(),
      mean_profit_regret = mean(profit_regret),
      median_profit_regret = median(profit_regret),
      sd_profit_regret = if (n() > 1) sd(profit_regret) else 0,
      mean_decision_regret = mean(decision_regret),
      mean_observations_per_customer = mean(observations_per_customer),
      win_rate = mean(best_in_run),
      .groups = "drop"
    ) %>%
    arrange(scenario_id, mean_profit_regret)
}

summarize_hierarchical_value <- function(oracle_metrics_df) {
  oracle_metrics_df %>%
    select(scenario_id, scenario, seed, model, profit_regret) %>%
    pivot_wider(
      names_from = model,
      values_from = profit_regret
    ) %>%
    transmute(
      scenario_id,
      scenario,
      seed,
      regret_reduction_vs_naive =
        `Naive Aggregate` - `Hierarchical Bayesian`,
      regret_reduction_vs_misspecified =
        `Misspecified Linear` - `Hierarchical Bayesian`
    ) %>%
    group_by(scenario_id, scenario) %>%
    summarize(
      runs = n(),
      mean_regret_reduction_vs_naive = mean(regret_reduction_vs_naive),
      median_regret_reduction_vs_naive = median(regret_reduction_vs_naive),
      mean_regret_reduction_vs_misspecified = mean(regret_reduction_vs_misspecified),
      median_regret_reduction_vs_misspecified = median(regret_reduction_vs_misspecified),
      .groups = "drop"
    )
}

summarize_calibration <- function(calibration_metrics_df) {
  calibration_metrics_df %>%
    group_by(scenario_id, scenario, model) %>%
    summarize(
      mean_brier_score = mean(brier_score),
      mean_log_loss = mean(log_loss),
      mean_calibration_bias = mean(calibration_bias),
      mean_oracle_probability_rmse = mean(oracle_probability_rmse),
      .groups = "drop"
    ) %>%
    arrange(scenario_id, mean_brier_score)
}

summarize_dynamic_policy_robustness <- function(dynamic_metrics_df) {
  dynamic_metrics_df %>%
    group_by(scenario_id, scenario, demand_shocks_enabled, policy) %>%
    summarize(
      runs = n(),
      mean_total_profit = mean(total_profit),
      mean_cumulative_regret = mean(cumulative_regret),
      mean_stockout_frequency = mean(stockout_frequency),
      mean_final_inventory = mean(final_inventory),
      .groups = "drop"
    ) %>%
    arrange(scenario_id, mean_cumulative_regret)
}

plot_benchmark_model_regret <- function(oracle_metrics_df) {
  ggplot(
    oracle_metrics_df,
    aes(
      x = model,
      y = profit_regret,
      fill = model
    )
  ) +
    geom_boxplot(alpha = 0.8) +
    facet_wrap(~ scenario_id, scales = "free_y") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "none"
    ) +
    labs(
      title = "Analytical Model Profit Regret by Scenario",
      x = "Analytical Model",
      y = "Profit Regret"
    )
}

plot_benchmark_calibration <- function(calibration_metrics_df) {
  ggplot(
    calibration_metrics_df,
    aes(
      x = model,
      y = brier_score,
      fill = model
    )
  ) +
    geom_boxplot(alpha = 0.8) +
    facet_wrap(~ scenario_id, scales = "free_y") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "none"
    ) +
    labs(
      title = "Calibration Error by Analytical Model",
      x = "Analytical Model",
      y = "Brier Score"
    )
}

plot_benchmark_policy_regret <- function(dynamic_metrics_df) {
  ggplot(
    dynamic_metrics_df,
    aes(
      x = policy,
      y = cumulative_regret,
      fill = policy
    )
  ) +
    geom_boxplot(alpha = 0.8) +
    facet_wrap(~ scenario_id, scales = "free_y") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "none"
    ) +
    labs(
      title = "Dynamic Policy Cumulative Regret by Scenario",
      x = "Dynamic Pricing Policy",
      y = "Cumulative Regret"
    )
}

save_benchmark_outputs <- function(results, output_dir) {
  metrics_dir <- file.path(output_dir, "metrics")
  figures_dir <- file.path(output_dir, "figures")

  aggregated_regret_df <- summarize_aggregated_regret(
    results$oracle_metrics
  )
  calibration_summary_df <- summarize_calibration(
    results$calibration_metrics
  )
  dynamic_policy_summary_df <- summarize_dynamic_policy_robustness(
    results$dynamic_metrics
  )
  hierarchical_value_df <- summarize_hierarchical_value(
    results$oracle_metrics
  )

  readr::write_csv(results$oracle_metrics, file.path(metrics_dir, "model_decision_metrics.csv"))
  readr::write_csv(results$calibration_metrics, file.path(metrics_dir, "calibration_metrics.csv"))
  readr::write_csv(results$parameter_metrics, file.path(metrics_dir, "parameter_recovery_metrics.csv"))
  readr::write_csv(results$dynamic_metrics, file.path(metrics_dir, "dynamic_policy_metrics.csv"))
  readr::write_csv(results$dynamic_trace, file.path(metrics_dir, "dynamic_policy_traces.csv"))
  readr::write_csv(results$external_exports, file.path(metrics_dir, "external_export_manifest.csv"))
  readr::write_csv(aggregated_regret_df, file.path(metrics_dir, "aggregated_regret_metrics.csv"))
  readr::write_csv(hierarchical_value_df, file.path(metrics_dir, "hierarchical_modeling_value.csv"))
  readr::write_csv(calibration_summary_df, file.path(metrics_dir, "calibration_summary.csv"))
  readr::write_csv(dynamic_policy_summary_df, file.path(metrics_dir, "dynamic_policy_robustness.csv"))
  readr::write_csv(analytical_model_registry(), file.path(metrics_dir, "analytical_model_registry.csv"))
  readr::write_csv(policy_registry(), file.path(metrics_dir, "policy_registry.csv"))

  ggsave(
    file.path(figures_dir, "model_regret_by_scenario.png"),
    plot_benchmark_model_regret(results$oracle_metrics),
    width = 12,
    height = 7
  )
  ggsave(
    file.path(figures_dir, "calibration_by_model.png"),
    plot_benchmark_calibration(results$calibration_metrics),
    width = 12,
    height = 7
  )
  ggsave(
    file.path(figures_dir, "dynamic_policy_regret_by_scenario.png"),
    plot_benchmark_policy_regret(results$dynamic_metrics),
    width = 12,
    height = 7
  )

  invisible(list(
    aggregated_regret = aggregated_regret_df,
    hierarchical_value = hierarchical_value_df,
    calibration_summary = calibration_summary_df,
    dynamic_policy_summary = dynamic_policy_summary_df
  ))
}

main <- function() {
  config <- default_benchmark_config()
  results <- run_benchmark_grid(config)

  message("Benchmark complete.")
  message("Metrics written to: ", file.path(config$output_dir, "metrics"))
  message("Figures written to: ", file.path(config$output_dir, "figures"))

  invisible(results)
}

if (sys.nframe() == 0) {
  main()
}
