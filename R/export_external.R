sanitize_for_json <- function(x) {
  if (is.list(x)) {
    return(lapply(x, sanitize_for_json))
  }

  if (is.numeric(x)) {
    x[is.infinite(x)] <- NA_real_
  }

  x
}

external_observed_data <- function(analyst_df) {
  analyst_df %>%
    select(
      customer_id,
      offer_id,
      offered_price,
      promotion_active,
      purchase
    )
}

external_latent_truth <- function(customer_df) {
  customer_df %>%
    select(
      customer_id,
      latent_segment,
      true_alpha,
      true_beta,
      true_willingness_to_pay,
      lifetime_value
    )
}

external_oracle_decisions <- function(true_profit_df) {
  oracle <- compute_oracle_optimal_price(true_profit_df)

  true_profit_df %>%
    transmute(
      price,
      oracle_expected_demand = expected_demand,
      oracle_constrained_sales = constrained_sales,
      oracle_expected_profit = expected_profit,
      oracle_optimal_price = oracle$oracle_optimal_price,
      oracle_optimal_profit = oracle$oracle_optimal_profit,
      is_oracle_optimal = price == oracle$oracle_optimal_price
    )
}

external_scenario_metadata <- function(
    scenario,
    scenario_path,
    seed,
    observed_df,
    latent_df) {
  list(
    schema_version = "1.0.0",
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    scenario_file = scenario_path,
    random_seed = seed,
    scenario = sanitize_for_json(scenario),
    row_counts = list(
      observed_data = nrow(observed_df),
      latent_truth = nrow(latent_df)
    ),
    data_roles = list(
      observed_data = "blind_analysis_input",
      latent_truth = "evaluation_only",
      oracle_decisions = "evaluation_only",
      scenario_metadata = "shared_configuration"
    ),
    observable_variables = names(observed_df),
    latent_variables = names(latent_df),
    evaluation_only_outputs = c(
      "latent_truth.csv",
      "oracle_decisions.csv"
    )
  )
}

write_external_analysis_exports <- function(
    export_dir,
    scenario,
    scenario_path,
    seed,
    analyst_df,
    customer_df,
    true_profit_df) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required for scenario metadata exports.", call. = FALSE)
  }

  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  observed_df <- external_observed_data(analyst_df)
  latent_df <- external_latent_truth(customer_df)
  oracle_df <- external_oracle_decisions(true_profit_df)
  metadata <- external_scenario_metadata(
    scenario = scenario,
    scenario_path = scenario_path,
    seed = seed,
    observed_df = observed_df,
    latent_df = latent_df
  )

  observed_path <- file.path(export_dir, "observed_data.csv")
  latent_path <- file.path(export_dir, "latent_truth.csv")
  metadata_path <- file.path(export_dir, "scenario_metadata.json")
  oracle_path <- file.path(export_dir, "oracle_decisions.csv")

  readr::write_csv(observed_df, observed_path)
  readr::write_csv(latent_df, latent_path)
  jsonlite::write_json(
    metadata,
    metadata_path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  readr::write_csv(oracle_df, oracle_path)

  tibble::tibble(
    export = c(
      "observed_data",
      "latent_truth",
      "scenario_metadata",
      "oracle_decisions"
    ),
    role = c(
      "blind_analysis_input",
      "evaluation_only",
      "shared_configuration",
      "evaluation_only"
    ),
    path = c(
      observed_path,
      latent_path,
      metadata_path,
      oracle_path
    )
  )
}
