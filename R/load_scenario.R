scenario_defaults <- function() {
  list(
    name = "Unnamed Scenario",
    description = "",
    unit_cost = 5,
    price_grid = list(min = 1, max = 35, by = 0.25),
    offer_price_range = list(min = 2, max = 30),
    random_seed = 2028,
    promotional_effects = list(
      enabled = TRUE,
      probability = 0.2,
      logit_lift_mean = 0.25,
      logit_lift_sd = 0.10
    ),
    replenishment = list(
      enabled = FALSE,
      quantity = 0,
      interval = Inf
    ),
    inventory_behavior = list(
      enabled = TRUE,
      scarcity_logit_lift = 0.4,
      abundance_logit_penalty = 0.15
    ),
    lifetime_value = list(
      Budget = list(mean = 100, sd = 20),
      Mainstream = list(mean = 350, sd = 40),
      Premium = list(mean = 1200, sd = 150)
    ),
    noise_levels = list(
      price_noise_sd = 0,
      purchase_noise_sd = 0
    ),
    demand_shock_settings = list(
      enabled = FALSE,
      probability = 0,
      logit_shift_mean = 0,
      logit_shift_sd = 0
    )
  )
}

required_scenario_fields <- function() {
  c(
    "market_size",
    "inventory_limits",
    "customer_segment_proportions",
    "price_elasticity_parameters",
    "noise_levels",
    "simulation_horizon",
    "demand_shock_settings"
  )
}

merge_scenario_defaults <- function(x, defaults) {
  for (nm in names(defaults)) {
    if (is.null(x[[nm]])) {
      x[[nm]] <- defaults[[nm]]
    } else if (is.list(x[[nm]]) && is.list(defaults[[nm]])) {
      x[[nm]] <- merge_scenario_defaults(x[[nm]], defaults[[nm]])
    }
  }

  x
}

validate_required_scenario_fields <- function(scenario, path = NULL) {
  required_fields <- required_scenario_fields()
  missing_fields <- required_fields[
    vapply(required_fields, function(field) is.null(scenario[[field]]), logical(1))
  ]

  if (length(missing_fields) > 0) {
    stop(
      "Scenario is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      if (!is.null(path)) paste0(" in ", path) else "",
      call. = FALSE
    )
  }
}

validate_scenario <- function(scenario, path = NULL) {
  if (length(scenario$customer_segment_proportions) == 0) {
    stop("Scenario must define at least one customer segment.", call. = FALSE)
  }

  proportion_sum <- sum(unlist(scenario$customer_segment_proportions))

  if (!isTRUE(all.equal(proportion_sum, 1, tolerance = 1e-6))) {
    stop("Customer segment proportions must sum to 1.", call. = FALSE)
  }

  segment_names <- names(scenario$customer_segment_proportions)
  missing_elasticity <- setdiff(
    segment_names,
    names(scenario$price_elasticity_parameters)
  )

  if (length(missing_elasticity) > 0) {
    stop(
      "Missing price elasticity parameters for segment(s): ",
      paste(missing_elasticity, collapse = ", "),
      call. = FALSE
    )
  }

  required_elasticity_fields <- c("alpha_mean", "alpha_sd", "beta_mean", "beta_sd")

  for (segment in segment_names) {
    params <- scenario$price_elasticity_parameters[[segment]]
    missing_params <- setdiff(required_elasticity_fields, names(params))

    if (length(missing_params) > 0) {
      stop(
        "Missing price elasticity field(s) for ",
        segment,
        ": ",
        paste(missing_params, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (scenario$market_size <= 0) {
    stop("market_size must be positive.", call. = FALSE)
  }

  if (scenario$simulation_horizon <= 0) {
    stop("simulation_horizon must be positive.", call. = FALSE)
  }

  if (scenario$inventory_limits$total <= 0) {
    stop("inventory_limits$total must be positive.", call. = FALSE)
  }

  scenario
}

load_scenario <- function(path, defaults_path = "configs/defaults.yaml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The yaml package is required to load scenario files.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("Scenario file not found: ", path, call. = FALSE)
  }

  scenario <- yaml::read_yaml(path)
  validate_required_scenario_fields(scenario, path)

  defaults <- scenario_defaults()

  if (!is.null(defaults_path) && file.exists(defaults_path)) {
    config_defaults <- yaml::read_yaml(defaults_path)
    defaults <- merge_scenario_defaults(config_defaults, defaults)
  }

  scenario <- merge_scenario_defaults(scenario, defaults)
  scenario <- validate_scenario(scenario, path)

  class(scenario) <- c("simulation_scenario", class(scenario))
  scenario
}

scenario_price_grid <- function(scenario) {
  seq(
    scenario$price_grid$min,
    scenario$price_grid$max,
    by = scenario$price_grid$by
  )
}

scenario_summary <- function(scenario) {
  tibble::tibble(
    field = c(
      "Scenario",
      "Market size",
      "Inventory limit",
      "Simulation horizon",
      "Customer segments",
      "Demand shock enabled"
    ),
    value = c(
      scenario$name,
      scenario$market_size,
      scenario$inventory_limits$total,
      scenario$simulation_horizon,
      paste(
        names(scenario$customer_segment_proportions),
        unlist(scenario$customer_segment_proportions),
        sep = "=",
        collapse = ", "
      ),
      scenario$demand_shock_settings$enabled
    )
  )
}
