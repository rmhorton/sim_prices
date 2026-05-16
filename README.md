# sim_prices

## Benchmark Harness

Run the full scenario/seed benchmark:

```sh
Rscript benchmark_runner.R
```

Useful controls:

```sh
BENCHMARK_SEEDS=2028:2032 \
BENCHMARK_PARALLEL=TRUE \
BENCHMARK_WORKERS=4 \
Rscript benchmark_runner.R
```

For a faster smoke run, reduce JAGS settings:

```sh
BENCHMARK_SEEDS=2028 \
BENCHMARK_JAGS_CHAINS=2 \
BENCHMARK_JAGS_ADAPT=100 \
BENCHMARK_JAGS_UPDATE=100 \
BENCHMARK_JAGS_ITER=200 \
Rscript benchmark_runner.R
```

Outputs are written to `outputs/metrics/` and `outputs/figures/`.

External Stan/Python export bundles are written to:

```text
outputs/external/<scenario_id>/seed_<seed>/
```

Each bundle contains blind analysis input (`observed_data.csv`) plus evaluation-only truth/oracle files. See `docs/data_dictionary.md`.
