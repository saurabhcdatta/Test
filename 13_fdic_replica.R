# 13_fdic_replica.R -----------------------------------------------------------------------
use_placeholders <- FALSE
floor_plan <- 1.20; floor_premium <- 1.30
hist <- read.csv("data/ncusif_history_illustrative.csv", stringsAsFactors = FALSE)
hist$S <- hist$insured_shares_B * 1e9; hist$SJ <- ifelse(is.na(hist$insured_shares_june_B), NA, hist$insured_shares_june_B * 1e9); hist$L <- hist$loss_expense_M * 1e6; hist$ox <- hist$opex_M * 1e6
hist$prem <- hist$premiums_M * 1e6; hist$dist <- hist$distributions_M * 1e6; hist$y <- hist$yield_pct / 100

## 1. simulator for one window ---------------------------------------------------------------
sim_window <- function(w, er0, premiums = FALSE, distributions = FALSE, steady_prem_bp = 0, dist_above = NA) {
  d <- w[order(w$year), ]; n <- nrow(d)
  CD <- 0.01 * d$S[1]; E <- er0 / 100 * d$S[1] - CD           # start: deposit trued up, retained earnings = ER0 - 1%
  er <- numeric(n); er[1] <- er0 / 100
  for (t in 2:n) {
    CD_new <- 0.01 * (if (!is.na(d$SJ[t])) d$SJ[t] else (d$S[t - 1] + d$S[t]) / 2)
    income <- d$y[t] * (CD + E)
    NI <- income - d$ox[t] - d$L[t] + (if (premiums) d$prem[t] else 0) - (if (distributions) d$dist[t] else 0) +
          steady_prem_bp / 1e4 * d$S[t]
    E <- E + NI
    if (!is.na(dist_above)) { ex <- (CD_new + E) / d$S[t] - dist_above / 100; if (ex > 0) E <- E - ex * d$S[t] }   # pay out above a target
    er[t] <- (CD_new + E) / d$S[t]; CD <- CD_new
  }
  100 * er
}
required_start <- function(w, floor, ...) uniroot(function(x) min(sim_window(w, x, ...)) - floor, c(0.9, 2.5), tol = 1e-6)$root
steady_premium <- function(w, er0, floor) uniroot(function(b) min(sim_window(w, er0, steady_prem_bp = b)) - floor, c(-50, 200), tol = 1e-6)$root

## 2. run the crisis windows ---------------------------------------------------------------------
windows <- sort(unique(hist$window))
if (!use_placeholders) windows <- windows[sapply(windows, function(x) !any(hist$quality[hist$window == x] == "placeholder"))]
res <- do.call(rbind, lapply(windows, function(x) {
  w <- hist[hist$window == x, ]; w <- w[order(w$year), ]
  er0_actual <- w$er_reported_pct[1]
  path_actual <- sim_window(w, er0_actual, premiums = TRUE, distributions = TRUE)    # what actually happened (check vs reported)
  path_noprem <- sim_window(w, er0_actual)                                            # counterfactual: no premiums, no distributions
  data.frame(window = x, years = paste(range(w$year), collapse = "-"),
             er_start_actual = er0_actual,
             min_er_reported = min(w$er_reported_pct),
             min_er_sim_with_premiums = round(min(path_actual), 3),
             min_er_no_premiums = round(min(path_noprem), 3),
             required_start_floor_1.20 = round(required_start(w, floor_plan), 3),
             required_start_floor_1.30 = round(required_start(w, floor_premium), 3),
             steady_premium_bp_yr = round(steady_premium(w, er0_actual, floor_plan), 1),
             actual_premiums_bp = round(1e4 * sum(w$prem) / mean(w$S), 1))
}))
print(res, row.names = FALSE)


## 3. the FDIC-style conclusion ----------------------------------------------------------------------
ncusif_drr <- max(res$required_start_floor_1.20)
cat(sprintf("\nNCUSIF 'DRR' (no restoration plan through the worst window, no premiums): %.2f%%\n", ncusif_drr))
cat(sprintf("Stricter (never below the 1.30%% premium threshold): %.2f%%\n", max(res$required_start_floor_1.30)))

## 4. paths at the required starting level, for the chart/appendix --------------------------------------
paths <- do.call(rbind, lapply(windows, function(x) {
  w <- hist[hist$window == x, ]; w <- w[order(w$year), ]
  data.frame(window = x, year = w$year, er_reported = w$er_reported_pct,
             er_no_premiums_from_actual = round(sim_window(w, w$er_reported_pct[1]), 3),
             er_from_required_start = round(sim_window(w, required_start(w, floor_plan)), 3))
}))
print(paths, row.names = FALSE)
write.csv(res, "tables/fdic_replica_windows.csv", row.names = FALSE)
write.csv(paths, "tables/fdic_replica_paths.csv", row.names = FALSE)
