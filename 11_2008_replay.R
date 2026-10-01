# 11_2008_replay.R -----------------------------------------------------------------------

g_2008   <- c(8.0, 10.5, 4.5, 5.0)          # % per year, 2027..2030 (organic)
L_2008   <- c(4, 9, 10, 0)                  # bp of insured shares per year, 2027..2030 (0 = model's own path in year 4)
rate_2008 <- -300                           # bp, reinvestment shock with repricing lag
L_2008_release <- c(4, 9, 10, -3)           # variant: 2011-style reserve release in year 4

d26 <- panel[panel$vintage == 2026, ]; d26 <- d26[order(d26$period), ]
fwd <- d26[d26$period >= 1, ]; S1 <- d26$insured_shares[d26$period == 0]
half_g <- function(ann) rep((1 + ann / 100)^(1 / 2) - 1, each = 2)            # annual -> two equal half-year rates
g_path <- c(half_g(g_2008), fwd$g[9:10])                                         # periods 1..8 replaced (2027-2030), 2031 at the model's values
S_path <- S1 * cumprod(1 + g_path)                                               # shares along the scenario path
loss_path <- function(bp_yr) { L <- fwd$ins_losses
  extra <- rep(bp_yr, each = 2) / 1e4 * S_path[1:8] / 2; L[1:8] <- ifelse(rep(bp_yr, each = 2) == 0, L[1:8], extra); L }

runs <- list(
  `2026 projection, as is`                 = replay(2026),
  `... 2008-type share growth only`        = replay(2026, g_path = g_path),
  `... 2008-type losses only`              = replay(2026, L_path = loss_path(L_2008)),
  `... 2008-type rate cut only (-300 bp)`  = replay(2026, rate_shock = rate_2008),
  `2008-type: all three combined`          = replay(2026, g_path = g_path, L_path = loss_path(L_2008), rate_shock = rate_2008),
  `... same, with 2011-style reserve release in year 4` = replay(2026, g_path = g_path, L_path = loss_path(L_2008_release), rate_shock = rate_2008)
)
rep2008 <- do.call(rbind, lapply(names(runs), function(n) { p <- runs[[n]]; er <- p[-1]
  data.frame(case = n, dec2026 = 100 * p["er1"], trough = 100 * min(er), trough_when = format(as.Date(fwd$period_end[which.min(er)]), "%b-%y"),
             dec2031 = 100 * p[["er10"]], addon_formula_bp = addon_bp(p), nol_formula = nol_of(addon_bp(p)),
             nol_to_trough = max(1.20, 1.20 + 100 * (p["er1"] - min(er))), stringsAsFactors = FALSE) }))
rownames(rep2008) <- NULL
print(rep2008, digits = 4)

## in the units of the thresholds table -------------------------------------------------------
c(avg_share_growth_2027_30 = 100 * (prod(1 + g_2008 / 100)^(1 / 4) - 1),
  avg_losses_bp_yr = mean(L_2008), avg_losses_M_yr = mean(L_2008) / 1e4 * mean(S_path[1:8]) / 1e6,
  cum_losses_4yr_B = sum(rep(L_2008, each = 2) / 1e4 * S_path[1:8] / 2) / 1e9,
  window_avg_yield_pct = 100 * mean(replay_yield <- {   # what the -300 bp shock does to the achieved yield
    ovn <- pmax(fwd$maturities - fwd$nonovn_maturities, 0); P1 <- d26$portfolio[d26$period == 0]
    frac <- pmin(1, (ovn + cumsum(fwd$nonovn_maturities)) / P1); (fwd$portfolio_yield / 100 + rate_2008 / 1e4 * frac)[1:8] }))
write.csv(rep2008, "tables/replay_2008.csv", row.names = FALSE)
