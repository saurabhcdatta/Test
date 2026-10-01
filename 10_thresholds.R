# 10_thresholds.R -------------------------------------------------------------------------

V <- 2026
targets <- seq(1.20, 1.33, by = 0.01)
d26  <- panel[panel$vintage == V, ]; d26 <- d26[order(d26$period), ]
win  <- d26[d26$period >= 1 & d26$period <= 10, ]
S_avg <- mean(win$insured_shares)                                   # for "bp of shares per year"
g_model_ann <- 100 * (prod(1 + win$g)^(1 / 5) - 1)                 # %/yr over 2027-31
L_model_yr  <- sum(win$ins_losses) / 5                              # $/yr over 2027-31
L_model_5yr <- sum(d26$ins_losses[d26$period >= 0])                 # sheet's 11-period total
y_model_avg <- mean(win$portfolio_yield)                            
## 1. solvers ------------------------------------------------------------------------------
addon_at <- function(...) addon_bp(replay(V, ...))
solve_for <- function(target_bp, arg, lower, upper) {
  f <- function(x) do.call(addon_at, setNames(list(x), arg)) - target_bp
  uniroot(f, c(lower, upper), tol = 1e-6)$root
}
need <- data.frame(nol = targets, addon_bp = round(100 * (targets - 1.20), 0))
need$dg      <- sapply(need$addon_bp, solve_for, arg = "dg",    lower = -10,   upper = 60)      # pp/yr added to the model's path
need$dL_yr   <- sapply(need$addon_bp, solve_for, arg = "dL_yr", lower = -1e9,  upper = 5e9)     # $/yr added over 2027-30
need$dy      <- sapply(need$addon_bp, solve_for, arg = "dy",    lower = -1000, upper = 500)     # bp added to the yield path

## 2. express in the sheet's own terms ---------------------------------------------------------
need$share_growth_ann <- sapply(need$dg, function(x) 100 * (prod(1 + win$g + x / 200)^(1 / 5) - 1))   # avg annual growth 2027-31
need$share_growth_vs_model_pp <- need$share_growth_ann - g_model_ann
need$losses_per_yr_M    <- (L_model_yr + need$dL_yr) / 1e6
need$losses_5yr_total_M <- (L_model_5yr + 5 * need$dL_yr) / 1e6
need$losses_bp_of_shares_yr <- 1e4 * (L_model_yr + need$dL_yr) / S_avg
need$losses_x_model     <- (L_model_yr + need$dL_yr) / L_model_yr
need$yield_avg          <- y_model_avg + need$dy / 100

## 3. the table ------------------------------------------------------------------------------
thr <- data.frame(
  `Target NOL`                          = sprintf("%.2f%%", need$nol),
  `Add-on needed (bp)`                  = need$addon_bp,
  `Avg share growth 2027-31 (%/yr)`     = sprintf("%.1f", need$share_growth_ann),
  `vs model 5.7%/yr (pp)`               = sprintf("%+.1f", need$share_growth_vs_model_pp),
  `Losses per year 2027-31 ($M)`        = round(need$losses_per_yr_M),
  `Total 5-yr projected losses ($M)`    = round(need$losses_5yr_total_M),
  `Losses, bp of shares per year`       = sprintf("%.2f", need$losses_bp_of_shares_yr),
  `Multiple of model losses`            = sprintf("%.1fx", need$losses_x_model),
  `Avg portfolio yield 2027-31 (%)`     = sprintf("%.2f", need$yield_avg),
  check.names = FALSE)
thr[1, 3:8] <- paste("up to", thr[1, 3:8]); thr[1, 9] <- paste(thr[1, 9], "or higher")     
print(thr, row.names = FALSE)
cat(sprintf("\nmodel: share growth %.1f%%/yr, losses $%.0fM/yr ($%.0fM over 5 yrs, %.2f bp of shares), yield %.2f%%\n",
            g_model_ann, L_model_yr / 1e6, L_model_5yr / 1e6, 1e4 * L_model_yr / S_avg, y_model_avg))

## 4. check: replay at the solved values reproduces the target ---------------------------------
print(data.frame(target = need$nol,
           nol_at_solved_g = round(nol_of(sapply(need$dg, function(x) addon_at(dg = x))), 3),
           nol_at_solved_L = round(nol_of(sapply(need$dL_yr, function(x) addon_at(dL_yr = x))), 3)))

write.csv(thr, "tables/thresholds_2026.csv", row.names = FALSE)

## 5. balanced combination: each lever does an equal share of the work -------------------------
# scale the three one-at-a-time distances by a common factor f and solve f so the combination
# hits the target (interactions included). f ~ 1/3 under linearity.
need$f_combo <- sapply(seq_len(nrow(need)), function(i) {
  g <- function(f) addon_at(dg = f * need$dg[i], dL_yr = f * need$dL_yr[i], dy = f * need$dy[i]) - need$addon_bp[i]
  uniroot(g, c(0, 1), tol = 1e-6)$root
})
need$combo_growth <- sapply(seq_len(nrow(need)), function(i) 100 * (prod(1 + win$g + need$f_combo[i] * need$dg[i] / 200)^(1 / 5) - 1))
need$combo_losses_M <- (L_model_yr + need$f_combo * need$dL_yr) / 1e6
need$combo_yield    <- y_model_avg + need$f_combo * need$dy / 100
thr$`Balanced combination (growth / losses per yr / yield)` <-
  sprintf("%.1f%% / $%.0fM / %.2f%%", need$combo_growth, need$combo_losses_M, need$combo_yield)
thr$`Share of each lever's solo move` <- sprintf("%.0f%%", 100 * need$f_combo)
print(thr[, c(1, 3, 5, 9, 10, 11)], row.names = FALSE)
# check the combinations land on target
print(data.frame(target = need$nol, nol_combo = round(nol_of(sapply(seq_len(nrow(need)), function(i)
  addon_at(dg = need$f_combo[i] * need$dg[i], dL_yr = need$f_combo[i] * need$dL_yr[i], dy = need$f_combo[i] * need$dy[i]))), 3)))
write.csv(thr, "tables/thresholds_2026.csv", row.names = FALSE)

## 6. two-lever combination: yield held at the projection, growth and losses each do half --------
# (only ~29% of the book reprices within the window, so the yield is largely locked in)
need$f_combo2 <- sapply(seq_len(nrow(need)), function(i) {
  g <- function(f) addon_at(dg = f * need$dg[i], dL_yr = f * need$dL_yr[i]) - need$addon_bp[i]
  uniroot(g, c(0, 1.2), tol = 1e-6)$root
})
need$combo2_growth   <- sapply(seq_len(nrow(need)), function(i) 100 * (prod(1 + win$g + need$f_combo2[i] * need$dg[i] / 200)^(1 / 5) - 1))
need$combo2_losses_M <- (L_model_yr + need$f_combo2 * need$dL_yr) / 1e6
thr$`Combination, yield fixed (growth / losses per yr)` <- sprintf("%.1f%% / $%.0fM", need$combo2_growth, need$combo2_losses_M)
thr$`Share of each lever's solo move (yield fixed)` <- sprintf("%.0f%%", 100 * need$f_combo2)
print(thr[, c(1, 3, 5, 12, 13)], row.names = FALSE)
print(data.frame(target = need$nol, nol_combo2 = round(nol_of(sapply(seq_len(nrow(need)), function(i)
  addon_at(dg = need$f_combo2[i] * need$dg[i], dL_yr = need$f_combo2[i] * need$dL_yr[i]))), 3))[c(1, 6, 14), ])
write.csv(thr, "tables/thresholds_2026.csv", row.names = FALSE)
