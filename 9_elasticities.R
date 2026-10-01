# 9_elasticities.R ---------------------------------------------------------------------

## 1. replay any vintage from period 1 with optional perturbations -------------------------------
replay <- function(v, dg = 0, dy = 0, dL_yr = 0, dL_bp_yr = 0, dopex_yr = 0, rate_shock = 0,
                   g_path = NULL, y_path = NULL, L_path = NULL, opex_path = NULL, dist_on = TRUE) {
  d <- panel[panel$vintage == v, ]; d <- d[order(d$period), ]
  s <- d[d$period == 0, ]; fwd <- d[d$period >= 1, ]
  P <- s$portfolio; CD <- s$cap_deposit; E <- s$equity; S <- s$insured_shares; nol_in <- s$nol_input / 100
  g <- if (is.null(g_path)) fwd$g else g_path
  y <- if (is.null(y_path)) fwd$portfolio_yield / 100 else y_path
  L <- if (is.null(L_path)) fwd$ins_losses else L_path
  ox <- if (is.null(opex_path)) fwd$opex else opex_path
  win <- fwd$period <= 10
  g[win] <- g[win] + dg / 2 / 100
  y[win] <- y[win] + dy / 1e4
  L[win] <- L[win] + (dL_yr / 2) + (dL_bp_yr / 1e4 * S / 2)
  ox[win] <- ox[win] + dopex_yr / 2
  if (rate_shock != 0) {                       # share of the book repriced by each period: overnight balance (= maturities - non-overnight
    ovn  <- pmax(fwd$maturities - fwd$nonovn_maturities, 0)          # maturities) reprices every period; term maturities reprice once
    frac <- pmin(1, (ovn + cumsum(fwd$nonovn_maturities)) / P)
    y[win] <- y[win] + rate_shock / 1e4 * frac[win]
  }
  er <- numeric(nrow(fwd)); er_prev <- (CD + E) / S
  for (t in seq_len(nrow(fwd))) {
    S_new <- S * (1 + g[t]); CD_new <- 0.01 * S
    NI <- P * y[t] / 2 - ox[t] - L[t] + fwd$g_fee[t] + fwd$ngn_adj[t]
    dist <- if (dist_on) max(0, er_prev - nol_in) * S else 0
    E <- E + NI - dist; P <- P + NI
    er[t] <- (CD_new + E) / S_new; er_prev <- er[t]; S <- S_new; CD <- CD_new
  }
  c(er1 = (s$cap_deposit + s$equity) / s$insured_shares, er = er)   # er[1] = period 1 ... er[10] = period 10
}
addon_bp <- function(path) 1e4 * (path["er1"] - path[["er10"]])         # Dec Y minus Dec Y+5 (the sheet's convention)
nol_of   <- function(a) pmax(1.20, 1.20 + a / 100)

## 2. elasticities by vintage --------------------------------------------------------------------
vints <- sort(unique(panel$vintage))
elas <- do.call(rbind, lapply(vints, function(v) {
  base <- replay(v); a0 <- addon_bp(base)                              # add-on with the model's distribution rule
  b0 <- addon_bp(replay(v, dist_on = FALSE))                           # structural sensitivities: no distribution cap
  dER <- function(...) -(addon_bp(replay(v, ..., dist_on = FALSE)) - b0)   # bp change in ER at Dec+4 (positive = ratio higher)
  e_g  <- dER(dg = 1); e_L <- dER(dL_yr = 100e6); e_Lbp <- dER(dL_bp_yr = 1); e_y <- dER(dy = 100); e_r <- dER(rate_shock = 100); e_o <- dER(dopex_yr = 100e6)
  # pass-through fraction actually used for the rate shock (window average)
  d <- panel[panel$vintage == v & panel$period >= 1 & panel$period <= 10, ]; d <- d[order(d$period), ]; P1 <- panel$portfolio[panel$vintage == v & panel$period == 0]
  fr <- pmin(1, (pmax(d$maturities - d$nonovn_maturities, 0) + cumsum(d$nonovn_maturities)) / P1)
  data.frame(vintage = v, addon_bp = a0, nol = nol_of(a0), dead_zone_bp = max(0, -a0),
             ER_share_g_1pp = e_g, ER_loss_100M = e_L, ER_loss_1bp = e_Lbp, ER_yield_100bp = e_y, ER_rate_100bp = e_r, ER_opex_100M = e_o,
             pass_through = mean(fr), P_to_S = 100 * P1 / panel$insured_shares[panel$vintage == v & panel$period == 0],
             e_comp = panel$eq_comp[panel$vintage == v & panel$period == 0])
}))
elas$NOL_share_g_1pp  <- ifelse(elas$addon_bp > 0, -elas$ER_share_g_1pp, 0)      # bp of NOL per +1 pp/yr share growth (at the margin)
elas$NOL_loss_100M    <- ifelse(elas$addon_bp > 0, -elas$ER_loss_100M, 0)
elas$NOL_yield_100bp  <- ifelse(elas$addon_bp > 0, -elas$ER_yield_100bp, 0)
elas$NOL_rate_100bp   <- ifelse(elas$addon_bp > 0, -elas$ER_rate_100bp, 0)
print(round(elas, 2))


## 3. how far each lever is from moving the NOL, and what would move it 13 bp ------------------------
levers <- data.frame(vintage = elas$vintage,
  share_g_pp_for_13bp = (13 - elas$addon_bp) / -elas$ER_share_g_1pp,      # extra pp/yr of share growth to reach 1.33
  losses_M_for_13bp   = (13 - elas$addon_bp) / -elas$ER_loss_100M * 100,  # extra $M/yr of losses
  yield_bp_for_13bp   = (13 - elas$addon_bp) / -elas$ER_yield_100bp * 100) # bp lower yield
print(round(levers, 0))

## 4. stress-sized shocks through the elasticities (comparable units: bp of ER) ----------------------------
shock <- data.frame(vintage = elas$vintage,
  share_surge_15pp_1yr = replicate(1, sapply(elas$vintage, function(v) { b <- replay(v, dist_on = FALSE); p <- replay(v, dist_on = FALSE, g_path = { d <- panel[panel$vintage == v & panel$period >= 1, ]; d <- d[order(d$period), ]; g <- d$g; g[1:2] <- g[1:2] + 0.075; g }); -(addon_bp(p) - addon_bp(b)) })),
  loss_wave_5bp_2yr    = sapply(elas$vintage, function(v) { b <- replay(v, dist_on = FALSE); S1 <- panel$insured_shares[panel$vintage == v & panel$period == 0]; p <- replay(v, dist_on = FALSE, L_path = { d <- panel[panel$vintage == v & panel$period >= 1, ]; d <- d[order(d$period), ]; L <- d$ins_losses; L[1:4] <- L[1:4] + 5 / 1e4 * S1 / 2; L }); -(addon_bp(p) - addon_bp(b)) }),
  yield_minus_100bp    = -elas$ER_yield_100bp,
  rate_minus_100bp     = -elas$ER_rate_100bp)
print(round(shock, 1))


## 5. swap attribution: 2022 assumptions into the 2026 projection, one lever at a time ----------------------
path_of <- function(v, col) { d <- panel[panel$vintage == v & panel$period >= 1, ]; d <- d[order(d$period), ]; d[[col]] }
S26 <- panel$insured_shares[panel$vintage == 2026 & panel$period == 0]; S22 <- panel$insured_shares[panel$vintage == 2022 & panel$period == 0]
base26 <- addon_bp(replay(2026))
swap <- c(
  `2026 projection (as is)`                     = base26,
  `... with 2022 share-growth path`             = addon_bp(replay(2026, g_path = path_of(2022, "g"))),
  `... with 2022 loss path (scaled to shares)`  = addon_bp(replay(2026, L_path = path_of(2022, "ins_losses") * S26 / S22)),
  `... with 2022 yield path`                    = addon_bp(replay(2026, y_path = path_of(2022, "portfolio_yield") / 100)),
  `... with 2022 operating-cost path`           = addon_bp(replay(2026, opex_path = path_of(2022, "opex"))),
  `... with all four 2022 paths`                = addon_bp(replay(2026, g_path = path_of(2022, "g"), L_path = path_of(2022, "ins_losses") * S26 / S22,
                                                                  y_path = path_of(2022, "portfolio_yield") / 100, opex_path = path_of(2022, "opex"))),
  `2022 projection (as is)`                     = addon_bp(replay(2022)))
swap_tbl <- data.frame(case = names(swap), addon_bp = round(swap, 1), nol = round(nol_of(swap), 2), row.names = NULL)
swap_tbl$effect_bp <- c(NA, round(swap[2:5] - base26, 1), round(swap[6] - base26, 1), NA)
print(swap_tbl)


write.csv(elas, "elasticities_by_vintage.csv", row.names = FALSE)
write.csv(swap_tbl, "swap_attribution_2022_2026.csv", row.names = FALSE)

## 6. charts -------------------------------------------------------------------------------------
NAVY <- "#1F3A5F"; TEAL <- "#2A9D8F"; RED <- "#B23A48"; GREY <- "#8A8F98"; GOLD <- "#D9A441"; DGREY <- "#3C4048"; LGREY <- "#D5D8DD"
dir.create("figures", showWarnings = FALSE)
# Fig 7: elasticities by vintage (ER solid; NOL hollow, dropping to zero in the dead zone)
png("figures/fig7_elasticities.png", width = 6.5, height = 4.6, units = "in", res = 220)
par(mfrow = c(2, 2), mar = c(3, 3.6, 2.2, 0.8), mgp = c(2.2, 0.5, 0), tcl = -0.25, cex.axis = 0.75, cex.lab = 0.8, col.axis = DGREY, col.lab = DGREY, fg = DGREY)
panel_e <- function(er, nol, ylab, main, col, ylim) {
  plot(elas$vintage, er, type = "n", ylim = ylim, xlab = "", ylab = ylab, axes = FALSE); axis(1, at = elas$vintage, lwd = 0); axis(2, las = 1, lwd = 0, lwd.ticks = 0.6)
  abline(h = pretty(ylim), col = LGREY, lwd = 0.6); abline(h = 0, col = DGREY, lwd = 0.8); box(col = LGREY)
  lines(elas$vintage, er, col = col, lwd = 2.2); points(elas$vintage, er, pch = 19, col = col, cex = 1.1)
  if (!is.null(nol)) { lines(elas$vintage, nol, col = GOLD, lwd = 1.6, lty = 2); points(elas$vintage, nol, pch = 21, bg = "white", col = GOLD, cex = 1.1) }
  mtext(main, side = 3, line = 0.5, cex = 0.72, font = 2, col = DGREY)
}
panel_e(elas$ER_share_g_1pp, -elas$NOL_share_g_1pp, "bp per +1 pp/yr", "Share growth (+1 pp/yr for five years)", RED, c(min(-2.2, floor(min(elas$ER_share_g_1pp) * 2) / 2), 0.3))
legend("bottomleft", c("effect on the equity ratio", "effect on the NOL (sign flipped; 0 = floor binds)"), col = c(RED, GOLD), lty = c(1, 2), pch = c(19, 21), pt.bg = "white", bty = "n", cex = 0.6)
panel_e(elas$ER_loss_100M, -elas$NOL_loss_100M, "bp per +$100M/yr", "Insurance losses (+$100M/yr for five years)", GOLD, c(min(-2.6, floor(min(elas$ER_loss_100M) * 2) / 2), 0.3))
panel_e(elas$ER_yield_100bp, -elas$NOL_yield_100bp, "bp per +100 bp", "Investment yield (+100 bp, parallel shift)", TEAL, c(-0.5, max(5, ceiling(max(elas$ER_yield_100bp)) + 0.5)))
panel_e(100 * elas$pass_through, NULL, "% of book repriced (window avg)", "Rate pass-through (% of book repricing in window)", NAVY, c(0, 60))
dev.off()
# Fig 8: swap attribution, 2026 add-on -> 2022 add-on
png("figures/fig8_swap_attribution.png", width = 6.5, height = 3.0, units = "in", res = 220)
par(mar = c(3.8, 3.8, 1.2, 1), mgp = c(2.4, 0.6, 0), tcl = -0.25, cex.axis = 0.75, cex.lab = 0.85, col.axis = DGREY, col.lab = DGREY, fg = DGREY)
eff <- c(swap[1], swap[4] - swap[1], swap[2] - swap[1], swap[3] - swap[1], swap[5] - swap[1], swap[7] - swap[6], swap[7])
labs <- c("2026 add-on\n(as projected)", "2022 yield\npath", "2022 share-\ngrowth path", "2022 loss\npath", "2022 operating-\ncost path", "2022 starting\nposition*", "2022 add-on\n(as projected)")
plot(NA, xlim = c(0.4, 7.6), ylim = c(-6, 15), xlab = "", ylab = "NOL add-on, basis points", axes = FALSE)
for (pp in 1:2) axis(1, at = seq(pp, 7, 2), labels = labs[seq(pp, 7, 2)], lwd = 0, padj = 0.5, cex.axis = 0.62); axis(2, at = seq(-5, 15, 5), las = 1, lwd = 0, lwd.ticks = 0.6); abline(h = seq(-5, 15, 5), col = LGREY, lwd = 0.6); abline(h = 0, col = DGREY, lwd = 0.8); box(col = LGREY)
run <- eff[1]; rect(0.7, 0, 1.3, run, col = NAVY, border = NA); text(1, run - 0.8, sprintf("%.1f", run), font = 2, cex = 0.7, col = NAVY)
for (i in 2:6) { rect(i - 0.3, run, i + 0.3, run + eff[i], col = TEAL, border = NA); text(i, run + eff[i] + 0.7, sprintf("+%.1f", eff[i]), font = 2, cex = 0.7, col = TEAL)
                 segments(i - 0.7, run, i + 0.3, run, col = GREY, lty = 3, lwd = 0.8); run <- run + eff[i] }
rect(6.7, 0, 7.3, eff[7], col = NAVY, border = NA); text(7, eff[7] + 0.7, sprintf("%.1f", eff[7]), font = 2, cex = 0.7, col = NAVY)
dev.off()


## 7. report tables and key numbers (used by the Rmd) ---------------------------------------------
bpf <- function(x, d = 1) sprintf(paste0("%+.", d, "f bp"), x)
tab4 <- data.frame(
  `Model year    ` = elas$vintage,
  `Share growth (+1 pp/yr)`            = bpf(elas$ER_share_g_1pp),
  `Losses (+$100M/yr)`                 = bpf(elas$ER_loss_100M),
  `Losses (+1 bp of shares/yr)`        = bpf(elas$ER_loss_1bp),
  `Investment yield (+100 bp)`         = bpf(elas$ER_yield_100bp),
  `Market rates (+100 bp, with repricing lag)` = bpf(elas$ER_rate_100bp),
  `Share of book repricing`            = sprintf("%.0f%%", 100 * elas$pass_through),
  `Does the NOL respond?`              = ifelse(elas$addon_bp > 0, "Yes, one for one", sprintf("No \u2014 %.1f bp cushion first", elas$dead_zone_bp)),
  check.names = FALSE)
swap_lab <- c("2026 projection, as is", "\u2026 with the 2022 share-growth path", "\u2026 with the 2022 loss path (scaled to shares)",
              "\u2026 with the 2022 yield path", "\u2026 with the 2022 operating-cost path", "\u2026 with all four 2022 paths", "2022 projection, as is")
tab5 <- data.frame(Case = swap_lab, `NOL add-on` = sprintf("%+.1f bp", swap_tbl$addon_bp),
                   `Implied NOL` = ifelse(swap_tbl$nol <= 1.20, "1.20% (floor)", sprintf("%.2f%%", swap_tbl$nol)),
                   `Effect of the swap` = ifelse(is.na(swap_tbl$effect_bp), "\u2014", sprintf("%+.1f bp", swap_tbl$effect_bp)), check.names = FALSE)
if (exists("kn")) {
  kn$el_g_2020 <- sprintf("%.1f", -elas$ER_share_g_1pp[elas$vintage == 2020]); kn$el_g_2026 <- sprintf("%.1f", -elas$ER_share_g_1pp[elas$vintage == 2026])
  kn$el_y_2020 <- sprintf("%.1f", elas$ER_yield_100bp[elas$vintage == 2020]);  kn$el_y_2026 <- sprintf("%.1f", elas$ER_yield_100bp[elas$vintage == 2026])
  kn$pt_2023 <- sprintf("%.0f%%", 100 * elas$pass_through[elas$vintage == 2023]); kn$pt_2026 <- sprintf("%.0f%%", 100 * elas$pass_through[elas$vintage == 2026])
  kn$dead_2026 <- sprintf("%.1f", elas$dead_zone_bp[elas$vintage == 2026])
  kn$lev_g_2026 <- sprintf("%.0f", levers$share_g_pp_for_13bp[levers$vintage == 2026]); kn$lev_L_2026 <- sprintf("$%.0f million", levers$losses_M_for_13bp[levers$vintage == 2026])
  kn$lev_y_2026 <- sprintf("%.1f", -levers$yield_bp_for_13bp[levers$vintage == 2026] / 100); kn$lev_L_2022 <- sprintf("$%.0f million", levers$losses_M_for_13bp[levers$vintage == 2022])
  kn$lev_L_2023 <- sprintf("$%.0f million", levers$losses_M_for_13bp[levers$vintage == 2023]); kn$lev_g_2026_pct <- sprintf("%.0f%%", elas$vintage[1] * 0 + 100 * (prod(1 + panel$g[panel$vintage == 2026 & panel$period >= 1 & panel$period <= 10] + levers$share_g_pp_for_13bp[levers$vintage == 2026] / 200)^(1 / 5) - 1))
  kn$swing <- sprintf("%.1f", swap[7] - swap[1]); kn$sw_y <- sprintf("%.1f", swap[4] - swap[1]); kn$sw_g <- sprintf("%.1f", swap[2] - swap[1])
  kn$sw_L <- sprintf("%.1f", swap[3] - swap[1]); kn$sw_o <- sprintf("%.1f", swap[5] - swap[1]); kn$sw_start <- sprintf("%.1f", swap[7] - swap[6])
}
write.csv(tab4, "tables/tab4.csv", row.names = FALSE); write.csv(tab5, "tables/tab5.csv", row.names = FALSE)
