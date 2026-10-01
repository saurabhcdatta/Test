# 12_tweaks.R ------------------------------------------------------------------------------

replay0 <- function(v, g_path = NULL, y_path = NULL, L_bp_yrs = NULL, dist_on = TRUE) {
  d <- panel[panel$vintage == v, ]; d <- d[order(d$period), ]
  p0 <- d[d$period == -1, ]; fwd <- d[d$period >= 0, ]
  P <- p0$portfolio; CD <- p0$cap_deposit; E <- p0$equity; S <- p0$insured_shares; nol_in <- p0$nol_input / 100
  g <- if (is.null(g_path)) fwd$g else g_path
  y <- if (is.null(y_path)) fwd$portfolio_yield / 100 else y_path
  L <- fwd$ins_losses; ox <- fwd$opex; fees <- fwd$g_fee + fwd$ngn_adj
  er <- numeric(nrow(fwd)); er_prev <- (CD + E) / S
  for (t in seq_len(nrow(fwd))) {
    Lt <- if (!is.null(L_bp_yrs) && t <= 2 * length(L_bp_yrs)) L_bp_yrs[ceiling(t / 2)] / 1e4 * S / 2 else L[t]
    S_new <- S * (1 + g[t]); CD_new <- 0.01 * S
    NI <- P * y[t] / 2 - ox[t] - Lt + fees[t]
    dist <- if (dist_on) max(0, er_prev - nol_in) * S else 0
    E <- E + NI - dist; P <- P + NI
    er[t] <- (CD_new + E) / S_new; er_prev <- er[t]; S <- S_new; CD <- CD_new
  }
  list(er0 = (p0$cap_deposit + p0$equity) / p0$insured_shares, base_tu = 0.01 + p0$equity / p0$insured_shares,
       y0 = p0$portfolio_yield / 100, er = er)
}
# check: the replay from period 0 reproduces the sheet (max abs deviation in pp)
max(sapply(sort(unique(panel$vintage)), function(v) { d <- panel[panel$vintage == v & panel$period >= 0, ]; d <- d[order(d$period), ]
  max(abs(100 * replay0(v)$er - d$equity_ratio)) }))

## the ladder ---------------------------------------------------------------------------------
nol_cur <- function(r) 1.20 + 100 * (r$er[1] - r$er[11])                      # current convention: period 0 -> period 10
nol_trough <- function(r, base) 1.20 + 100 * (base - min(r$er))               # trough convention
ladder <- do.call(rbind, lapply(sort(unique(panel$vintage)), function(v) {
  d <- panel[panel$vintage == v & panel$period >= 0, ]; d <- d[order(d$period), ]
  g_rec <- d$g; g_rec[1:2] <- (1.12)^(1 / 2) - 1                                
  r0 <- replay0(v)
  r3 <- replay0(v, y_path = rep(r0$y0, 11))
  r4 <- replay0(v, y_path = rep(r0$y0, 11), g_path = g_rec)
  r5 <- replay0(v, y_path = rep(r0$y0, 11), g_path = g_rec, L_bp_yrs = c(2, 2))
  data.frame(vintage = v, nol_in_force = d$nol_input[1],
             s0_current      = nol_cur(r0),
             s1_trough       = nol_trough(r0, r0$er0),
             s2_trued_base   = nol_trough(r0, r0$base_tu),
             s3_yield_frozen = nol_trough(r3, r3$base_tu),
             s4_rec_shares   = nol_trough(r4, r4$base_tu),
             s5_hist_losses  = nol_trough(r5, r5$base_tu),
             s6_concentration= nol_trough(r5, r5$base_tu) + 0.05,
             trough_when_s5  = format(as.Date(d$period_end[which.min(r5$er)]), "%b-%y"))
}))
ladder[, 3:9] <- lapply(ladder[, 3:9], function(x) pmax(1.20, x))               # statutory floor
print(ladder, digits = 4)
cat("\nrange across vintages:\n"); print(round(sapply(ladder[, 3:9], function(x) diff(range(x))), 3))
write.csv(ladder, "tables/tweak_ladder.csv", row.names = FALSE)
