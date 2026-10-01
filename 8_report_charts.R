# 8_report_charts.R ---------------------------------------------------------------------


NAVY <- "#1F3A5F"; TEAL <- "#2A9D8F"; RED <- "#B23A48"; GREY <- "#8A8F98"; GOLD <- "#D9A441"; DGREY <- "#3C4048"; LGREY <- "#D5D8DD"
dir.create("figures", showWarnings = FALSE)
open_png <- function(file, h) png(file.path("figures", file), width = 6.5, height = h, units = "in", res = 220)
base_par <- function(mar = c(3.2, 3.8, 1.2, 1)) par(mar = mar, mgp = c(2.4, 0.6, 0), tcl = -0.25, family = "sans",
                                                   col.axis = DGREY, col.lab = DGREY, fg = DGREY, cex.axis = 0.8, cex.lab = 0.85, xaxs = "i", yaxs = "i")
hgrid <- function(at) abline(h = at, col = LGREY, lwd = 0.7)
date_to_x <- function(d) as.numeric(substr(d, 1, 4)) + ifelse(substr(d, 6, 7) == "06", 0.5, 1.0)   # balance-sheet date -> calendar position
vints <- sort(unique(panel$vintage))

## Fig 1: ER projections by model year ------------------------------------------------------------
open_png("fig1_er_paths.png", 3.2); base_par()
plot(NA, xlim = c(2020.4, 2032.3), ylim = c(1.14, 1.36), xlab = "", ylab = "Equity ratio (%)", axes = FALSE)
axis(1, at = 2021:2031, lwd = 0, lwd.ticks = 0.6); axis(2, at = seq(1.15, 1.35, 0.05), labels = sprintf("%.2f%%", seq(1.15, 1.35, 0.05)), las = 1, lwd = 0, lwd.ticks = 0.6)
hgrid(seq(1.15, 1.35, 0.05)); box(col = LGREY)
abline(h = 1.33, col = GOLD, lwd = 1.3, lty = 2); text(2020.55, 1.336, "NOL in force 1.33%", col = GOLD, cex = 0.7, adj = 0, font = 2)
abline(h = 1.25, col = GREY, lwd = 1, lty = 3);   text(2032.2, 1.252, "1.25%", col = GREY, cex = 0.7, adj = 1)
abline(h = 1.20, col = RED, lwd = 1.3, lty = 2);  text(2020.55, 1.204, "Statutory floor 1.20%", col = RED, cex = 0.7, adj = 0, font = 2)
pal <- hcl.colors(length(vints), "viridis", rev = FALSE)[c(2:7, 7)]; pal <- hcl.colors(9, "viridis")[2:8]
for (i in seq_along(vints)) {
  d <- panel[panel$vintage == vints[i], ]; d <- d[order(d$period), ]
  x <- date_to_x(d$period_end); lines(x, d$equity_ratio, col = pal[i], lwd = if (vints[i] == 2026) 2.6 else 1.8)
  text(x[11] + 0.1, d$equity_ratio[11], vints[i], col = pal[i], cex = 0.7, adj = 0, font = 2)
}
act <- panel[panel$period == -1, ]; lines(date_to_x(act$period_end), act$equity_ratio, lwd = 2.6); points(date_to_x(act$period_end), act$equity_ratio, pch = 19, cex = 0.7)
legend("bottomleft", "June actuals", lwd = 2.6, pch = 19, bty = "n", cex = 0.75)
dev.off()

## Fig 2: NOL by model year ----------------------------------------------------------------------------
open_png("fig2_nol.png", 2.8); base_par(mar = c(3.4, 3.8, 1.2, 1))
plot(NA, xlim = c(2019.6, 2027.1), ylim = c(1.15, 1.42), xlab = "Model year (June data)", ylab = "NOL (%)", axes = FALSE)
rect(2019.6, 1.15, 2027.1, 1.25, col = "#F8ECEE", border = NA)
axis(1, at = vints, lwd = 0, lwd.ticks = 0.6); axis(2, at = seq(1.15, 1.40, 0.05), labels = sprintf("%.2f%%", seq(1.15, 1.40, 0.05)), las = 1, lwd = 0, lwd.ticks = 0.6)
hgrid(seq(1.15, 1.40, 0.05)); box(col = LGREY)
lines(c(nol_tbl$vintage, 2027), c(nol_tbl$nol_in_force, tail(nol_tbl$nol_in_force, 1)), type = "s", col = GOLD, lwd = 2)
abline(h = 1.25, col = GREY, lty = 3); text(2026.6, 1.253, "1.25%", col = GREY, cex = 0.7)
abline(h = 1.20, col = RED, lty = 2, lwd = 1.3); text(2026.6, 1.186, "floor 1.20%", col = RED, cex = 0.7, font = 2)
lines(nol_tbl$vintage, nol_tbl$nol_a, col = NAVY, lwd = 2.2); points(nol_tbl$vintage, nol_tbl$nol_a, pch = 19, col = NAVY, cex = 1.1)
text(nol_tbl$vintage, nol_tbl$nol_a + 0.014, sprintf("%.2f%%", nol_tbl$nol_a), col = NAVY, cex = 0.7, font = 2)
legend("topright", c("NOL implied by the formula", "NOL set by the Board"), col = c(NAVY, GOLD), lwd = 2, pch = c(19, NA), bty = "n", cex = 0.75)
dev.off()

## Fig 3: net income vs break-even ----------------------------------------------------------------------
open_png("fig3_ni_breakeven.png", 2.9); base_par()
ni <- wstat$ni_M_yr; be <- ann$breakeven_ni_M
yl3 <- c(min(-280, floor(min(ni) / 100) * 100 - 80), max(760, ceiling(max(c(ni, be)) / 100) * 100 + 60))
plot(NA, xlim = c(2019.5, 2026.5), ylim = yl3, xlab = "", ylab = "$ millions per year", axes = FALSE)
axis(1, at = vints, lwd = 0, lwd.ticks = 0.6); axis(2, at = pretty(yl3), las = 1, lwd = 0, lwd.ticks = 0.6); hgrid(pretty(yl3)); box(col = LGREY)
rect(vints - 0.31, pmin(ni, 0), vints + 0.31, pmax(ni, 0), col = ifelse(ni < 0, RED, TEAL), border = NA); abline(h = 0, col = DGREY, lwd = 0.8)
lines(vints, be, col = NAVY, lwd = 2); points(vints, be, pch = 18, col = NAVY, cex = 1.3)
lab <- sprintf("%+d", round(ni)); inside <- abs(ni) > 90
text(vints[inside], ni[inside] / 2, lab[inside], col = "white", font = 2, cex = 0.7)
text(vints[!inside], ni[!inside] + ifelse(ni[!inside] >= 0, 25, -25), lab[!inside], font = 2, cex = 0.7, col = DGREY)
legend("topleft", c("Needed to keep pace with share growth", "Projected net income"), col = c(NAVY, RED), lwd = c(2, NA), pch = c(18, 15), pt.cex = c(1.3, 1.6), bty = "n", cex = 0.75)
dev.off()

## Fig 4: sources of the four-year change (stacked, mixed signs) ------------------------------------------
open_png("fig4_decomposition.png", 3.0); base_par()
yl4 <- c(min(-5, floor(min(decomp$dilution_bp + pmin(decomp$cd_lag_bp, 0) + pmin(decomp$earnings_bp, 0)) - 2)), ceiling(max(pmax(decomp$earnings_bp, 0) + pmax(decomp$cd_lag_bp, 0)) + 3))
plot(NA, xlim = c(2019.5, 2027.0), ylim = yl4, xlab = "", ylab = "Basis points over five years", axes = FALSE)
axis(1, at = vints, lwd = 0, lwd.ticks = 0.6); axis(2, at = pretty(yl4), las = 1, lwd = 0, lwd.ticks = 0.6); hgrid(pretty(yl4)); box(col = LGREY)
comp <- rbind(earnings = decomp$earnings_bp, dilution = decomp$dilution_bp, lag = decomp$cd_lag_bp)
ccol <- c(earnings = TEAL, dilution = RED, lag = GREY)
for (j in seq_along(vints)) { pb <- 0; nb <- 0
  for (k in rownames(comp)) { v <- comp[k, j]
    if (v >= 0) { rect(vints[j] - 0.31, pb, vints[j] + 0.31, pb + v, col = ccol[k], border = "white", lwd = 0.5); pb <- pb + v }
    else        { rect(vints[j] - 0.31, nb + v, vints[j] + 0.31, nb, col = ccol[k], border = "white", lwd = 0.5); nb <- nb + v } } }
abline(h = 0, col = DGREY, lwd = 0.8)
points(vints, decomp$total_bp, pch = 18, cex = 1.5); text(vints + 0.37, decomp$total_bp, sprintf("%+.1f", decomp$total_bp), adj = 0, cex = 0.7, font = 2)
legend("topleft", c("Fund earnings", "Share growth diluting the ratio", "Timing of the 1% deposit true-up", "Net change in ratio"),
       fill = c(TEAL, RED, GREY, NA), border = NA, pch = c(NA, NA, NA, 18), bty = "n", cex = 0.7, ncol = 2)
dev.off()

## Fig 5: bridge 2024 -> 2026 (window attribution) ------------------------------------------------------------
open_png("fig5_waterfall.png", 2.9); base_par(mar = c(3.6, 3.8, 1.2, 1))
steps <- c(attr(attr26, "ni_from"), attr26$change_M_yr, attr(attr26, "ni_to"))
labs  <- c("2024\nprojection", "Higher\ninvestment yield", "Larger\nportfolio", "Lower NCUA\nbudget", "Lower assumed\nlosses", "2026\nprojection")
yl5 <- c(0, ceiling(max(cumsum(steps[1:5]), steps[6]) / 100) * 100 + 60)
plot(NA, xlim = c(0.4, 6.6), ylim = yl5, xlab = "", ylab = "Projected net income ($M per year)", axes = FALSE)
axis(1, at = 1:6, labels = labs, lwd = 0, padj = 0.6, cex.axis = 0.7); axis(2, at = pretty(yl5), las = 1, lwd = 0, lwd.ticks = 0.6); hgrid(pretty(yl5)); box(col = LGREY)
run <- 0
for (i in 1:6) { v <- steps[i]
  if (i %in% c(1, 6)) { rect(i - 0.3, 0, i + 0.3, v, col = NAVY, border = NA); text(i, v + 20, sprintf("$%.0fM", v), font = 2, cex = 0.7); run <- v }
  else { rect(i - 0.3, run, i + 0.3, run + v, col = TEAL, border = NA); text(i, run + v + 20, sprintf("+$%.0fM", v), font = 2, cex = 0.7, col = TEAL)
         segments(i - 0.7, run, i + 0.3, run, col = GREY, lty = 3, lwd = 0.8); run <- run + v } }
dev.off()

## Fig 6: factor shares, two panels ---------------------------------------------------------------------------
open_png("fig6_factor_shares.png", 3.5)
layout(matrix(1:2, 1), widths = c(1.15, 1)); base_par(mar = c(5.2, 3.8, 2.2, 0.6))
fcol <- c(dilution = RED, opex = GREY, losses = GOLD, lag = NAVY, interest = TEAL)
flab <- c(dilution = "Share growth (dilution)", opex = "NCUA operating cost", losses = "Insurance losses", lag = "Deposit true-up timing", interest = "Investment income")
stack_plot <- function(M, ylim, ylab, main, min_lab = 5) {
  plot(NA, xlim = c(0.4, ncol(M) + 0.6), ylim = ylim, xlab = "", ylab = ylab, axes = FALSE, main = "")
  for (pass in 1:2) axis(1, at = seq(pass, ncol(M), 2), labels = colnames(M)[seq(pass, ncol(M), 2)], lwd = 0, cex.axis = 0.7);   # two passes so no year is dropped axis(2, at = seq(0, 100, 25), las = 1, lwd = 0, lwd.ticks = 0.6); hgrid(seq(0, 100, 25)); box(col = LGREY)
  for (j in seq_len(ncol(M))) { b <- 0
    for (k in rownames(M)) { v <- M[k, j]; rect(j - 0.34, b, j + 0.34, b + v, col = fcol[k], border = "white", lwd = 0.5)
      if (v >= min_lab) text(j, b + v / 2, sprintf("%.0f", v), col = "white", font = 2, cex = 0.65); b <- b + v } }
  mtext(main, side = 3, line = 0.4, cex = 0.75, font = 2, col = DGREY)
}
MA <- t(shareA[, c("dilution", "opex", "losses", "lag", "interest")]); colnames(MA) <- rownames(shareA)
stack_plot(MA, c(0, 100), "% of gross movement in the ratio", "A. Share of all factor effects\n(investment income works against the add-on)")
MB <- t(pmax(shareB[, c("dilution", "opex", "losses", "lag")], 0)); colnames(MB) <- rownames(shareB)
stack_plot(MB, c(0, 120), "% of factors building the add-on", "B. What builds the NOL add-on\n(sums to 100 each year)", min_lab = 6)
text(seq_len(ncol(MB)), 104, paste0(offset$coverage_pct, "%"), font = 2, cex = 0.7, col = ifelse(offset$coverage_pct >= 100, TEAL, DGREY))
text(4, 114, "Share of the drag offset by investment income", cex = 0.65, font = 3, col = DGREY)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE); plot.new()
legend("bottom", flab[c("interest", "lag", "losses", "opex", "dilution")], fill = fcol[c("interest", "lag", "losses", "opex", "dilution")], border = NA, bty = "n", cex = 0.7, ncol = 3, inset = 0.005)
dev.off()

## Fig 9: stress scenarios ----------------------------------------------------------------------------------
open_png("fig9_stress.png", 2.8); base_par(mar = c(3.2, 15.5, 1.2, 1))
n <- nrow(stress); ypos <- rev(seq_len(n))
plot(NA, xlim = c(1.18, 1.52), ylim = c(0.4, n + 0.6), xlab = "", ylab = "", axes = FALSE)
axis(1, at = seq(1.20, 1.50, 0.05), labels = sprintf("%.2f%%", seq(1.20, 1.50, 0.05)), lwd = 0, lwd.ticks = 0.6)
axis(2, at = ypos, labels = stress$scenario, las = 1, lwd = 0, cex.axis = 0.68); box(col = LGREY)
abline(v = seq(1.25, 1.50, 0.05), col = LGREY, lwd = 0.7)
nol_bar <- ifelse(seq_len(n) == 1, 1.20, stress$nol)             # model row shown at the formula's floor
rect(1.20, ypos - 0.31, nol_bar, ypos + 0.31, col = ifelse(seq_len(n) == 1, GREY, NAVY), border = NA)
text(nol_bar + 0.006, ypos, sprintf("%.2f%%", nol_bar), adj = 0, font = 2, cex = 0.72, col = ifelse(seq_len(n) == 1, GREY, NAVY))
abline(v = 1.33, col = GOLD, lwd = 1.5, lty = 2); abline(v = 1.20, col = RED, lwd = 1.2); abline(v = 1.50, col = GREY, lty = 3)
legend("topright", c("NOL in force 1.33%", "Statutory floor 1.20%", "Statutory cap 1.50%"), col = c(GOLD, RED, GREY), lty = c(2, 1, 3), lwd = c(1.5, 1.2, 1), bty = "n", cex = 0.7)
dev.off()

list.files("figures")
