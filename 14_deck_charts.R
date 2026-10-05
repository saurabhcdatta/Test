# 14_deck_charts.R ------------------------------------------------------------------------
# The five chart images used in the executive deck (NOL_Executive_Deck_images.pptx), drawn in
# base R from the pipeline's own objects, so they can be regenerated here when the data change.
# Written to deck_images/. To refresh the deck without Node.js: open the .pptx, right-click each
# chart image > Change Picture > pick the new file of the same name.
# Requires scripts 1, 2, 3, 5, 6, 7 and 11 to have been run (run_all.R does this).

dir.create("deck_images", showWarnings = FALSE)
NAVY <- "#1F3A5F"; TEAL <- "#2A9D8F"; GOLD <- "#D9A441"; RED <- "#B23A48"; DGREY <- "#3C4048"; GREY <- "#6B7280"; LGRID <- "#E6E8EC"
open_png <- function(f, w, h) png(file.path("deck_images", f), width = w, height = h, units = "in", res = 220)
base_par <- function(mar) par(mar = mar, mgp = c(2, 0.5, 0), tcl = 0, family = "sans", col.axis = DGREY, cex.axis = 0.85, lend = 1)
hgrid <- function(at) abline(h = at, col = LGRID, lwd = 1)
vints <- nol_tbl$vintage
board <- nol_tbl$nol_in_force

## c2: formula NOL vs the Board's level ---------------------------------------------------------------
open_png("c2_nol.png", 5.7, 3.55); base_par(c(3.2, 3.6, 2.2, 0.8))
plot(NA, xlim = c(2019.7, 2026.3), ylim = c(1.15, 1.42), axes = FALSE, xlab = "", ylab = "")
hgrid(seq(1.15, 1.40, 0.05)); axis(1, at = vints, lwd = 0); axis(2, at = seq(1.15, 1.40, 0.05), labels = sprintf("%.2f%%", seq(1.15, 1.40, 0.05)), las = 1, lwd = 0)
abline(h = 1.20, col = RED, lty = 2); text(2020.05, 1.196, "statutory floor", col = RED, cex = 0.75, adj = c(0, 1))
lines(vints, board, col = GOLD, lwd = 2.6, type = "b", pch = 19, cex = 0.9)
lines(vints, nol_tbl$nol_a, col = NAVY, lwd = 3, type = "b", pch = 19, cex = 1.05)
above <- nol_tbl$nol_a <= 1.27 | vints == 2020
text(vints, nol_tbl$nol_a + ifelse(above, 0.013, -0.016), sprintf("%.3f%%", nol_tbl$nol_a), col = NAVY, cex = 0.74, font = 2)
legend("topright", legend = c("NOL set by the Board", "NOL implied by the formula"), col = c(GOLD, NAVY), lwd = c(2.6, 3), pch = 19, bty = "n", cex = 0.8, text.col = DGREY)
mtext("NOL by model year (June data)", side = 3, line = 0.6, adj = 0, cex = 0.95, col = DGREY)
dev.off()

## c3: net income vs the break-even, and what it does to the ratio (two panels) --------------------------------
ni <- ann$ni_per_yr_M; be <- ann$breakeven_ni_M; d_er <- decomp$total_bp              # five-year change in the ratio, bp
open_png("c3_ni.png", 5.6, 4.1); layout(matrix(1:2, 2), heights = c(1.15, 0.95))
base_par(c(0.5, 3.8, 2.0, 0.6)); par(xaxs = "r")
yl <- c(-300, 760)
plot(NA, xlim = c(2019.5, 2026.5), ylim = yl, axes = FALSE, xlab = "", ylab = "")
hgrid(seq(-200, 600, 200)); axis(2, at = seq(-200, 600, 200), labels = format(seq(-200, 600, 200), big.mark = ","), las = 1, lwd = 0)
rect(vints - 0.3, pmin(ni, 0), vints + 0.3, pmax(ni, 0), col = ifelse(ni < 0, RED, TEAL), border = NA); abline(h = 0, col = "#C9CDD3")
lines(vints, be, col = NAVY, lwd = 3, type = "b", pch = 18, cex = 1.4)
text(vints, ni + ifelse(ni >= 0, 28, -30), sprintf("%+.0f", ni), cex = 0.7, col = DGREY)
legend("topleft", legend = c("Projected net income", "Needed to hold the ratio steady"), fill = c(TEAL, NA), border = NA, col = c(NA, NAVY), lwd = c(NA, 3), pch = c(NA, 18), bty = "n", cex = 0.78, text.col = DGREY)
mtext("Net income vs. the amount needed to keep pace with share growth, $M a year", side = 3, line = 0.5, adj = 0, cex = 0.82, col = DGREY)
base_par(c(2.4, 3.8, 1.5, 0.6))
yl2 <- c(min(-18, floor(min(d_er)) - 4), max(8, ceiling(max(d_er)) + 4))
plot(NA, xlim = c(2019.5, 2026.5), ylim = yl2, axes = FALSE, xlab = "", ylab = "")
hgrid(pretty(yl2)); axis(2, at = pretty(yl2), labels = sprintf("%+d bp", as.integer(pretty(yl2))), las = 1, lwd = 0, cex.axis = 0.78)
rect(vints - 0.3, pmin(d_er, 0), vints + 0.3, pmax(d_er, 0), col = ifelse(d_er < 0, RED, TEAL), border = NA); abline(h = 0, col = "#C9CDD3")
neg <- d_er < 0
text(vints[neg], d_er[neg] - 1.0, sprintf("%+.0f", d_er[neg]), cex = 0.7, font = 2, col = RED, adj = c(0.5, 1))
text(vints[!neg], d_er[!neg] + 1.0, sprintf("%+.0f", d_er[!neg]), cex = 0.7, font = 2, col = TEAL, adj = c(0.5, 0))
axis(1, at = vints, lwd = 0, cex.axis = 0.85)
text(2022, 4.2, "ratio falls \u2192 the formula adds a cushion", col = RED, cex = 0.66, adj = c(0.5, 0))
text(2025.5, -5, "ratio rises \u2192 the formula\nadds nothing", col = TEAL, cex = 0.66, adj = c(0.5, 1))
mtext("What that does to the equity ratio: five-year change, basis points", side = 3, line = 0.3, adj = 0, cex = 0.82, col = DGREY)
dev.off()

## c4: three-driver shares, stacked area -----------------------------------------------------------
sy <- shareA3[, "Investment yield"]; sl <- shareA3[, "Insurance losses"]; sg <- shareA3[, "Share growth"]
y1 <- sy; y2 <- y1 + sl; y3 <- y2 + sg
open_png("c4_drivers.png", 5.9, 3.6); base_par(c(3.6, 3.4, 2.2, 0.4)); par(xaxs = "i", yaxs = "i")
plot(NA, xlim = c(2020, 2027.25), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
polygon(c(vints, rev(vints)), c(rep(0, 7), rev(y1)), col = TEAL, border = NA)
polygon(c(vints, rev(vints)), c(y1, rev(y2)), col = GOLD, border = NA)
polygon(c(vints, rev(vints)), c(y2, rev(y3)), col = adjustcolor(RED, 0.92), border = NA)
lines(vints, y1, col = "white", lwd = 1.5); lines(vints, y2, col = "white", lwd = 1.5); abline(h = c(25, 50, 75), col = adjustcolor("white", 0.5), lwd = 0.7)
axis(1, at = vints, labels = sprintf("%d\n%.3f%%", vints, nol_tbl$nol_a), lwd = 0, padj = 0.5, cex.axis = 0.74)
axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"), las = 1, lwd = 0)
lab3 <- function(y, txt, col, cex) { for (i in 1:7) text(vints[i] + c(0.06, rep(0, 5), -0.06)[i], y[i], txt[i], col = col[i], font = 2, cex = cex[i], adj = c(c(0, rep(0.5, 5), 1)[i], 0.5)) }
lab3(y1 / 2, sprintf("%.0f%%", sy), rep("white", 7), rep(0.85, 7))
lab3(y2 + sg / 2, sprintf("%.0f%%", sg), rep("white", 7), rep(0.85, 7))
lab3(y1 + sl / 2, sprintf("%.0f%%", sl), ifelse(sl >= 8, "white", DGREY), ifelse(sl >= 8, 0.78, 0.65))
text(2026.08, tail(y1, 1) / 2, "Investment\nyield", col = TEAL, font = 2, cex = 0.8, adj = 0)
text(2026.08, tail(y1, 1) + tail(sl, 1) / 2, "Insurance\nlosses", col = GOLD, font = 2, cex = 0.8, adj = 0)
text(2026.08, tail(y2, 1) + tail(sg, 1) / 2, "Share\ngrowth", col = RED, font = 2, cex = 0.8, adj = 0)
mtext("Share of the three drivers' combined effect on the projected ratio (year, formula NOL)", side = 3, line = 0.6, adj = 0, cex = 0.72, col = DGREY)
dev.off()

## c5: assumed vs history ----------------------------------------------------------------------------
S0_26 <- panel$insured_shares[panel$vintage == 2026 & panel$period == 0]
loss_bp_26 <- 1e4 * wstat$losses_M_yr[wstat$vintage == 2026] * 1e6 / S0_26
g_26 <- wstat$share_g_ann[wstat$vintage == 2026]
hist_losses <- c(9.3, 10.1)      # 2009-10 insurance loss expense, bp of insured shares (NCUA Board bulletins)
hist_growth <- c(10.5, 20.0)     # organic insured-share growth 2009 and 2020, % per year
bar3 <- function(f, labels, vals, col, fmt, ttl, ymax, note) {
  open_png(f, 4.3, 1.85); base_par(c(2.6, 0.6, 2.0, 0.4)); par(yaxs = "i")
  bp <- barplot(vals, col = c(adjustcolor(col, 0.55), col, col), border = NA, ylim = c(0, ymax), axes = FALSE, names.arg = NA, width = 0.55, space = 0.8)
  axis(1, at = bp, labels = labels, lwd = 0, padj = 0.4, cex.axis = 0.78); abline(h = 0, col = "#C9CDD3")
  text(bp, vals + ymax * 0.03, sprintf(fmt, vals), adj = c(0.5, 0), cex = 0.85, font = 2, col = DGREY)
  text(bp[1], vals[1] + ymax * 0.22, note, col = DGREY, cex = 0.7, font = 3, adj = c(0.5, 0))
  mtext(ttl, side = 3, line = 0.5, adj = 0, cex = 0.85, col = DGREY); dev.off() }
mult_L <- max(hist_losses) / loss_bp_26; mult_g <- max(hist_growth) / g_26
bar3("c5_losses.png", c("Assumed in the\nadverse scenario", "2009 actual", "2010 actual"), c(loss_bp_26, hist_losses), GOLD, "%.1f",
     "Insurance losses, basis points of insured shares per year", 12.5, sprintf("%.0fx smaller than\nthe 2009-10 actuals", mult_L))
bar3("c5_growth.png", c("Assumed in the\nadverse scenario", "2009 actual", "2020 actual"), c(g_26, hist_growth), RED, "%.1f%%",
     "Insured-share growth, % per year", 24, sprintf("%.1fx smaller than\nthe 2020 surge", mult_g))

## c5 combined: both "assumed vs history" panels in one image (used by the R Markdown deck) ---------------------
open_png("c5_assumed.png", 5.5, 4.1); layout(matrix(1:2, 2)); par(oma = c(0, 0, 1.6, 0))
for (k in 1:2) {
  base_par(c(2.6, 0.6, 1.6, 0.4)); par(yaxs = "i")
  vals <- if (k == 1) c(loss_bp_26, hist_losses) else c(g_26, hist_growth); ymax <- if (k == 1) 12.5 else 24; col <- if (k == 1) GOLD else RED
  labels <- if (k == 1) c("Assumed in the\nadverse scenario", "2009 actual", "2010 actual") else c("Assumed in the\nadverse scenario", "2009 actual", "2020 actual")
  bp <- barplot(vals, col = c(adjustcolor(col, 0.55), col, col), border = NA, ylim = c(0, ymax), axes = FALSE, names.arg = NA, width = 0.55, space = 0.8)
  axis(1, at = bp, labels = labels, lwd = 0, padj = 0.4, cex.axis = 0.76); abline(h = 0, col = "#C9CDD3")
  text(bp, vals + ymax * 0.03, sprintf(if (k == 1) "%.1f" else "%.1f%%", vals), adj = c(0.5, 0), cex = 0.85, font = 2, col = DGREY)
  text(bp[1], vals[1] + ymax * 0.2, if (k == 1) sprintf("%.0fx smaller than\nthe 2009-10 actuals", mult_L) else sprintf("%.1fx smaller than\nthe 2020 surge", mult_g), col = DGREY, cex = 0.7, font = 3, adj = c(0.5, 0))
  mtext(if (k == 1) "Insurance losses, basis points of insured shares per year" else "Insured-share growth, % per year", side = 3, line = 0.3, adj = 0, cex = 0.8, col = DGREY)
}
mtext("What the adverse scenario assumes vs. what the Fund actually experienced", side = 3, line = 0.2, adj = 0.02, cex = 0.9, col = DGREY, outer = TRUE, font = 2)
dev.off()

## c6: NOL implied by the stress scenarios (trough basis; current scenario on the formula) --------------
pick <- function(pattern) stress$nol[grepl(pattern, stress$scenario)][1]
scen <- c("Current adverse scenario", "2020-style share surge", "Surge + two-year loss wave (6 bp/yr)", "One $3 billion failure",
          "2008-type episode replayed (approx.)", "Surge + one $3 billion failure", "Surge + loss wave (12 bp/yr)")
vals <- c(nol_tbl$nol_a[vints == 2026], pick("^2020-style"), pick("6 bp/yr"), pick("^Current scenario plus one"),
          if (exists("rep2008")) rep2008$nol_to_trough[grepl("all three combined", rep2008$case)] else NA, pick("Surge and rate cuts plus one"), pick("12 bp/yr"))
open_png("c6_stress.png", 5.4, 3.65); base_par(c(2.6, 12.0, 0.8, 1.4)); par(oma = c(0, 0, 1.4, 0))
ypos <- rev(seq_along(scen))
plot(NA, xlim = c(1.15, 1.52), ylim = c(0.3, 7.9), axes = FALSE, xlab = "", ylab = "")
abline(v = c(1.20, 1.30, 1.40, 1.50), col = LGRID)
rect(1.15, ypos - 0.31, vals, ypos + 0.31, col = ifelse(seq_along(scen) == 1, GREY, NAVY), border = NA)
text(vals + 0.006, ypos, sprintf("%.2f%%", vals), adj = c(0, 0.5), cex = 0.8, font = 2, col = NAVY)
abline(v = 1.33, col = GOLD, lty = 2, lwd = 1.6); text(1.335, 0.45, "1.33% in force", col = GOLD, cex = 0.72, adj = c(0, 0.5))
axis(1, at = c(1.20, 1.30, 1.40, 1.50), labels = sprintf("%.2f%%", c(1.20, 1.30, 1.40, 1.50)), lwd = 0, cex.axis = 0.8)
axis(2, at = ypos, labels = scen, las = 1, lwd = 0, cex.axis = 0.74)
mtext("NOL implied by stress scenarios run through the same model (Dec-2026 start)", side = 3, line = 0.2, adj = 0.02, cex = 0.85, col = DGREY, outer = TRUE)
dev.off()
## c7: the six changes as a waterfall for the 2026 model year (needs `ladder` from 12_tweaks.R) -----------------
if (exists("ladder")) {
  L26 <- ladder[ladder$vintage == 2026, ]
  lev <- c(L26$s0_current, L26$s1_trough, L26$s2_trued_base, L26$s3_yield_frozen, L26$s4_rec_shares, L26$s5_hist_losses, L26$s6_concentration)
  steps <- c("Formula\ntoday", "1 Worst\npoint", "2 Capital\nheld", "3 Gains when\nearned", "4 Deposit\ninflows", "5 Loss\nwave", "6 One large\nfailure", "All six\nelements")
  open_png("c7_ladder.png", 5.6, 4.15); base_par(c(7.0, 3.6, 2.2, 0.6)); par(xaxs = "i", yaxs = "i")
  plot(NA, xlim = c(0.4, 8.6), ylim = c(1.15, 1.37), axes = FALSE, xlab = "", ylab = "")
  hgrid(seq(1.15, 1.35, 0.05)); axis(2, at = seq(1.15, 1.35, 0.05), labels = sprintf("%.2f%%", seq(1.15, 1.35, 0.05)), las = 1, lwd = 0)
  abline(h = 1.20, col = RED, lty = 2); abline(h = 1.33, col = GOLD, lty = 2, lwd = 1.5)
  text(4.5, 1.197, "statutory floor", col = RED, cex = 0.66, adj = c(0.5, 1)); text(0.45, 1.332, "level in force, 1.33%", col = GOLD, cex = 0.66, adj = c(0, 0))
  rect(1 - 0.34, 1.15, 1 + 0.34, lev[1], col = GREY, border = NA)
  MID <- "#6C8EBF"
  for (i in 2:7) { lo <- lev[i - 1]; hi <- lev[i]; rect(i - 0.34, lo, i + 0.34, hi, col = if (i <= 4) NAVY else MID, border = NA)
    segments(i - 1 + 0.34, lo, i - 0.34, lo, col = "#9AA3AF", lty = 3)
    text(i, hi + 0.004, sprintf("%+.1f bp", 100 * (hi - lo)), cex = 0.68, col = NAVY, font = 2, adj = c(0.5, 0)) }
  rect(8 - 0.34, 1.15, 8 + 0.34, lev[7], col = TEAL, border = NA)
  text(1, lev[1] + 0.004, sprintf("%.2f%%", lev[1]), cex = 0.78, font = 2, col = DGREY, adj = c(0.5, 0))
  text(8, lev[7] + 0.004, sprintf("%.2f%%", lev[7]), cex = 0.78, font = 2, col = TEAL, adj = c(0.5, 0))
  for (i in 1:8) mtext(steps[i], side = 1, at = i, line = 1.9, cex = 0.62, col = DGREY)
  mtext("NOL", side = 1, at = 0.3, line = 3.15, cex = 0.62, col = GREY, adj = 1, font = 3)
  for (i in 1:8) mtext(sprintf("%.2f%%", lev[c(1:7, 7)][i]), side = 1, at = i, line = 3.15, cex = 0.68, font = 2, col = c(GREY, rep(NAVY, 3), rep(MID, 3), TEAL)[i])
  mtext(sprintf("Seven model years: %.2f\u2013%.2f%% (a %.0f bp range) vs %.3f\u2013%.3f%% (%.0f bp) today",
                min(ladder$s6_concentration), max(ladder$s6_concentration), 100 * diff(range(ladder$s6_concentration)), min(ladder$s0_current), max(ladder$s0_current), 100 * diff(range(ladder$s0_current))),
        side = 1, line = 4.6, adj = 0, at = 0.4, cex = 0.7, font = 2, col = NAVY)
  mtext(sprintf("The Fund's own history replayed through the same model: %.2f\u2013%.2f%%", min(stress$nol[-1]), max(stress$nol[-1])),
        side = 1, line = 5.5, adj = 0, at = 0.4, cex = 0.7, col = DGREY)
  legend(1.6, 1.372, legend = c("measurement: getting the arithmetic right", "risk elements: what a downturn actually does"), fill = c(NAVY, MID), border = NA, bty = "n", cex = 0.66, text.col = DGREY)
  mtext("2026 model year: the formula today, then each element recognised in turn", side = 3, line = 0.6, adj = 0, cex = 0.85, col = DGREY)
  dev.off()
}
cat("deck charts written to deck_images/:", paste(list.files("deck_images"), collapse = ", "), "\n")
