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
text(vints, nol_tbl$nol_a + ifelse(above, 0.013, -0.016), sprintf("%.2f%%", nol_tbl$nol_a), col = NAVY, cex = 0.78, font = 2)
legend("topright", legend = c("NOL set by the Board", "NOL implied by the formula"), col = c(GOLD, NAVY), lwd = c(2.6, 3), pch = 19, bty = "n", cex = 0.8, text.col = DGREY)
mtext("NOL by model year (June data)", side = 3, line = 0.6, adj = 0, cex = 0.95, col = DGREY)
dev.off()

## c3: net income vs the break-even --------------------------------------------------------------
ni <- ann$ni_per_yr_M; be <- ann$breakeven_ni_M
open_png("c3_ni.png", 5.6, 3.6); base_par(c(3.0, 3.8, 2.2, 0.6))
yl <- c(-300, 760)
plot(NA, xlim = c(2019.5, 2026.5), ylim = yl, axes = FALSE, xlab = "", ylab = "")
hgrid(seq(-200, 600, 200)); axis(1, at = vints, lwd = 0); axis(2, at = seq(-200, 600, 200), labels = format(seq(-200, 600, 200), big.mark = ","), las = 1, lwd = 0)
rect(vints - 0.3, pmin(ni, 0), vints + 0.3, pmax(ni, 0), col = ifelse(ni < 0, RED, TEAL), border = NA); abline(h = 0, col = "#C9CDD3")
lines(vints, be, col = NAVY, lwd = 3, type = "b", pch = 18, cex = 1.4)
text(vints, ni + ifelse(ni >= 0, 28, -30), sprintf("%+.0f", ni), cex = 0.72, col = DGREY)
legend("topleft", legend = c("Projected net income", "Needed to hold the ratio steady"), fill = c(TEAL, NA), border = NA, col = c(NA, NAVY), lwd = c(NA, 3), pch = c(NA, 18), bty = "n", cex = 0.8, text.col = DGREY)
mtext("$ millions per year, five-year projection window", side = 3, line = 0.6, adj = 0, cex = 0.95, col = DGREY)
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
axis(1, at = vints, labels = sprintf("%d\n%.2f%%", vints, nol_tbl$nol_a), lwd = 0, padj = 0.5, cex.axis = 0.78)
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
bar3 <- function(f, labels, vals, col, fmt, ttl, ymax) {
  open_png(f, 4.3, 1.85); base_par(c(2.6, 0.6, 2.0, 0.4)); par(yaxs = "i")
  bp <- barplot(vals, col = col, border = NA, ylim = c(0, ymax), axes = FALSE, names.arg = NA, width = 0.55, space = 0.8)
  axis(1, at = bp, labels = labels, lwd = 0, padj = 0.4, cex.axis = 0.78); abline(h = 0, col = "#C9CDD3")
  text(bp, vals + ymax * 0.03, sprintf(fmt, vals), adj = c(0.5, 0), cex = 0.85, font = 2, col = DGREY)
  mtext(ttl, side = 3, line = 0.5, adj = 0, cex = 0.85, col = DGREY); dev.off() }
bar3("c5_losses.png", c("Assumed\n(2026 model)", "2009 actual", "2010 actual"), c(loss_bp_26, hist_losses), GOLD, "%.1f",
     "Not a stress: insurance losses, bp of insured shares per year", 12.5)
bar3("c5_growth.png", c("Assumed\n(2026 model)", "2009 actual", "2020 actual"), c(g_26, hist_growth), RED, "%.1f%%",
     "Insured-share growth, % per year", 24)

## c5 combined: both "assumed vs history" panels in one image (used by the R Markdown deck) ---------------------
open_png("c5_assumed.png", 5.5, 3.9); layout(matrix(1:2, 2)); 
for (k in 1:2) {
  base_par(c(2.6, 0.6, 2.0, 0.4)); par(yaxs = "i")
  vals <- if (k == 1) c(loss_bp_26, hist_losses) else c(g_26, hist_growth); ymax <- if (k == 1) 12.5 else 24; col <- if (k == 1) GOLD else RED
  labels <- if (k == 1) c("Assumed\n(2026 model)", "2009 actual", "2010 actual") else c("Assumed\n(2026 model)", "2009 actual", "2020 actual")
  bp <- barplot(vals, col = col, border = NA, ylim = c(0, ymax), axes = FALSE, names.arg = NA, width = 0.55, space = 0.8)
  axis(1, at = bp, labels = labels, lwd = 0, padj = 0.4, cex.axis = 0.78); abline(h = 0, col = "#C9CDD3")
  text(bp, vals + ymax * 0.03, sprintf(if (k == 1) "%.1f" else "%.1f%%", vals), adj = c(0.5, 0), cex = 0.85, font = 2, col = DGREY)
  mtext(if (k == 1) "Not a stress: insurance losses, bp of insured shares per year" else "Insured-share growth, % per year", side = 3, line = 0.5, adj = 0, cex = 0.85, col = DGREY)
}
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
cat("deck charts written to deck_images/:", paste(list.files("deck_images"), collapse = ", "), "\n")
