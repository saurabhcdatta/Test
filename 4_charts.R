# 4_charts.R ----------------------------------------------------------------------
# Base-R charts. Requires 1_load_panel.R, 2_nol_calc.R, 3_decomposition.R.
# Each block draws to the screen; uncomment png()/dev.off() pairs to save.

vints <- sort(unique(panel$vintage))
cols  <- colorRampPalette(c("grey70", "steelblue4", "firebrick"))(length(vints))
names(cols) <- vints
panel$date <- as.Date(panel$period_end)

## Fig 1: projected ER paths by vintage, calendar axis --------------------------------------
# png("fig1_er_paths.png", 1400, 800, res = 130)
plot(NA, xlim = range(panel$date), ylim = c(1.15, 1.35), xlab = "", ylab = "Equity ratio (%)",
     main = "OCE Adverse projections: equity ratio by model vintage", xaxt = "n")
axis.Date(1, at = seq(as.Date("2020-06-30"), as.Date("2031-06-30"), by = "year"), format = "%Y")
abline(h = c(1.20, 1.25, 1.33), lty = c(2, 3, 2), col = c("firebrick", "grey40", "darkgreen"))
for (v in vints) {
  d <- panel[panel$vintage == v, ]
  lines(d$date, d$equity_ratio, col = cols[as.character(v)], lwd = 2)
  points(d$date[1], d$equity_ratio[1], pch = 19, col = cols[as.character(v)])
}
act <- panel[panel$period == -1, ]
lines(act$date, act$equity_ratio, lwd = 3, col = "black")
legend("bottomleft", legend = c(paste("vintage", vints), "June actuals"),
       col = c(cols, "black"), lwd = c(rep(2, length(vints)), 3), bty = "n", ncol = 2, cex = 0.8)
text(as.Date("2020-07-01"), 1.336, "NOL in force 1.33", col = "darkgreen", cex = 0.8, adj = 0)
# dev.off()

## Fig 2: NOL by vintage ----------------------------------------------------------------------
# png("fig2_nol_series.png", 1100, 700, res = 130)
plot(nol_tbl$vintage, nol_tbl$nol_a, type = "b", pch = 19, lwd = 2, ylim = c(1.18, 1.40),
     xlab = "Model vintage (June data)", ylab = "NOL (%)", main = "Computed NOL = 1.20% + projected ER decline")
lines(nol_tbl$vintage, nol_tbl$nol_b, type = "b", pch = 1, lty = 2, lwd = 2, col = "steelblue4")
lines(nol_tbl$vintage, nol_tbl$nol_in_force, type = "s", col = "darkgreen", lwd = 2)
abline(h = c(1.20, 1.25), lty = 3, col = c("firebrick", "grey40"))
legend("topright", c("year-end to Dec+4 (your formula)", "year-end to trough", "NOL in force (sheet input)"),
       col = c("black", "steelblue4", "darkgreen"), lty = c(1, 2, 1), pch = c(19, 1, NA), lwd = 2, bty = "n", cex = 0.85)
text(nol_tbl$vintage, nol_tbl$nol_a + 0.012, sprintf("%.2f", nol_tbl$nol_a), cex = 0.8)
# dev.off()

## Fig 3: the three drivers + net income ------------------------------------------------------
# png("fig3_drivers.png", 1400, 900, res = 130)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
barplot(drv$g_avg_ann, names.arg = drv$vintage, col = "grey60", ylab = "% per year",
        main = "Insured share growth (adverse path, annualized avg)")
barplot(rbind(drv$loss_M_yr, drv$loss_peak_M), beside = TRUE, names.arg = drv$vintage,
        col = c("grey60", "firebrick"), ylab = "$ millions", main = "Insurance losses: avg per year / peak half")
legend("topright", c("avg $/yr", "peak $/half"), fill = c("grey60", "firebrick"), bty = "n")
plot(drv$vintage, drv$yld_avg, type = "b", pch = 19, lwd = 2, ylim = c(1, 4), xlab = "vintage",
     ylab = "%", main = "Portfolio yield: June base vs horizon average")
lines(drv$vintage, drv$yld_base, type = "b", pch = 1, lty = 2, col = "steelblue4", lwd = 2)
legend("topleft", c("horizon average", "June base"), col = c("black", "steelblue4"), lty = 1:2, pch = c(19, 1), bty = "n")
barplot(drv$ni_cum_M / 1000, names.arg = drv$vintage, col = ifelse(drv$ni_cum_M < 0, "firebrick", "darkgreen"),
        ylab = "$ billions", main = "Cumulative 5-yr projected net income")
abline(h = 0)
par(op)
# dev.off()

## Fig 4: decomposition of the 4-year ER change ------------------------------------------------
# png("fig4_decomposition.png", 1200, 750, res = 130)
m <- t(as.matrix(decomp[, c("earnings_bp", "dilution_bp", "cd_lag_bp", "other_bp")]))
colnames(m) <- decomp$vintage
op <- par(mar = c(4, 4, 3, 1))
bp <- barplot(m, beside = TRUE, col = c("darkgreen", "firebrick", "steelblue4", "grey60"),
              ylab = "basis points over 4 years", main = "Dec(vintage) -> Dec+4 change in ER, by source",
              ylim = range(c(m, decomp$total_bp)) * 1.15)
abline(h = 0)
points(colMeans(bp), decomp$total_bp, pch = 18, cex = 1.8)
legend("topleft", c("earnings (NI / shares)", "dilution (-e x g)", "deposit-lag term", "other (fees, NGN, distributions)", "net change"),
       fill = c("darkgreen", "firebrick", "steelblue4", "grey60", NA), border = c(rep("black", 4), NA),
       pch = c(NA, NA, NA, NA, 18), bty = "n", cex = 0.85)
par(op)
# dev.off()

## Fig 5: NI vs break-even -------------------------------------------------------------------------
# png("fig5_breakeven.png", 1100, 650, res = 130)
plot(ann$vintage, ann$ni_per_yr_M, type = "b", pch = 19, lwd = 2, ylim = c(-250, 750),
     xlab = "vintage", ylab = "$ millions per year", main = "Projected net income vs break-even (holds ER flat)")
lines(ann$vintage, ann$breakeven_ni_M, type = "b", pch = 1, lty = 2, lwd = 2, col = "firebrick")
abline(h = 0, col = "grey60")
legend("topleft", c("avg NI, Dec->Dec+4", "break-even = e x g x shares"), col = c("black", "firebrick"),
       lty = 1:2, pch = c(19, 1), lwd = 2, bty = "n")
# dev.off()
