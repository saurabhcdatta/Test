# 5_factor_shares.R ----------------------------------------------------------------

## 1. period contributions in bp -------------------------------------------------------------
proj <- panel[panel$period >= 0, ]
proj$eq_comp_l <- 100 * proj$equity_l / proj$shares_l
proj$cd_comp_l <- 100 * proj$cd_l     / proj$shares_l
proj$f_interest <- 100 * 100 * proj$interest_rev / proj$insured_shares      # bp
proj$f_opex     <- -100 * 100 * proj$opex        / proj$insured_shares
proj$f_losses   <- -100 * 100 * proj$ins_losses  / proj$insured_shares
proj$f_dilution <- -100 * proj$eq_comp_l * proj$g / (1 + proj$g)
proj$f_lag      <-  100 * (1 / (1 + proj$g) - proj$cd_comp_l)
proj$f_other    <- 100 * 100 * (proj$g_fee + proj$ngn_adj - proj$distribution) / proj$insured_shares
proj$d_er_bp    <- 100 * (proj$equity_ratio - proj$er_l)
fac <- c("f_interest", "f_opex", "f_losses", "f_dilution", "f_lag", "f_other")

## 2. five-year window totals ------------------------------------------------------------------
win <- proj[proj$period >= 1 & proj$period <= 10, ]
bp  <- aggregate(win[, c(fac, "d_er_bp")], by = list(vintage = win$vintage), FUN = sum)
bp$net_check <- rowSums(bp[, fac])                    # equals d_er_bp up to rounding of the 3-dp ratios
names(bp) <- sub("^f_", "", names(bp))
print(round(bp, 1))


## 3. scaling A: share of gross absolute movement (adds to 100) -------------------------------------
m  <- as.matrix(bp[, c("interest", "opex", "losses", "dilution", "lag", "other")])
shareA <- round(100 * abs(m) / rowSums(abs(m)), 1)
rownames(shareA) <- bp$vintage
shareA


## 4. scaling B: composition of NOL-building factors (=100) and offset coverage ------------------------
neg <- pmin(m, 0); pos <- pmax(m, 0)
shareB <- round(100 * (-neg) / rowSums(-neg), 1)
rownames(shareB) <- bp$vintage
offset <- data.frame(vintage = bp$vintage,
                     drag_bp   = round(rowSums(-neg), 1),         # what builds the add-on
                     offset_bp = round(rowSums(pos), 1),          # what works against it (interest income; 2020 deposit timing)
                     coverage_pct = round(100 * rowSums(pos) / rowSums(-neg)),
                     addon_bp  = round(rowSums(-neg) - rowSums(pos), 1),
                     nol       = round(pmax(1.20, 1.20 + (rowSums(-neg) - rowSums(pos)) / 100), 2))
shareB
offset


## 5. change in relative importance, 2024 -> 2026 ---------------------------------------------------------
data.frame(factor = colnames(shareA),
           shareA_2024 = shareA["2024", ], shareA_2026 = shareA["2026", ], changeA = shareA["2026", ] - shareA["2024", ],
           shareB_2024 = shareB["2024", ], shareB_2026 = shareB["2026", ], changeB = shareB["2026", ] - shareB["2024", ])

write.csv(cbind(bp, shareA = shareA, shareB = shareB, offset[, -1]), "factor_shares_by_vintage.csv", row.names = FALSE)

## 6. charts ----------------------------------------------------------------------------------------------
cols5 <- c(interest = "#2A9D8F", opex = "#8A8F98", losses = "#D9A441", dilution = "#B23A48", lag = "#1F3A5F")
# png("fig6_factor_shares.png", 1300, 650, res = 130)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
barplot(t(shareA[, names(cols5)]), col = cols5, border = NA, ylab = "% of gross movement in the ratio",
        main = "A. Share of gross absolute movement", legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", cex = 0.75, inset = c(-0.02, -0.15), horiz = TRUE))
barplot(t(shareB[, c("opex", "losses", "dilution", "lag")]), col = cols5[c("opex", "losses", "dilution", "lag")], border = NA,
        ylab = "% of factors building the add-on", main = "B. Composition of the drag; label = offset coverage",
        ylim = c(0, 115))
text(seq(0.7, by = 1.2, length.out = nrow(shareB)), 106, paste0(offset$coverage_pct, "%"), cex = 0.8, font = 2)
par(op)
# dev.off()

## 7. executive view: the three named drivers only, rescaled to 100 --------------------------------
# (investment income, insurance losses, share-growth dilution; opex and deposit timing set aside)
m3 <- abs(as.matrix(bp[, c("interest", "losses", "dilution")]))
shareA3 <- round(100 * m3 / rowSums(m3), 1); rownames(shareA3) <- bp$vintage
colnames(shareA3) <- c("Investment yield", "Insurance losses", "Share growth")
shareA3
# 2020: 38 / 19 / 44  ->  2026: 66 / 5 / 29
write.csv(cbind(vintage = bp$vintage, shareA3), "tables/three_driver_shares.csv", row.names = FALSE)

# Fig 10: stacked area, base R
TEAL <- "#2A9D8F"; GOLD <- "#D9A441"; RED <- "#B23A48"; DGREY <- "#3C4048"; GREY <- "#8A8F98"
png("figures/fig10_three_factor.png", width = 6.5, height = 4.0, units = "in", res = 220)
par(mar = c(5.2, 3.6, 3.2, 6.5), mgp = c(2.2, 0.5, 0), tcl = 0, family = "sans", xaxs = "i", yaxs = "i", col.axis = DGREY, cex.axis = 0.8)
x <- bp$vintage; y1 <- shareA3[, 1]; y2 <- y1 + shareA3[, 2]; y3 <- y2 + shareA3[, 3]
plot(NA, xlim = range(x), ylim = c(0, 100), xlab = "", ylab = "", axes = FALSE)
polygon(c(x, rev(x)), c(rep(0, length(x)), rev(y1)), col = TEAL, border = NA)
polygon(c(x, rev(x)), c(y1, rev(y2)), col = GOLD, border = NA)
polygon(c(x, rev(x)), c(y2, rev(y3)), col = adjustcolor(RED, 0.92), border = NA)
lines(x, y1, col = "white", lwd = 1.5); lines(x, y2, col = "white", lwd = 1.5)
abline(h = c(25, 50, 75), col = adjustcolor("white", 0.5), lwd = 0.7)
axis(1, at = x, lwd = 0, cex.axis = 0.9); axis(2, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"), las = 1, lwd = 0)
xl <- x; xl[1] <- x[1] + 0.05; xl[length(x)] <- x[length(x)] - 0.05; adj <- rep(0.5, length(x)); adj[1] <- 0; adj[length(x)] <- 1
text(xl, y1 / 2, sprintf("%.0f%%", shareA3[, 1]), col = "white", font = 2, cex = 0.8, adj = adj)
text(xl, y2 + shareA3[, 3] / 2, sprintf("%.0f%%", shareA3[, 3]), col = "white", font = 2, cex = 0.8, adj = adj)
text(xl, y1 + shareA3[, 2] / 2, sprintf("%.0f%%", shareA3[, 2]), col = ifelse(shareA3[, 2] >= 8, "white", DGREY), font = 2, cex = ifelse(shareA3[, 2] >= 8, 0.75, 0.62), adj = adj)
mtext(c("Investment\nyield", "Insurance\nlosses", "Share\ngrowth"), side = 4, at = c(tail(y1, 1) / 2, tail(y1, 1) + tail(shareA3[, 2], 1) / 2, tail(y2, 1) + tail(shareA3[, 3], 1) / 2),
      las = 1, line = 0.4, col = c(TEAL, GOLD, RED), font = 2, cex = 0.8)
mtext(sprintf("%.2f%%", nol_tbl$nol_a), side = 1, at = x, line = 2.6, cex = 0.7, col = DGREY)
mtext("Formula NOL", side = 1, at = x[1] - 0.45, line = 2.6, cex = 0.7, col = GREY, font = 3, adj = 1)
mtext("Model year (June data)", side = 1, line = 4.0, cex = 0.8, col = DGREY)
mtext("Investment yield now dominates the equity-ratio arithmetic", side = 3, line = 1.7, adj = 0, cex = 1.05, font = 2, col = DGREY)
mtext("Share of the combined effect of the three drivers on the projected four-year change in the equity ratio (sums to 100%)", side = 3, line = 0.5, adj = 0, cex = 0.62, col = GREY)
dev.off()
