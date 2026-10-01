# 7_report_tables.R -----------------------------------------------------------------

fmt_pct <- function(x, d = 2) sprintf(paste0("%.", d, "f%%"), x)
fmt_M   <- function(x, sign = FALSE) { s <- ifelse(x < 0, "\u2212", ifelse(sign & x > 0, "+", "")); paste0(s, "$", formatC(abs(x), format = "f", digits = 0, big.mark = ","), "M") }
fmt_bp  <- function(x) paste0(ifelse(x < 0, "\u2212", "+"), abs(round(x)), " bp")

## 1. per-vintage window statistics ---------------------------------------------------
win <- panel[panel$period >= 1 & panel$period <= 10, ]
wstat <- do.call(rbind, lapply(split(win, win$vintage), function(d) {
  d <- d[order(d$period), ]
  data.frame(vintage      = d$vintage[1],
             yield_avg    = mean(d$portfolio_yield),
             losses_M_yr  = sum(d$ins_losses) / 5 / 1e6,
             opex_M_yr    = sum(d$opex) / 5 / 1e6,
             interest_M_yr= sum(d$interest_rev) / 5 / 1e6,
             share_g_ann  = 100 * (prod(1 + d$g)^(1 / 5) - 1),
             ni_M_yr      = sum(d$net_income) / 5 / 1e6,
             portf_avg    = mean(d$portf_l))
}))
rownames(wstat) <- NULL

# budget growth assumption: the growth rate applied in the vintage (negative if a cut is assumed)
bg <- sapply(split(panel[panel$period >= 0, ], panel$vintage[panel$period >= 0]),
             function(d) if (any(d$budget_growth < 0)) min(d$budget_growth) else max(d$budget_growth))
budget_note <- sprintf("%+.1f%%/yr", bg)                       # default text ...
names(budget_note) <- names(bg)
budget_note["2025"] <- "\u221218.3%/yr (input carried forward)"  
budget_note["2026"] <- "+7.3%/yr (base 27% lower)"       

## 2. Table 1: the projection behind each year's NOL ----------------------------------------
tab1 <- data.frame(
  `Model year    `                          = nol_tbl$vintage,      # trailing spaces widen the column in Word (pandoc sizes pipe-table columns by header width)
  `NOL from formula`                        = ifelse(nol_tbl$nol_a <= 1.20, "1.20% (floor)", fmt_pct(nol_tbl$nol_a)),
  `NOL set by Board`                        = paste0(fmt_pct(nol_tbl$nol_in_force), ifelse(nol_tbl$vintage <= 2021, "\u00b9", "")),
  `5-yr change in ratio`                    = fmt_bp(decomp$total_bp),
  `Investment yield   `                     = fmt_pct(wstat$yield_avg, 2),
  `Budget growth assumed`                   = budget_note[as.character(nol_tbl$vintage)],
  `Insurance losses assumed`                = paste0(fmt_M(wstat$losses_M_yr), "/yr"),
  `Share growth assumed`                    = paste0(sprintf("%.1f", wstat$share_g_ann), "%/yr"),
  `Projected net income`                    = paste0(fmt_M(wstat$ni_M_yr, sign = TRUE), "/yr"),
  `Net income to hold ratio flat`           = paste0(fmt_M(ann$breakeven_ni_M), "/yr"),
  check.names = FALSE)
tab1

## 3. Table 2: what changed between two vintices (window averages) -----------------------------
attr_window <- function(v0, v1) {
  a <- wstat[wstat$vintage == v0, ]; b <- wstat[wstat$vintage == v1, ]
  y0 <- a$yield_avg / 100; y1 <- b$yield_avg / 100
  d_size <- (b$portf_avg - a$portf_avg) * mean(c(y0, y1)) / 1e6          # per year
  d_yld  <- mean(c(a$portf_avg, b$portf_avg)) * (y1 - y0) / 1e6
  d_opex <- -(b$opex_M_yr - a$opex_M_yr); d_loss <- -(b$losses_M_yr - a$losses_M_yr)
  out <- data.frame(driver = c(sprintf("Higher investment yield (%.1f%% \u2192 %.1f%%) as maturing bonds reprice", a$yield_avg, b$yield_avg),
                               sprintf("Larger investment portfolio (about %.0f%% bigger)", 100 * (b$portf_avg / a$portf_avg - 1)),
                               "Lower NCUA operating cost (2026 budget cut)", "Lower assumed insurance losses"),
                    change_M_yr = c(d_yld, d_size, d_opex, d_loss))
  out$share_pct <- 100 * out$change_M_yr / sum(out$change_M_yr)
  attr(out, "ni_from") <- a$ni_M_yr; attr(out, "ni_to") <- b$ni_M_yr; attr(out, "total") <- sum(out$change_M_yr)
  out
}
attr26 <- attr_window(2024, 2026)
tab2 <- rbind(
  data.frame(Driver = attr26$driver, `Change in projected net income` = paste0(fmt_M(attr26$change_M_yr, sign = TRUE), "/yr"),
             `Share of the swing` = sprintf("%.0f%%", attr26$share_pct), check.names = FALSE),
  data.frame(Driver = sprintf("Total: %s/yr in the 2024 projection \u2192 %s/yr in the 2026 projection", fmt_M(attr(attr26, "ni_from")), fmt_M(attr(attr26, "ni_to"))),
             `Change in projected net income` = paste0(fmt_M(attr(attr26, "total"), sign = TRUE), "/yr"), `Share of the swing` = "100%", check.names = FALSE))
tab2

## 4. Table 3: what builds the add-on, and how much investment income offsets ---------------------------
tab3 <- data.frame(
  `Model year    `                         = offset$vintage,
  `Share growth`                           = sprintf("%.0f%%", shareB[, "dilution"]),
  `Operating cost`                         = sprintf("%.0f%%", shareB[, "opex"]),
  `Losses`                                 = sprintf("%.0f%%", shareB[, "losses"]),
  `Deposit timing`                         = paste0(sprintf("%.0f%%", pmax(shareB[, "lag"], 0)), ifelse(bp$lag > 0, "\u00b9", "")),
  `Downward pressure (bp, 4 yrs)`          = sprintf("%.1f", offset$drag_bp),
  `Offset by investment income`            = paste0(sprintf("%.0f%%", offset$coverage_pct), ifelse(bp$lag > 0, "\u00b9", "")),
  `Resulting add-on`                       = ifelse(offset$addon_bp <= 0, "none \u2192 1.20% (floor)",
                                                    sprintf("%.0f bp \u2192 %.2f%%", offset$addon_bp, offset$nol)),
  check.names = FALSE)
names(tab3)[8] <- "Resulting add-on \u2192 NOL"      # (\u escapes are not allowed inside backticks)
tab3

## 5. Table 6: stress scenarios --------------------------------------------------------------------
tab6 <- data.frame(
  Scenario                 = stress$scenario,
  `Lowest projected ratio` = fmt_pct(stress$trough_er),
  `Decline from start`     = paste0(round(stress$decline_bp), " bp"),
  `Implied NOL`            = c(sprintf("%.2f%% to trough; 1.20%% (floor) under the formula's Dec-to-Dec convention", stress$nol[1]),
                               fmt_pct(stress$nol[-1])),
  check.names = FALSE)
tab6

## 6. Appendix tables ----------------------------------------------------------------------------
tabA1 <- data.frame(
  `Model year    ` = nol_tbl$vintage, `June actual` = sprintf("%.3f", nol_tbl$june_er), `Year-end (projected)` = sprintf("%.3f", nol_tbl$ye_er),
  `Five years later` = sprintf("%.3f", nol_tbl$er_dec5), `Lowest point (when)    ` = sprintf("%.3f (%s)", nol_tbl$trough_er, nol_tbl$trough_when),
  `Decline, year-end to 5 yrs later` = sprintf("%.3f", nol_tbl$decline_a),
  `NOL, formula` = ifelse(nol_tbl$nol_a <= 1.20, "1.20 (floor)", sprintf("%.2f", nol_tbl$nol_a)),
  `NOL, to lowest point` = sprintf("%.2f", nol_tbl$nol_b), `NOL set by Board` = sprintf("%.2f", nol_tbl$nol_in_force), check.names = FALSE)
tabA2 <- er_wide; names(tabA2) <- c("Model year    ", "Jun Y", "Dec Y", "Jun Y+1", "Dec Y+1", "Jun Y+2", "Dec Y+2", "Jun Y+3", "Dec Y+3", "Jun Y+4", "Dec Y+4", "Jun Y+5", "Dec Y+5")
tabA2[, -1] <- lapply(tabA2[, -1], function(z) sprintf("%.3f", z))

## 7. key numbers for the narrative ------------------------------------------------------------------
g26 <- function(col) wstat[wstat$vintage == 2026, col]
kn <- list(
  nol_first  = fmt_pct(min(nol_tbl$nol_a[nol_tbl$vintage <= 2024])), nol_last24 = fmt_pct(max(nol_tbl$nol_a[nol_tbl$vintage <= 2024])),
  nol_2025   = fmt_pct(nol_tbl$nol_a[nol_tbl$vintage == 2025]),
  ni_2024    = fmt_M(wstat$ni_M_yr[wstat$vintage == 2024]), ni_2026 = fmt_M(g26("ni_M_yr")),
  be_2026    = fmt_M(ann$breakeven_ni_M[ann$vintage == 2026]),
  eq_comp_26 = sprintf("%.2f%%", panel$eq_comp[panel$vintage == 2026 & panel$period == 0]),
  equity_26B = sprintf("$%.1f billion", panel$equity[panel$vintage == 2026 & panel$period == 0] / 1e9),
  loss_bp_26 = sprintf("%.2f", 1e4 * g26("losses_M_yr") * 1e6 / panel$insured_shares[panel$vintage == 2026 & panel$period == 0]),
  loss_M_26  = fmt_M(g26("losses_M_yr")),
  offset_26  = sprintf("%.0f%%", offset$coverage_pct[offset$vintage == 2026]),
  offset_2123= sprintf("%.0f\u2013%.0f%%", min(offset$coverage_pct[offset$vintage %in% 2021:2023]), max(offset$coverage_pct[offset$vintage %in% 2021:2023])),
  shareA_int_2020 = sprintf("%.0f%%", shareA["2020", "interest"]), shareA_int_2026 = sprintf("%.0f%%", shareA["2026", "interest"]),
  dil_share_26 = sprintf("%.0f%%", shareB["2026", "dilution"]),
  drag_range = sprintf("%.0f to %.0f", min(offset$drag_bp[offset$vintage != 2022]), max(offset$drag_bp[offset$vintage != 2022])),
  yield_share = sprintf("%.0f%%", attr26$share_pct[1]), budget_share = sprintf("%.0f%%", attr26$share_pct[3]),
  stress_range = sprintf("%.2f%% to %.2f%%", min(stress$nol[-1]), max(stress$nol[-1])),
  er_change_26 = fmt_bp(decomp$total_bp[decomp$vintage == 2026])
)
str(kn)

for (nm in c("tab1", "tab2", "tab3", "tab6", "tabA1", "tabA2")) write.csv(get(nm), paste0("tables/", nm, ".csv"), row.names = FALSE)
