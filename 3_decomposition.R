# 3_decomposition.R -------------------------------------------------------------

## 1. period-level contributions (pp of insured shares) ------------------------------
proj <- panel[panel$period >= 0, ]
proj$eq_comp_l <- 100 * proj$equity_l / proj$shares_l
proj$c_earn    <- 100 * proj$net_income / proj$insured_shares
proj$c_dilut   <- -proj$eq_comp_l * proj$g / (1 + proj$g)
proj$c_other   <- 100 * (proj$g_fee + proj$ngn_adj - proj$distribution) / proj$insured_shares
proj$c_cdlag   <- 1 / (1 + proj$g) - 100 * proj$cd_l / proj$shares_l     # change in the 1/(1+g) deposit term (pp)
proj$d_er      <- proj$equity_ratio - proj$er_l
proj$resid     <- proj$d_er - (proj$c_earn + proj$c_dilut + proj$c_other + proj$c_cdlag)
summary(proj$resid)      # rounding only (ER shown at 3 dp)

## 2. five-year window Dec(vintage) -> Dec(vintage+5): periods 1..10 ------------------------
win <- proj[proj$period >= 1 & proj$period <= 10, ]
decomp <- aggregate(cbind(c_earn, c_dilut, c_cdlag, c_other, d_er) ~ vintage, data = win, FUN = sum)
decomp[, -1] <- round(100 * decomp[, -1], 1)          # basis points over 5 years
names(decomp) <- c("vintage","earnings_bp","dilution_bp","cd_lag_bp","other_bp","total_bp")
decomp


## 3. annualized rates: NI/S vs e*g, and break-even NI --------------------------------
ann <- do.call(rbind, lapply(split(win, win$vintage), function(d) {
  yrs <- nrow(d) / 2
  data.frame(vintage        = d$vintage[1],
             ni_per_yr_M    = sum(d$net_income) / yrs / 1e6,
             ni_over_S_bp   = 100 * sum(d$c_earn)  / yrs,             # bp per year
             dilution_bp    = 100 * sum(d$c_dilut) / yrs,
             net_drift_bp   = 100 * sum(d$c_earn + d$c_dilut) / yrs,
             share_g_ann    = 100 * (prod(1 + d$g)^(1/yrs) - 1),        # annualized share growth, %
             eq_comp_avg    = mean(d$eq_comp_l),
             breakeven_ni_M = 2 * mean(d$eq_comp_l / 100 * d$g / (1 + d$g) * d$insured_shares) / 1e6)
}))
rownames(ann) <- NULL
print(ann, digits = 3)


## 4. driver summary by vintage (horizon = 11 projected halves) ----------------------------
drv <- do.call(rbind, lapply(split(proj, proj$vintage), function(d) {
  d <- d[order(d$period), ]
  data.frame(vintage       = d$vintage[1],
             g_first       = d$share_growth[1],
             g_avg_half    = mean(d$share_growth),
             g_avg_ann     = 100 * ((1 + mean(d$g))^2 - 1),
             loss_M_yr     = sum(d$ins_losses) / (nrow(d) / 2) / 1e6,
             loss_peak_M   = max(d$ins_losses) / 1e6,
             yld_base      = panel$portfolio_yield[panel$vintage == d$vintage[1] & panel$period == -1],
             yld_avg       = mean(d$portfolio_yield),
             yld_end       = d$portfolio_yield[nrow(d)],
             int_M_half    = mean(d$interest_rev) / 1e6,
             opex_first_M  = d$opex[1] / 1e6,
             opex_last_M   = d$opex[nrow(d)] / 1e6,
             opex_M_half   = mean(d$opex) / 1e6,
             budget_g      = if (any(d$budget_growth < 0)) min(d$budget_growth) else max(d$budget_growth),
             ni_cum_M      = sum(d$net_income) / 1e6,
             ni_M_half     = mean(d$net_income) / 1e6)
}))
rownames(drv) <- NULL
print(drv, digits = 3)

## 5. what moved net income between vintices (horizon averages, $M per half) ----------------
attr_ni <- function(v0, v1) {
  a <- drv[drv$vintage == v0, ]; b <- drv[drv$vintage == v1, ]
  P0 <- mean(proj$portf_l[proj$vintage == v0]); P1 <- mean(proj$portf_l[proj$vintage == v1])
  y0 <- a$yld_avg / 100; y1 <- b$yld_avg / 100
  d_int  <- b$int_M_half - a$int_M_half
  d_size <- (P1 - P0) * mean(c(y0, y1)) / 2 / 1e6          # bigger portfolio at average yield
  d_yld  <- mean(c(P0, P1)) * (y1 - y0) / 2 / 1e6          # higher yield on average portfolio
  out <- c(d_interest = d_int, of_which_size = d_size, of_which_yield = d_yld,
           d_opex = -(b$opex_M_half - a$opex_M_half), d_losses = -(b$loss_M_yr - a$loss_M_yr) / 2,
           d_NI = b$ni_M_half - a$ni_M_half)
  round(out, 1)
}
attr_ni(2024, 2026)          
attr_ni(2024, 2025)
attr_ni(2025, 2026)
attr_ni(2022, 2024)


## 6. rule-of-thumb sensitivities at 2026 scale -----------------------------------------------
S26 <- panel$insured_shares[panel$vintage == 2026 & panel$period == 0]
e26 <- panel$eq_comp[panel$vintage == 2026 & panel$period == 0]
c(bp_per_yr_per_100M_NI = 100 * 100e6 / S26,                # +100M/yr of NI  -> bp/yr on ER
  bp_per_yr_per_1pp_share_growth = e26 * 1,                 # +1 pp/yr share growth -> -e bp/yr (dilution)
  bp_per_1pp_growth_deposit_lag = 100 * (1/1.01 - 1))       # one-off: 1 pp higher semiannual g -> deposit term
