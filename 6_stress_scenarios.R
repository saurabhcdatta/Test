# 6_stress_scenarios.R ---------------------------------------------------------------

## 1. starting point and the model's own paths (2026 vintage, periods 2..10) ------------------
v26   <- panel[panel$vintage == 2026, ]; v26 <- v26[order(v26$period), ]
start <- v26[v26$period == 0, ]
fwd   <- v26[v26$period >= 1, ]
P0 <- start$portfolio; CD0 <- start$cap_deposit; E0 <- start$equity; S0 <- start$insured_shares
NOL_IN_FORCE <- start$nol_input / 100
mdl <- list(g = fwd$g, y = fwd$portfolio_yield / 100, opex = fwd$opex, L = fwd$ins_losses)
n   <- nrow(fwd)                                        # 10 half-years, Jun-2027 .. Dec-2031

## 2. simulator ----------------------------------------------------------------------------------
run_path <- function(g, y, opex, L, extra = rep(0, n)) {
  P <- P0; CD <- CD0; E <- E0; S <- S0; er_prev <- (CD0 + E0) / S0; er <- numeric(n)
  for (t in seq_len(n)) {
    S_new  <- S * (1 + g[t]); CD_new <- 0.01 * S
    NI     <- P * y[t] / 2 - opex[t] - (L[t] + extra[t])
    dist   <- max(0, er_prev - NOL_IN_FORCE) * S
    E      <- E + NI - dist; P <- P + NI
    er[t]  <- (CD_new + E) / S_new; er_prev <- er[t]
    S <- S_new; CD <- CD_new
  }
  er
}
summarise_path <- function(name, er) {
  start_er <- (CD0 + E0) / S0; tr <- min(er)
  data.frame(scenario = name, start_er = 100 * start_er, trough_er = 100 * tr,
             trough_when = format(as.Date(fwd$period_end[which.min(er)]), "%b-%y"), decline_bp = 1e4 * (start_er - tr),
             nol = max(1.20, 100 * (0.012 + start_er - tr)), stringsAsFactors = FALSE)
}

## 3. scenario inputs ------------------------------------------------------------------------------
surge  <- c(0.095, 0.095, 0.05, 0.05, mdl$g[5:n])       # ~20%/yr, then ~10%/yr, then the model's path
flat_y <- rep(0.0325, n)                                # yield frozen: rate cuts offset reinvestment gains
loss_bp <- function(bp, halves) c(rep(S0 * bp / 1e4 / 2, halves), rep(0, n - halves))   # extra losses, bp of shares per year
big3 <- c(3e9, rep(0, n - 1))                           # one $3B failure in 2027 H1

## 4. run and collect ---------------------------------------------------------------------------------
stress <- rbind(
  summarise_path("Current adverse scenario, as modeled",                   run_path(mdl$g, mdl$y, mdl$opex, mdl$L)),
  summarise_path("2020-style share surge (~20% yr 1, 10% yr 2), nothing else", run_path(surge, mdl$y, mdl$opex, mdl$L)),
  summarise_path("Share surge plus rate cuts (yield frozen at 3.25%)",       run_path(surge, flat_y, mdl$opex, mdl$L)),
  summarise_path("Surge and rate cuts plus a two-year loss wave of 6 bp/yr", run_path(surge, flat_y, mdl$opex, mdl$L, loss_bp(6, 4))),
  summarise_path("Current scenario plus one $3 billion failure",             run_path(mdl$g, mdl$y, mdl$opex, mdl$L, big3)),
  summarise_path("Surge and rate cuts plus one $3 billion failure",          run_path(surge, flat_y, mdl$opex, mdl$L, big3)),
  summarise_path("Surge and rate cuts plus a two-year loss wave of 12 bp/yr", run_path(surge, flat_y, mdl$opex, mdl$L, loss_bp(12, 4)))
)
rownames(stress) <- NULL
print(stress, digits = 4)
# check: first row reproduces the model (trough 1.283 at 2027H2, +3.7 bp over four years, floor)

## 5. scale of the model's own loss assumption, for the narrative ---------------------------------------
c(model_losses_M_per_yr = sum(mdl$L) / (n / 2) / 1e6,
  model_losses_bp_of_shares = 1e4 * sum(mdl$L) / (n / 2) / S0,
  six_bp_per_yr_in_dollars_B = S0 * 6 / 1e4 / 1e9)

write.csv(stress, "stress_scenarios.csv", row.names = FALSE)
