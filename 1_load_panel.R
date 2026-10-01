# 1_load_panel.R ------------------------------------------------------------
## 0. setup --------------------------------------------------------------------
# setwd("C:/path/to/NOL")                          # <- adjust
panel <- read.csv("nol_panel.csv", stringsAsFactors = FALSE)

## 1. look ---------------------------------------------------------------------
dim(panel)
str(panel)
table(panel$vintage)
panel[panel$vintage == 2026,
      c("half","share_growth","ins_losses","portfolio_yield","opex","net_income",
        "distribution","equity_ratio")]

## 2. derived columns ------------------------------------------------------------
panel$g       <- panel$share_growth / 100                          # semiannual share growth
panel$cd_comp <- 100 * panel$cap_deposit / panel$insured_shares    # 1%-deposit part of ER (pp)
panel$eq_comp <- 100 * panel$equity      / panel$insured_shares    # retained-earnings part (pp)
panel$er_calc <- panel$cd_comp + panel$eq_comp                      # should equal equity_ratio

lag_v <- function(x, id) ave(x, id, FUN = function(z) c(NA, head(z, -1)))
panel$equity_l <- lag_v(panel$equity,         panel$vintage)
panel$shares_l <- lag_v(panel$insured_shares, panel$vintage)
panel$cd_l     <- lag_v(panel$cap_deposit,    panel$vintage)
panel$portf_l  <- lag_v(panel$portfolio,      panel$vintage)
panel$er_l     <- 100 * (panel$cd_l + panel$equity_l) / panel$shares_l   # exact prior-period ER

## 3. the accounting rules, as identities (max abs deviation per vintage) --------------
proj <- panel[panel$period >= 0, ]
chk <- data.frame(
  vintage  = proj$vintage,
  ni       = with(proj, interest_rev - opex - ins_losses - net_income),      # NI = interest - opex - losses
  opex     = with(proj, op_budget * otr / 100 - opex),                       # opex = (annual budget/2) x OTR
  int_rev  = with(proj, portf_l * portfolio_yield / 200 / interest_rev - 1), # interest = prior portfolio x yield / 2 (relative; yield shown at 4 dp)
  cap_dep  = with(proj, 0.01 * shares_l - cap_deposit),                      # CD_t = 1% x shares_{t-1}  (one-period lag)
  cd_adj   = with(proj, cap_deposit - cd_l - cd_adj),                        # CD adjustment = change in CD
  portf    = with(proj, portf_l + net_income - portfolio),                   # portfolio moves by NI only (CD true-ups not invested)
  equity   = with(proj, equity_l + net_income + g_fee + ngn_adj - distribution - equity),
  shares   = with(proj, shares_l * (1 + g) / insured_shares - 1),            # relative; growth rates shown at 2 dp
  er       = with(proj, 100 * (cap_deposit + equity) / insured_shares - equity_ratio),
  distrib  = with(proj, pmax(0, er_l - nol_input) / 100 * shares_l - distribution)   # distribution_t = max(0, ER_{t-1} - NOL) x shares_{t-1}
)
format(aggregate(. ~ vintage, data = chk, FUN = function(z) max(abs(z))), digits = 2, scientific = TRUE)
# expect: dollar identities exact to $1; int_rev and shares ~1e-5 relative (rounded inputs); the model is fully reproduced

## 4. the ER identity: ER = 1/(1+g) + Equity/Shares ---------------------------------
proj <- panel[panel$period >= 0, ]
summary(1 / (1 + proj$g) - proj$cd_comp)           # ~0: deposit component (pp) is 1/(1+g)
range(proj$eq_comp)                                 # retained-earnings component, 0.19-0.37 pp
