# 2_nol_calc.R -------------------------------------------------------------------

## 1. ER paths side by side (wide) ---------------------------------------------------
er_wide <- reshape(panel[, c("vintage","period","equity_ratio")],
                   idvar = "vintage", timevar = "period", direction = "wide")
names(er_wide) <- c("vintage", "base", paste0("p", 0:10))
er_wide

## 2. NOL table ----------------------------------------------------------------------
nol_tbl <- do.call(rbind, lapply(split(panel, panel$vintage), function(d) {
  d   <- d[order(d$period), ]
  er  <- d$equity_ratio
  tro <- which.min(er[2:12]) + 1                    # index of the trough among periods 0-10 (index 1 = June base)
  pk  <- which.max(er[2:12]) + 1
  data.frame(vintage    = d$vintage[1],
             nol_in_force = d$nol_input[1],          # NOL input cell on the sheet
             june_er    = er[1],
             ye_er      = er[2],                     # period 0 = Dec of the model year
             er_dec5    = er[12],                    # period 10 = Dec five years out
             trough_er  = er[tro], trough_half = d$half[tro], trough_when = format(as.Date(d$period_end[tro]), "%b-%y"),
             peak_er    = er[pk],  peak_half   = d$half[pk],  peak_when   = format(as.Date(d$period_end[pk]), "%b-%y"),
             decline_a  = er[2] - er[12],
             decline_b  = er[2] - min(er[3:12]),
             stringsAsFactors = FALSE)
}))
nol_tbl$nol_a <- pmax(1.20, 1.20 + nol_tbl$decline_a)   # the sheet's convention (Q53 = 1.2% + P53)
nol_tbl$nol_b <- pmax(1.20, 1.20 + nol_tbl$decline_b)   # peak(year-end)-to-trough
rownames(nol_tbl) <- NULL
print(nol_tbl, digits = 4)

## 3. the series you asked for -------------------------------------------------------
nol_series <- nol_tbl[, c("vintage","ye_er","er_dec5","trough_er","trough_when","decline_a","nol_a","nol_b")]
nol_series$nol_a_rounded <- round(nol_series$nol_a, 2)
nol_series$below_125     <- nol_series$nol_a < 1.25
nol_series


## 4. note on 2020: 
with(nol_tbl[nol_tbl$vintage == 2020, ], 1.20 + peak_er - trough_er)

write.csv(nol_tbl, "nol_by_vintage.csv", row.names = FALSE)
