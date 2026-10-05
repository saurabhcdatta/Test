# 15_build_deck_base_r.R ------------------------------------------------------------------
# The executive deck, drawn entirely in base R: every slide (title bar, callouts, coloured dots,
# cards, badges, the dark title and closing slides, speaker notes) is written here as PowerPoint
# XML, filled with the pipeline's numbers and the chart images from 14_deck_charts.R, and zipped
# into NOL_Executive_Deck_R.pptx. deck_skeleton.zip holds only the theme, master, layout and
# notes master. No packages, no rmarkdown, no zip program: memCompress() supplies the deflate
# stream and a small CRC-32 routine the checksum.
# Requires scripts 1-3, 5-7, 9-12 and 14 to have been run (run_all.R does this).
# To change wording, edit the slide definitions in block 3; positions are inches on a 10 x 5.625 slide.

## 1. numbers from the pipeline --------------------------------------------------------------------
pc <- function(x, d = 2) sprintf(paste0("%.", d, "f%%"), x)
L26 <- ladder[ladder$vintage == 2026, ]; el20 <- elas[elas$vintage == 2020, ]; el26 <- elas[elas$vintage == 2026, ]
thr13 <- thr[thr$`Target NOL` == "1.33%", ]
lev <- c(L26$s0_current, L26$s1_trough, L26$s2_trued_base, L26$s3_yield_frozen, L26$s4_rec_shares, L26$s5_hist_losses, L26$s6_concentration)
step_bp <- sprintf("%+.1f bp", 100 * diff(lev)); step_lv <- pc(lev[-1])
V <- list(
  nol22 = pc(nol_tbl$nol_a[nol_tbl$vintage == 2022]), nol26 = pc(nol_tbl$nol_a[nol_tbl$vintage == 2026]),
  board26 = pc(nol_tbl$nol_in_force[nol_tbl$vintage == 2026]),
  be26 = kn$be_2026, lossM26 = kn$loss_M_26, ni26 = kn$ni_2026, lossbp26 = sprintf("%.2f", as.numeric(kn$loss_bp_26)),
  g26 = sprintf("%.1f%%", wstat$share_g_ann[wstat$vintage == 2026]),
  ylo = sprintf("%.1f%%", min(wstat$yield_avg[wstat$vintage %in% 2020:2022])), y26 = sprintf("%.1f%%", wstat$yield_avg[wstat$vintage == 2026]),
  cushion = sprintf("%.0f\u2013%.0f", min(-decomp$total_bp[decomp$vintage %in% 2021:2023]), max(-decomp$total_bp[decomp$vintage %in% 2021:2023])),
  nol2123 = sprintf("%.2f\u2013%.2f%%", min(nol_tbl$nol_a[nol_tbl$vintage %in% 2021:2023]), max(nol_tbl$nol_a[nol_tbl$vintage %in% 2021:2023])),
  ysh26 = sprintf("%.0f%%", shareA3["2026", "Investment yield"]), ysh20 = sprintf("%.0f%%", shareA3["2020", "Investment yield"]),
  lsh26 = sprintf("%.0f%%", shareA3["2026", "Insurance losses"]), dead26 = kn$dead_2026,
  levL26 = sprintf("$%sM", thr13[["Losses per year 2027-31 ($M)"]]), levG26 = paste0(thr13[["Avg share growth 2027-31 (%/yr)"]], "%"),
  lag22 = sprintf("%.0f%%", shareB["2022", "lag"]),
  ladder_rng = sprintf("%.2f\u2013%.2f%%", min(ladder$s6_concentration), max(ladder$s6_concentration)),
  formula_rng = sprintf("%.2f\u2013%.2f%%", min(ladder$s0_current), max(ladder$s0_current)),
  ladder26 = pc(L26$s6_concentration), yswing = kn$yield_share,
  elg20 = sprintf("%.1f", el20$ER_share_g_1pp), elg26 = sprintf("%.1f", el26$ER_share_g_1pp),
  ely20 = sprintf("%+.1f", el20$ER_yield_100bp), ely26 = sprintf("%+.1f", el26$ER_yield_100bp),
  stress_rng = sprintf("%.2f\u2013%.2f%%", min(stress$nol[-1]), max(stress$nol[-1])),
  r2008 = pc(rep2008$nol_to_trough[grepl("all three combined", rep2008$case)]),
  dec26 = pc(nol_tbl$ye_er[nol_tbl$vintage == 2026]), dec31 = pc(nol_tbl$er_dec5[nol_tbl$vintage == 2026]),
  month = format(Sys.Date(), "%B %Y"))

## 2. PowerPoint XML primitives ---------------------------------------------------------------------
NAVY <- "1F3A5F"; TEAL <- "2A9D8F"; GOLD <- "D9A441"; RED <- "B23A48"; DGREY <- "3C4048"; GREY <- "6B7280"
LGREY <- "F3F5F8"; ICE <- "E8EEF4"; WHITE <- "FFFFFF"; PALE <- "CADCFC"; MID <- "6C8EBF"
emu <- function(x) format(round(x * 914400), scientific = FALSE, trim = TRUE)
esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE) }
.id <- 1L; nid <- function() { .id <<- .id + 1L; .id }
r <- function(text, size = 11, bold = FALSE, italic = FALSE, color = DGREY, spc = NULL)   # one text run
  list(text = text, size = size, bold = bold, italic = italic, color = color, spc = spc)
run_xml <- function(x) sprintf('<a:r><a:rPr lang="en-US" sz="%d"%s%s%s dirty="0"><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:latin typeface="Calibri"/><a:cs typeface="Calibri"/></a:rPr><a:t>%s</a:t></a:r>',
  round(x$size * 100), if (x$bold) ' b="1"' else "", if (x$italic) ' i="1"' else "", if (!is.null(x$spc)) sprintf(' spc="%d"', x$spc) else "", x$color, esc(x$text))
para <- function(runs, align = "l") sprintf('<a:p><a:pPr algn="%s" indent="0" marL="0"><a:buNone/></a:pPr>%s</a:p>', align, paste(vapply(runs, run_xml, ""), collapse = ""))
txbox <- function(x, y, w, h, paras, anchor = "t") sprintf(
  '<p:sp><p:nvSpPr><p:cNvPr id="%d" name="Text %d"/><p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm><a:off x="%s" y="%s"/><a:ext cx="%s" cy="%s"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:noFill/></p:spPr><p:txBody><a:bodyPr wrap="square" lIns="0" tIns="0" rIns="0" bIns="0" rtlCol="0" anchor="%s"/><a:lstStyle/>%s</p:txBody></p:sp>',
  nid(), .id, emu(x), emu(y), emu(w), emu(h), anchor, paste(paras, collapse = ""))
tx <- function(x, y, w, h, text, size = 11, bold = FALSE, italic = FALSE, color = DGREY, align = "l", anchor = "t", spc = NULL)
  txbox(x, y, w, h, para(list(r(text, size, bold, italic, color, spc)), align), anchor)
rich <- function(x, y, w, h, runs, align = "l", anchor = "t") txbox(x, y, w, h, para(runs, align), anchor)
shape <- function(x, y, w, h, fill, prst = "rect", adj = NULL) sprintf(
  '<p:sp><p:nvSpPr><p:cNvPr id="%d" name="Shape %d"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm><a:off x="%s" y="%s"/><a:ext cx="%s" cy="%s"/></a:xfrm><a:prstGeom prst="%s"><a:avLst>%s</a:avLst></a:prstGeom><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:ln><a:noFill/></a:ln></p:spPr></p:sp>',
  nid(), .id, emu(x), emu(y), emu(w), emu(h), prst, if (is.null(adj)) "" else sprintf('<a:gd name="adj" fmla="val %d"/>', adj), fill)
rbox <- function(x, y, w, h, fill, radius = 0.08) shape(x, y, w, h, fill, "roundRect", round(50000 * radius / min(w, h)))
oval  <- function(x, y, d, fill) shape(x, y, d, d, fill, "ellipse")
hline <- function(x, y, w, color, pt = 0.75) sprintf(
  '<p:cxnSp><p:nvCxnSpPr><p:cNvPr id="%d" name="Line %d"/><p:cNvCxnSpPr/><p:nvPr/></p:nvCxnSpPr><p:spPr><a:xfrm><a:off x="%s" y="%s"/><a:ext cx="%s" cy="0"/></a:xfrm><a:prstGeom prst="line"><a:avLst/></a:prstGeom><a:ln w="%d"><a:solidFill><a:srgbClr val="%s"/></a:solidFill></a:ln></p:spPr></p:cxnSp>',
  nid(), .id, emu(x), emu(y), emu(w), round(pt * 12700), color)
png_dims <- function(f) { h <- readBin(f, "raw", 24); c(w = sum(as.integer(h[17:20]) * 256^(3:0)), h = sum(as.integer(h[21:24]) * 256^(3:0))) }
pic <- function(x, y, w, h, file, rid) {                     # fit the image inside the box, keeping its aspect
  d <- png_dims(file); s <- min(w / d["w"], h / d["h"]); pw <- d["w"] * s; ph <- d["h"] * s
  sprintf('<p:pic><p:nvPicPr><p:cNvPr id="%d" name="Picture %d" descr="%s"/><p:cNvPicPr><a:picLocks noChangeAspect="1"/></p:cNvPicPr><p:nvPr/></p:nvPicPr><p:blipFill><a:blip r:embed="rId%d"/><a:stretch><a:fillRect/></a:stretch></p:blipFill><p:spPr><a:xfrm><a:off x="%s" y="%s"/><a:ext cx="%s" cy="%s"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr></p:pic>',
    nid(), .id, esc(basename(file)), rid, emu(x + (w - pw) / 2), emu(y + (h - ph) / 2), emu(pw), emu(ph)) }
# slide-level helpers
title_bar <- function(text, sub = NULL) c(tx(0.5, 0.3, 9.0, if (is.null(sub)) 0.95 else 0.76, text, if (is.null(sub)) 24 else 22, TRUE, color = NAVY),
  if (!is.null(sub)) tx(0.5, 1.08, 9.0, 0.3, sub, 12, italic = TRUE, color = GREY))
footer <- function(n, source = "Source: OCE Adverse \"Standard Projection Equity Ratio Model\", 2020\u20132026 vintages; OCE analysis.")
  c(tx(0.5, 5.18, 8.2, 0.3, source, 8.5, color = GREY), tx(9.1, 5.18, 0.4, 0.3, as.character(n), 9, color = GREY, align = "r"))
stat <- function(x, y, w, big, label, color) c(tx(x, y, w, 0.62, big, 34, TRUE, color = color), tx(x, y + 0.62, w, 0.5, label, 11.5))
dot <- function(x, y, color, d = 0.26) oval(x, y, d, color)

## 3. the slides -----------------------------------------------------------------------------------------------
# each: list(bg = NULL|colour, shapes = c(...), images = c(file, ...) in rId order, notes = "...")
slides <- list()
slides[[1]] <- list(bg = NAVY, images = NULL, shapes = c(
  tx(0.7, 1.25, 8.6, 1.5, "Why the Share Insurance Fund's Normal Operating Level fell to the floor", 34, TRUE, color = WHITE),
  tx(0.7, 2.75, 8.6, 0.5, "\u2014 and what the number is no longer measuring", 20, color = PALE),
  unlist(lapply(1:3, function(i) c(dot(0.72, 3.62 + (i - 1) * 0.4, c(RED, GOLD, TEAL)[i], 0.22),
    tx(1.05, 3.56 + (i - 1) * 0.4, 3, 0.34, c("Share growth", "Insurance losses", "Investment yield")[i], 13, color = WHITE)))),
  tx(0.7, 4.85, 8.6, 0.35, paste0("Office of the Chief Economist  \u00b7  Executive briefing  \u00b7  ", V$month), 11, color = PALE)),
  notes = "The NOL is set each year as 1.20% plus how far the Fund's equity ratio is projected to fall over five years under an adverse scenario. This deck explains, in terms of the three drivers - share growth, losses and investment yield - why that number has fallen to the statutory floor, why the method looked fine until 2024, what it leaves out, and what a more secure version looks like.")

slides[[2]] <- list(bg = NULL, images = "deck_images/c2_nol.png", shapes = c(
  title_bar(sprintf("The formula's NOL has fallen to the %s floor while the Board has held %s", "1.20%", V$board26),
            "NOL = 1.20% + the projected five-year fall in the Fund's equity ratio under an adverse scenario"),
  pic(0.5, 1.45, 5.7, 3.55, "deck_images/c2_nol.png", 1),
  stat(6.55, 1.45, 3.0, V$nol22, "2022: the formula matched what the Board set", NAVY),
  stat(6.55, 2.65, 3.0, V$nol26, "2025 and 2026: on the statutory floor \u2014 the projected ratio no longer falls", RED),
  stat(6.55, 3.85, 3.0, V$board26, "The level the Board has kept in force since 2022", GOLD),
  footer(2)),
  notes = sprintf("The formula produced %s in 2020, %s in 2021-22, then slid: %s, %s, and the floor in both 2025 and 2026. The Board has held %s throughout. The gap is the subject of this deck.",
    pc(nol_tbl$nol_a[1]), V$nol2123, pc(nol_tbl$nol_a[4]), pc(nol_tbl$nol_a[5]), V$board26))

rows3 <- list(
  list(RED,  "Share growth \u2014 steady pressure", sprintf("Shares grow 5\u20138%% a year in every projection; keeping pace takes about %s a year of retained earnings \u2014 the line.", V$be26)),
  list(GOLD, "Insurance losses \u2014 tiny", sprintf("Assumed at a ten-year average excluding Melrose: %s a year, %s bp of insured shares.", V$lossM26, V$lossbp26)),
  list(TEAL, "Investment yield \u2014 what changed", sprintf("The ladder repriced from %s to %s; net income (the bars) went from about zero to %s a year \u2014 above the line.", V$ylo, V$y26, V$ni26)),
  list(NAVY, "Reading the chart", "Top: bars below the line mean retained earnings lag share growth. Bottom: the ratio then falls and the formula adds a cushion; from 2025 the bars clear the line, the ratio rises and the formula adds nothing. The Fund's buffer grows; the formula's cushion is what disappears."))
slides[[3]] <- list(bg = NULL, images = "deck_images/c3_ni.png", shapes = c(
  title_bar("Why the ratio is projected to rise: the Fund now earns more than it needs to keep pace with share growth"),
  pic(0.5, 1.4, 5.6, 3.6, "deck_images/c3_ni.png", 1),
  unlist(lapply(seq_along(rows3), function(i) { y <- 1.38 + (i - 1) * 0.93; rr <- rows3[[i]]
    c(dot(6.45, y + 0.02, rr[[1]], 0.22), tx(6.78, y - 0.05, 2.8, 0.3, rr[[2]], 11.5, TRUE), tx(6.78, y + 0.24, 2.8, 0.68, rr[[3]], 9.5)) })),
  footer(3)),
  notes = "The equity ratio erodes whenever retained earnings grow more slowly than insured shares; the line is the net income that keeps them growing at the same pace. Through 2023 projected net income was negative or near zero (bars below the line), so the ratio fell and the formula added a cushion. From 2025 net income is projected far above the line because maturing low-yield bonds are reinvested at today's rates and the budget was cut - the ratio rises, and a formula that only measures a projected fall has nothing to add. Share growth and losses did not change the picture. The Fund is accumulating capital; what vanishes is the formula's add-on above 1.20%.")

slides[[4]] <- list(bg = NULL, images = "deck_images/c4_drivers.png", shapes = c(
  title_bar("Why it worked until 2024: the three drivers were balanced \u2014 since then yield has taken over"),
  pic(0.5, 1.4, 5.9, 3.6, "deck_images/c4_drivers.png", 1),
  rich(6.65, 1.4, 2.9, 1.55, list(r("Balanced, then not. ", 11, TRUE), r(sprintf("Until 2023 yield, losses and share growth each carried roughly a third of the arithmetic. With earnings near zero the projected ratio fell, the formula produced a %s basis-point cushion, and the result (%s) looked sensible next to the Board's %s.", V$cushion, V$nol2123, V$board26), 11))),
  rich(6.65, 3.0, 2.9, 1.3, list(r("It was never tested. ", 11, TRUE), r("The scenario's mildness did not matter while low earnings did the work. Once yields doubled, the same scenario produced a rising ratio and the cushion disappeared \u2014 nothing about credit-union risk had changed.", 11))),
  tx(6.65, 4.25, 1.1, 0.55, V$ysh26, 28, TRUE, color = TEAL),
  tx(7.75, 4.28, 1.85, 0.6, sprintf("of the arithmetic is yield in 2026 (%s in 2020); losses are down to %s", V$ysh20, V$lsh26), 10),
  footer(4)),
  notes = sprintf("The bars split the combined effect of the three named drivers on the projected five-year change in the equity ratio. In 2021 losses and yield mattered almost equally. By 2026 yield accounts for %s, losses for %s. The crossover is 2024 - the year the NOL started sliding toward the floor.", V$ysh26, V$lsh26))

cards5 <- list(
  list("The \"adverse\" scenario is not a stress (chart)", sprintf("Losses assumed at %s bp of insured shares a year against 9\u201310 bp actually lost in 2009\u201310; share growth assumed at %s against 20%% in 2020. Both are recent averages, so the scenario can only be as severe as the recent past.", V$lossbp26, V$g26)),
  list("It measures a decline, not a buffer", sprintf("Once the ratio is projected to rise the cushion is zero whatever the risk: in 2026 the first %s bp of bad news do not register; %s would take %s a year of losses or %s share growth.", V$dead26, V$board26, V$levL26, V$levG26)),
  list("Procyclical by design", "The NOL falls when rates rise and the budget is cut, and would rise in a downturn, when premiums are hardest. Anything above it is paid out, so good years' earnings leave instead of building the buffer."),
  list("Mechanical artifacts", sprintf("Deposit-timing effects (about %s of the 2022 add-on), budget inputs carried forward unchecked, deposit inflows that earn nothing in the model.", V$lag22)))
slides[[5]] <- list(bg = NULL, images = c("deck_images/c5_losses.png", "deck_images/c5_growth.png"), shapes = c(
  title_bar("What the methodology misses"),
  pic(0.5, 1.35, 4.3, 1.85, "deck_images/c5_losses.png", 1), pic(0.5, 3.25, 4.3, 1.85, "deck_images/c5_growth.png", 2),
  unlist(lapply(seq_along(cards5), function(i) { y <- 1.32 + (i - 1) * 0.96; cc <- cards5[[i]]
    c(rbox(5.05, y, 4.45, 0.88, LGREY), tx(5.18, y + 0.06, 0.4, 0.36, as.character(i), 16, TRUE, color = RED),
      tx(5.55, y + 0.06, 3.85, 0.28, cc[[1]], 11, TRUE), tx(5.55, y + 0.33, 3.85, 0.53, cc[[2]], 8.5)) })),
  footer(5, "Sources: OCE model documentation (losses = ten-year average ex-Melrose; share growth = three-year average + one standard deviation); NCUA Board bulletins 2009\u201310; NCUA Share Insurance Fund history.")),
  notes = "Four limitations. The scenario is built from recent history - losses as a ten-year average excluding Melrose, share growth as a three-year average plus one standard deviation - so it can only be as severe as the last few years; the Fund's real stresses (1991, 2009-10, 2020) were far larger. The formula reads only the projected fall, so it goes blind once the ratio rises. It moves with interest rates and the agency's own budget rather than with risk, and the mandatory distribution above the NOL means a floor-level NOL in a strong year pays out the earnings that should be kept.")

rows6 <- list(
  list("Worst point, not the end", "A dip that recovers shows no decline at the end point."),
  list("Capital actually held", "Count the deposit owed on first-half share growth, billed in the autumn."),
  list("Gains only when earned", "Hold the yield at today's level; no reinvestment windfall in an adverse case."),
  list("Deposits flow in", "12% share growth in year one (2009: 10.5%; 2020: 20%), then the model path."),
  list("Losses arrive", "2 bp of insured shares a year for two years \u2014 a fifth of 2009\u201310."),
  list("One large failure", "A fixed 5 bp for concentration, like the pre-2022 legacy-asset component."))
slides[[6]] <- list(bg = NULL, images = "deck_images/c7_ladder.png", shapes = c(
  title_bar("Remedies: recognising the risks the formula leaves out gives a more secure \u2014 and more stable \u2014 level"),
  pic(0.5, 1.22, 5.5, 3.96, "deck_images/c7_ladder.png", 1),
  tx(6.25, 1.22, 3.3, 0.2, "MEASUREMENT \u2014 GETTING THE ARITHMETIC RIGHT", 7.5, TRUE, color = GREY, spc = 100),
  tx(6.25, 1.46 + 3 * 0.57 + 0.06, 3.3, 0.2, "RISK ELEMENTS \u2014 WHAT A DOWNTURN DOES", 7.5, TRUE, color = GREY, spc = 100),
  unlist(lapply(1:6, function(i) { y <- 1.46 + (i - 1) * 0.57 + if (i > 3) 0.3 else 0; rr <- rows6[[i]]; badge <- if (i <= 3) NAVY else MID
    c(oval(6.25, y + 0.03, 0.3, badge), tx(6.25, y + 0.03, 0.3, 0.3, as.character(i), 10, TRUE, color = WHITE, align = "ctr", anchor = "ctr"),
      tx(6.63, y - 0.02, 1.75, 0.26, rr[[1]], 10, TRUE), tx(8.2, y - 0.02, 1.35, 0.26, sprintf("%s \u2192 %s", step_bp[i], step_lv[i]), 10, TRUE, color = TEAL, align = "r"),
      tx(6.63, y + 0.22, 2.92, 0.3, rr[[2]], 7.5, color = GREY)) })),
  footer(6, "Source: OCE analysis; each change applied cumulatively to the 2026 projection (12_tweaks.R; every year in tables/tweak_ladder.csv). Calibrations in 4\u20136 are illustrative.")),
  notes = sprintf("The framing matters: none of these six was tuned to reach a particular number. The first three correct the measurement and need no view of the future; the last three recognise what a downturn actually does to the Fund, each set at a modest point in its own history - deposit inflows well below 2020, losses at a fifth of 2009-10, one large failure at a fixed charge. Where they land is a result, not a target: for 2026 the six together reach %s, which happens to coincide with the level in force, and the point is that the level in force is what a modest stress implies. Across the seven model years they give %s, a far narrower range than %s from the formula today - a level anchored in risk moves much less from year to year than one anchored in projected earnings. Replaying the Fund's own history through the same model points the same way: %s, with an approximate 2008-10 replay at %s even without the corporate losses. The elements: 1 Worst point - a dip that recovers shows no decline at the end of the horizon. 2 Capital actually held - the June ratio is measured before the deposit true-up on first-half growth. 3 Gains only when earned - the projection reprices old 1%% bonds at 2-3%% inside an adverse case, about $520M over five years; count that when it is earned, since in every past stress rates fell. 4 Deposits flow in - 12%% growth in year one, between 2009 and 2020. 5 Losses arrive - 2 bp a year for two years, a fifth of 2009-10, instead of the ten-year average. 6 One large failure - a fixed 5 bp, as the pre-2022 method carried a fixed legacy-asset component. The FDIC's rule is the cleanest version of the same idea: the smallest starting ratio from which a replay of the worst episode never triggers a restoration plan, with no payouts below it.",
    V$ladder26, V$ladder_rng, V$formula_rng, V$stress_rng, V$r2008))

cols7 <- list(
  list(RED,  "Richer, not safer", sprintf("The NOL fell because the Fund became more profitable. About %s of the swing is the interest-rate environment and most of the rest the agency's own budget; assumed losses stayed at %s of a basis point.", V$yswing, V$lossbp26)),
  list(GOLD, "More exposed, less sensitive", sprintf("A point of share growth or of yield moves the ratio more today than in 2020 (%s \u2192 %s bp; %s \u2192 %s bp). Yet the formula no longer responds to any of them until a stress-sized shock arrives.", V$elg20, V$elg26, V$ely20, V$ely26)),
  list(TEAL, "History says 1.35\u20131.45%", sprintf("A 2020-style surge, a 2009-type loss wave or one large failure replayed on today's Fund land at %s; 2008 replayed gives %s. The %s in force is what a modest stress produces \u2014 not a conservative number.", V$stress_rng, V$r2008, V$board26)))
slides[[7]] <- list(bg = NAVY, images = NULL, shapes = c(
  tx(0.6, 0.4, 8.8, 0.7, "Three things to take away", 26, TRUE, color = WHITE),
  unlist(lapply(1:3, function(i) { x <- 0.6 + (i - 1) * 3.0; cc <- cols7[[i]]
    c(tx(x, 1.35, 0.6, 0.6, as.character(i), 30, TRUE, color = cc[[1]]), tx(x, 1.95, 2.7, 0.4, cc[[2]], 14, TRUE, color = WHITE), tx(x, 2.38, 2.7, 1.7, cc[[3]], 10.5, color = PALE)) })),
  hline(0.6, 4.2, 8.8, "4F6A8F", 0.75),
  rich(0.6, 4.3, 8.8, 0.85, list(r("For the model owners: ", 9.5, TRUE, color = WHITE),
    r("the last projected column is missing from the printed copies; deposit inflows earn nothing in the sheet although the documentation says they are invested; the 2025 run carries a \u221218% budget input into every year; the 2024 sheet's 2025 budget cell reverts to the 2024 figure; and the 2026 workbook already contains a minimum-over-five-years calculation.", 9.5, color = PALE))),
  tx(0.6, 5.2, 8.8, 0.3, paste0("Office of the Chief Economist  \u00b7  ", V$month), 9, color = PALE)),
  notes = "Closing: the Fund is stronger and more exposed at the same time, and the number meant to track that exposure has stopped responding. Anchor the target to history, keep the agency's budget and projected profits out of it, and let good years build the reserve for bad ones.")

## 4. assemble the package ----------------------------------------------------------------------------------------
NS <- 'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"'
tmp <- file.path(tempdir(), "deck_r"); unlink(tmp, recursive = TRUE); dir.create(tmp)
unzip("deck_skeleton.zip", exdir = tmp)
for (d in c("ppt/slides/_rels", "ppt/notesSlides/_rels", "ppt/media", "ppt/_rels")) dir.create(file.path(tmp, d), recursive = TRUE, showWarnings = FALSE)
wfile <- function(path, x) writeLines(enc2utf8(x), file.path(tmp, path), useBytes = TRUE)
media_n <- 0L
for (i in seq_along(slides)) {
  s <- slides[[i]]
  rels <- character(0)
  for (k in seq_along(s$images)) { media_n <- media_n + 1L; mf <- sprintf("image%d.png", media_n); file.copy(s$images[k], file.path(tmp, "ppt/media", mf), overwrite = TRUE)
    rels <- c(rels, sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/%s"/>', k, mf)) }
  nk <- length(s$images)
  rels <- c(rels, sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout" Target="../slideLayouts/slideLayout1.xml"/>', nk + 1),
                  sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesSlide" Target="../notesSlides/notesSlide%d.xml"/>', nk + 2, i))
  bg <- if (is.null(s$bg)) "" else sprintf('<p:bg><p:bgPr><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:effectLst/></p:bgPr></p:bg>', s$bg)
  wfile(sprintf("ppt/slides/slide%d.xml", i), sprintf('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<p:sld %s><p:cSld name="Slide %d">%s<p:spTree><p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr><p:grpSpPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/><a:chOff x="0" y="0"/><a:chExt cx="0" cy="0"/></a:xfrm></p:grpSpPr>%s</p:spTree></p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>',
    NS, i, bg, paste(s$shapes, collapse = "")))
  wfile(sprintf("ppt/slides/_rels/slide%d.xml.rels", i), sprintf('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">%s</Relationships>', paste(rels, collapse = "")))
  wfile(sprintf("ppt/notesSlides/notesSlide%d.xml", i), sprintf('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<p:notes %s><p:cSld><p:spTree><p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr><p:grpSpPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/><a:chOff x="0" y="0"/><a:chExt cx="0" cy="0"/></a:xfrm></p:grpSpPr><p:sp><p:nvSpPr><p:cNvPr id="2" name="Slide Image Placeholder 1"/><p:cNvSpPr><a:spLocks noGrp="1" noRot="1" noChangeAspect="1"/></p:cNvSpPr><p:nvPr><p:ph type="sldImg"/></p:nvPr></p:nvSpPr><p:spPr/></p:sp><p:sp><p:nvSpPr><p:cNvPr id="3" name="Notes Placeholder 2"/><p:cNvSpPr><a:spLocks noGrp="1"/></p:cNvSpPr><p:nvPr><p:ph type="body" idx="1"/></p:nvPr></p:nvSpPr><p:spPr/><p:txBody><a:bodyPr/><a:lstStyle/><a:p><a:r><a:rPr lang="en-US" dirty="0"/><a:t>%s</a:t></a:r></a:p></p:txBody></p:sp></p:spTree></p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:notes>',
    NS, esc(s$notes)))
  wfile(sprintf("ppt/notesSlides/_rels/notesSlide%d.xml.rels", i), sprintf('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesMaster" Target="../notesMasters/notesMaster1.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide" Target="../slides/slide%d.xml"/></Relationships>', i))
}
n <- length(slides)
# presentation.xml: the skeleton's, with the slide list rewritten
pres <- paste(readLines(file.path(tmp, "ppt/presentation.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pres <- sub("<p:sldIdLst>.*?</p:sldIdLst>", paste0("<p:sldIdLst>", paste(sprintf('<p:sldId id="%d" r:id="rId%d"/>', 255 + seq_len(n), 1 + seq_len(n)), collapse = ""), "</p:sldIdLst>"), pres, perl = TRUE)
pres <- sub('<p:notesMasterIdLst><p:notesMasterId r:id="rId[0-9]+"/></p:notesMasterIdLst>', sprintf('<p:notesMasterIdLst><p:notesMasterId r:id="rId%d"/></p:notesMasterIdLst>', n + 2), pres)
wfile("ppt/presentation.xml", pres)
wfile("ppt/_rels/presentation.xml.rels", sprintf('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideMaster" Target="slideMasters/slideMaster1.xml"/>%s<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesMaster" Target="notesMasters/notesMaster1.xml"/><Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/presProps" Target="presProps.xml"/><Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/viewProps" Target="viewProps.xml"/><Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme" Target="theme/theme1.xml"/><Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/tableStyles" Target="tableStyles.xml"/></Relationships>',
  paste(sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide" Target="slides/slide%d.xml"/>', 1 + seq_len(n), seq_len(n)), collapse = ""), n + 2, n + 3, n + 4, n + 5, n + 6))
ct <- c('<Default Extension="xml" ContentType="application/xml"/>', '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>', '<Default Extension="png" ContentType="image/png"/>',
  '<Override PartName="/ppt/presentation.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml"/>',
  '<Override PartName="/ppt/presProps.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.presProps+xml"/>',
  '<Override PartName="/ppt/viewProps.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.viewProps+xml"/>',
  '<Override PartName="/ppt/tableStyles.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.tableStyles+xml"/>',
  '<Override PartName="/ppt/theme/theme1.xml" ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>',
  '<Override PartName="/ppt/slideMasters/slideMaster1.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml"/>',
  '<Override PartName="/ppt/slideLayouts/slideLayout1.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml"/>',
  '<Override PartName="/ppt/notesMasters/notesMaster1.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.notesMaster+xml"/>',
  sprintf('<Override PartName="/ppt/slides/slide%d.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.slide+xml"/>', seq_len(n)),
  sprintf('<Override PartName="/ppt/notesSlides/notesSlide%d.xml" ContentType="application/vnd.openxmlformats-officedocument.presentationml.notesSlide+xml"/>', seq_len(n)),
  '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>',
  '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>')
wfile("[Content_Types].xml", sprintf('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">%s</Types>', paste(ct, collapse = "")))

## 5. write the .pptx (zip) with base R ----------------------------------------------------------------------------
crc_table <- vapply(0:255, function(k) { c <- k; for (j in 1:8) c <- if (bitwAnd(c, 1L)) bitwXor(-306674912L, bitwShiftR(c, 1L)) else bitwShiftR(c, 1L); c }, 1L)
crc32 <- function(raw) { c <- -1L; for (b in as.integer(raw)) c <- bitwXor(crc_table[bitwAnd(bitwXor(c, b), 255L) + 1L], bitwShiftR(c, 8L)); bitwXor(c, -1L) }
write_zip <- function(root, out) {
  files <- list.files(root, recursive = TRUE, all.files = TRUE, full.names = FALSE)
  files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
  con <- file(out, "wb"); on.exit(close(con))
  w2 <- function(x) writeBin(as.integer(x), con, size = 2, endian = "little"); w4 <- function(x) writeBin(as.integer(x), con, size = 4, endian = "little")
  dos_date <- as.integer(bitwOr(bitwOr(bitwShiftL(2026 - 1980, 9), bitwShiftL(1, 5)), 1)); central <- list(); offset <- 0
  for (f in files) {
    raw <- readBin(file.path(root, f), "raw", file.info(file.path(root, f))$size)
    gz <- memCompress(raw, "gzip"); nn <- length(gz); defl <- gz[3:(nn - 4)]; crc <- crc32(raw); name <- charToRaw(enc2utf8(f))
    w4(0x04034b50L); w2(20L); w2(0L); w2(8L); w2(0L); w2(dos_date); w4(crc); w4(length(defl)); w4(length(raw)); w2(length(name)); w2(0L); writeBin(name, con); writeBin(defl, con)
    central[[f]] <- list(name = name, crc = crc, csize = length(defl), usize = length(raw), offset = offset); offset <- offset + 30 + length(name) + length(defl) }
  cd_start <- offset
  for (e in central) { w4(0x02014b50L); w2(20L); w2(20L); w2(0L); w2(8L); w2(0L); w2(dos_date); w4(e$crc); w4(e$csize); w4(e$usize); w2(length(e$name)); w2(0L); w2(0L); w2(0L); w2(0L); w4(0L); w4(e$offset); writeBin(e$name, con); offset <- offset + 46 + length(e$name) }
  w4(0x06054b50L); w2(0L); w2(0L); w2(length(central)); w2(length(central)); w4(offset - cd_start); w4(cd_start); w2(0L); invisible(out) }
write_zip(tmp, "NOL_Executive_Deck_R.pptx")
cat("written NOL_Executive_Deck_R.pptx:", n, "slides,", round(file.info("NOL_Executive_Deck_R.pptx")$size / 1024), "KB\n")
rm(r, tx, rich, txbox, para, run_xml, shape, rbox, oval, hline, pic, png_dims, dot, stat, footer, title_bar, nid, .id, emu, esc, wfile, w, slides, tmp)   # leave no helpers behind that could mask graphics functions
