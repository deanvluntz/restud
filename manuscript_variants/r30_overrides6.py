# -*- coding: utf-8 -*-
# Seventh pass (21 Sep 2026): figures re-derived from the single-source HPFD
# dataset (Dissertation_Dataset_Final.xlsx). Same wording; numbers only.
S = lambda n: "Section §§" + n + "§§"

ID1 = ("The regressors are predetermined with respect to the current year's inflow shock: the fiscal controls are lagged "
       "realisations and the projection is fixed before the flows it is asked to predict, so within-year reverse "
       "causality is excluded by construction. Three threats survive. The first is the lagged dependent variable, which "
       "makes least squares consistent but biased in finite samples by roughly −(1 + 3ρ)/T (Hurwicz, 1950; "
       "Kendall, 1954), about −0.06 at T = 41, so the persistence estimates and the long-run multipliers that divide "
       "by (1 − ρ) are lower bounds. A recursive-design residual bootstrap of the fiscal specification (2,000 "
       "replications) estimates the bias at −0.105, raising persistence from 0.49 to about 0.60 and the long-run "
       "response to the projection from 1.6 to about 2.0 per cent per point, with a recentred percentile interval of "
       "[0.9, 3.0]. The bias cannot manufacture significance of the projection.")
R3 = ("The projection term is the paper's central result. In the fiscal specification (column 1), a one-point increase "
      "in the projected ten-year rise of the OADR, one additional prospective pensioner per thousand working-age "
      "residents, is associated with a 0.8 per cent increase in the non-EU inflow rate (0.0082, HAC s.e. 0.0014); the "
      "trend specification gives 0.0075. The controls carry the predicted signs. Each additional thousand pounds of tax "
      "per working-age person is associated with 4.8 per cent higher inflows (P2), the substitution effect at the heart "
      "of the salience mechanism, where a naive fiscal-need story would predict the opposite. The coefficient of 0.57 on "
      "the lagged log change in debt/GDP (P3) implies that a one-point faster rise in the debt ratio is followed by "
      "roughly 0.6 per cent higher inflows. A point of lagged unemployment reduces inflows by about 2 per cent (P4), "
      "consistent with both the political-cost channel and labour-demand effects. Persistence (ρ ≈ 0.49 and "
      "0.71, both understated by the attenuation of Section 5.2) matches the UK literature.")
LR1 = ("The regressors mix I(1) and I(0) series (Online Appendix Table B1), which rules out a level relationship "
       "arising from common integration order and dictates the bounds procedure of Pesaran, Shin and Smith (2001), "
       "implemented with the R package ARDL (Natsiopoulos, Tzeremes and Finnan, 2026). ADF "
       "tests reject a unit root in the residuals of both specifications (p = 0.024 and 0.038). The bounds F-test "
       "rejects the null of no level relationship at five per cent (F = 4.94, p = 0.03); the t-version is marginal "
       "(p = 0.07), inside the inconclusive region between the bounds rather than failing against "
       "them. With 41 observations the tests have limited power: the data are "
       "consistent with a long-run equilibrium relationship, not proof of one. Table 3 reports the implied "
       "long-run multipliers.")
IC1 = ("*Zero-net-migration projections.* From each vintage's published components of change, the projected OADR at "
       "every horizon is recomputed under zero net migration from the observation year forward, by removing the "
       "vintage's cumulative assumed net migration from the projected working-age denominator (Online Appendix A), so "
       "that the regressor cannot contain the vintages' migration assumptions. It returns 0.0075 (HAC s.e. 0.0013) "
       "against 0.0081 with the published projections, with unchanged significance and diagnostics.")
IC2 = ("*Publication dating.* Dating each vintage from the year after its base reassigns the vintage in force in 21 of "
       "the 41 years, moving one observation by more than fifteen points. The estimate barely moves: 0.0086 (HAC s.e. "
       "0.0015) against 0.0081, significant at 0.1 per cent with diagnostics passing; the zero-migration variant returns 0.0072 (0.0015), the total dependency ratio falls to "
       "−0.0003 (p = 0.85) and the EU placebo stays insignificant (p = 0.35).")
IC3 = ("*The projection's marginal contribution.* With a lagged dependent variable, adjusted R² reaches 0.94 before "
       "the projection enters, so the informative comparison is the nested one. Dropping the projection gives F(1, 35) = "
       "18.79 (p = 0.0001), with the information criterion improving by 15.6 points when it is restored, a "
       "difference conventionally taken to leave no support for the restricted model; and without the projection "
       "RESET falls to p = 0.098 against p = 0.62 with it. The omission does not merely cost fit; it leaves detectable "
       "misspecification.")
F1 = ("*Old-age versus total dependency.* If the mechanism is fiscal, the projection that matters is the old-age ratio, "
      "since pensioners are expensive, not the total ratio, whose child component is cheap (" + S("4.2") + "). "
      "Substituting the projected ten-year change in the total dependency ratio eliminates the result: the coefficient "
      "falls to 0.0015 (p = 0.26) and is indistinguishable from zero, as it is in every other variant, at every horizon, in levels as in deltas. Against the no-projection "
      "baseline the contrast is sharper still: the old-age ratio improves the information criterion by 15.6 points, "
      "the total ratio worsens it by 0.9 (F(1, 35) = 0.99, p = 0.33), so the model is better "
      "specified with no projection than with the wrong one. A generic demographic confounder has no reason to select "
      "the pensioner-specific ratio so cleanly; a fiscal mechanism must.")
PE = ("Mitchell, Pain and Riley (2011) attribute the post-1997 expansion to a liberal policy era, captured by a step "
      "indicator from 1998, which their 2007 sample end makes indistinguishable from a 1998–2007 window. Adding their "
      "full indicator set (POL97, POL02 for the 2002 skilled-worker route, and enlargement indicators for 1986, 1995 and "
      "2004) to the fiscal specification leaves the indicators jointly irrelevant under HAC inference (F(5, 30) = 1.19, "
      "p = 0.34), while the projection holds at 0.0078 (p = 0.001). The "
      "reason is that the indicator nearly discretises the projection: the ten-year delta averages 0.9 points before "
      "1998, 14.9 within the window and −4.5 after 2007, so the literature's dummies were standing in for the "
      "projections. Two checks confirm the reading. Interacting the step with the projection returns a negative "
      "interaction (−0.0067, p = 0.37), the opposite of what a post-1997 account requires; and in Mitchell, Pain and "
      "Riley's own differenced form the projection is the only variable that registers (0.0062, p = 0.099 for non-EU "
      "inflows; 0.0072, p = 0.033 for total inflows), while no indicator approaches significance. Where the "
      "indicators do earn their place is on total inflows including the EU margin the government could not ration "
      "(F(5, 30) = 5.20, p = 0.001), with the projection falling to 0.0028 (p = 0.31), the EU placebo arriving from the "
      "other direction (maximum VIF 62; Online Appendix B).")
FR = ("The remaining checks, every check in the trend specification, and the alternative tax measures (Mendoza, Razin and Tesar, 1994) are in Online Appendix B. The result does not depend on the per-capita scaling: on "
      "gross non-EU inflows the coefficient is 0.0080 (HAC s.e. 0.0014), and in levels each further projected point is associated with roughly 1,300 to 1,400 additional "
      "arrivals a year. On the LTIM series from 1991 it is significant at 0.1 per cent (0.0159, s.e. 0.0040, N = "
      "28). The projected level in place of the change gives 0.0082; the term is significant at three, five, seven, and "
      "ten years, with ten fitting best, and the one-year change alone carries no signal (p = 0.57). Starting the sample "
      "in 1977, 1983 or 1987 returns 0.0082, 0.0087 and 0.0082, all significant at 0.1 per cent. What the battery cannot exclude is "
      "a confounder sharing the projection's exact profile, fiscal, pensioner-specific, operative only on the controlled "
      "margin, and timed to the vintages; none has suggested itself, and closing that residual vulnerability is what "
      "the agenda of " + S("7") + " is built for.")

# eighth pass (22 Sep 2026, ARDL citation + 1977 start): compensating trims
from r30_overrides3 import X3
X3 = X3.replace(" External validity can be tested in a cross-country panel on UN projection vintages, "
                "and the pension-age and migration instruments modelled jointly.",
                " External validity can be tested in a cross-country panel on UN projection vintages.")
X3 = X3.replace("with Home Office quarterly visa grants by category as an outcome of sufficient frequency for an "
                "event-study design around release dates.", "with quarterly visa grants by category supporting an event study around release dates.")
X3 = X3.replace("The mechanism can be taken to the Treasury files already inside the "
                "thirty-year declassification window, to undigitised Treasury forecast material (HM Treasury, 1997), and to text "
                "analysis of Budget documents.", "The mechanism can be taken to the Treasury files inside the thirty-year window and to "
                "undigitised forecast material (HM Treasury, 1997).")
from r30_overrides2 import C1
C1 = C1.replace(", which also predicts the preference for legislating pension-age increases decades before "
                "they bind.", ".")
_i = C1.find(" Beyond the substantive claim")
if _i > 0: C1 = C1[:_i]      # contributions already stated in the Introduction
X3 = X3.replace(" External validity can be tested in a cross-country panel on UN projection vintages.",
                " External validity can be tested on UN projection vintages.")
X3 = X3.replace("the dependent variable pools work, study, and family entry, so the implication that the "
                "response concentrates where discretion and youth are greatest is testable only with administrative data;",
                "the dependent variable pools work, study, and family entry;")

# VIF paragraph (Sam, 22 Sep 2026), added after the fit/diagnostics paragraph;
# Sam will make the offsetting cuts in the 30pp version himself.
from r30_overrides import R4
R4 = R4 + ("\n\nThe fiscal specification's maximum VIF (13.0) exceeds the conventional threshold of 10, but it belongs "
           "to the lagged dependent variable (13.0) and the lagged tax burden (10.4), two trending levels that share the "
           "growth of the period; the projection term's VIF is 1.3. Rules of thumb for the VIF are in any case poor guides "
           "to whether collinearity matters, which depends on whether the estimates are precise enough for the question at "
           "hand (O'Brien, 2007). Finally, the trend specification, with a maximum VIF of 6.0, returns a projection "
           "coefficient of 0.0073, so nothing in the headline result depends on the collinearity of the fiscal "
           "specification.")
