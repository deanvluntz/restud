# -*- coding: utf-8 -*-
# Fifth pass for the 30-page version (Sam, 18 Sep 2026): fiscal specification
# quoted for every check; enlargement-year variant dropped; MPR section condensed.
S = lambda n: "Section §§" + n + "§§"

F1 = ("*Old-age versus total dependency.* If the mechanism is fiscal, the projection that matters is the old-age ratio, "
      "since pensioners are expensive, not the total ratio, whose child component is cheap (" + S("4.2") + "). "
      "Substituting the projected ten-year change in the total dependency ratio eliminates the result: the coefficient "
      "falls to 0.0015 in the fiscal specification (p = 0.25; 0.0018, p = 0.21 in the trend) and is indistinguishable "
      "from zero, as it is in every other variant, at every horizon, in levels as in deltas. Against the no-projection "
      "baseline the contrast is sharper still: the old-age ratio improves the information criterion by 15.8 points, "
      "the total ratio worsens it by 0.8 (F(1, 35) = 1.03, p = 0.32; 11.9 and 0.5, F(1, 35) = 1.32, p = 0.26, in the "
      "trend specification), so the model is better "
      "specified with no projection than with the wrong one. A generic demographic confounder has no reason to select "
      "the pensioner-specific ratio so cleanly; a fiscal mechanism must.")
F2 = ("*The uncontrolled margin.* Might some unobserved driver, rising UK attractiveness, labour demand, or networks, "
      "correlate with projected ageing and move all migration, with policy irrelevant? EU inflows provide the test, "
      "responding freely to every such driver but not to UK entry policy. Re-estimating the full specification on EU "
      "inflows, constructed identically, returns an OADR-projection coefficient of 0.0028 (p = 0.42) in the fiscal "
      "specification and 0.0022 (p = 0.52) in the trend; only the persistence term survives. Figure 2 shows the "
      "contrast as added-variable plots in which the fitted slope is the coefficient: a clear conditional relationship "
      "on the controlled margin, none on the uncontrolled one. Whatever moved non-EU inflows in step with projected "
      "ageing did not move the flows the government could not ration; the confound would have to operate exclusively "
      "on the controlled margin, which is to say it would have to *be* policy.")
PE = ("Mitchell, Pain and Riley (2011) attribute the post-1997 expansion to a liberal policy era, captured by a step "
      "indicator from 1998, which their 2007 sample end makes indistinguishable from a 1998–2007 window. Adding their "
      "full indicator set (POL97, POL02 for the 2002 skilled-worker route, and enlargement indicators for 1986, 1995 and "
      "2004) to the fiscal specification leaves the indicators jointly irrelevant under HAC inference (F(5, 30) = 1.23, "
      "p = 0.32; 1.67, p = 0.17 in the trend specification), while the projection holds at 0.0078 (p < 0.001). The "
      "reason is that the indicator nearly discretises the projection: the ten-year delta averages 0.9 points before "
      "1998, 14.9 within the window and −4.5 after 2007, so the literature's dummies were standing in for the "
      "projections. Two checks confirm the reading. Interacting the step with the projection returns a negative "
      "interaction (−0.0068, p = 0.37), the opposite of what a post-1997 account requires; and in Mitchell, Pain and "
      "Riley's own differenced form the projection is the only variable that registers (0.0062, p = 0.099 for non-EU "
      "inflows; 0.0072, p = 0.033 for total inflows), while the indicators return p = 0.68, 0.79 and 0.71. Where the "
      "indicators do earn their place is on total inflows including the EU margin the government could not ration "
      "(F(5, 30) = 5.10, p = 0.002), with the projection falling to 0.0028 (p = 0.30), the EU placebo arriving from the "
      "other direction, reported with the caveat that this specification reaches a maximum VIF of 62 (Online Appendix B).")

FR = ("The remaining checks are reported in Online Appendix B. The result does not depend on the per-capita scaling: on "
      "gross non-EU inflows the coefficient is 0.0080 (HAC s.e. 0.0014) in the fiscal and 0.0073 (0.0015) in the trend "
      "specification, and in levels each further projected point is associated with roughly 1,300 to 1,400 additional "
      "arrivals a year. On the LTIM series from 1991 it remains significant at five per cent (0.0103, s.e. 0.0038, N = "
      "28). The projected level in place of the change gives 0.0082 (0.0080 in the trend specification); the term is significant at three, five, seven, and "
      "ten years, with ten fitting best, and the one-year change alone carries no signal (p = 0.56). Starting the sample "
      "in 1983 or 1987 returns 0.0069 and 0.0062, both significant at five per cent. What the battery cannot exclude is "
      "a confounder sharing the projection's exact profile, fiscal, pensioner-specific, operative only on the controlled "
      "margin, and timed to the vintages; none has suggested itself, and closing that residual vulnerability is what "
      "the agenda of " + S("7") + " is built for.")
