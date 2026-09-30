# -*- coding: utf-8 -*-
"""Applies the 22 Sep 2026 restructure to the 30-page variant after assemble_r30.py:
headline result + Figure 1 in the introduction, pointer to Online Appendix E,
Proposition 1 (proof in Online Appendix D), Figures 3-4 with the timing placebo and
second-lag check, and compensating cuts. Paragraphs are matched by opening words."""
import io, re
def paras(f): return io.open(f, encoding="utf-8").read()
t1, t2 = paras("manuscript_r30_part1.md"), paras("manuscript_r30_part2.md")
def sub(text, a, b):
    assert text.count(a) == 1, a[:70]; return text.replace(a, b)
def para_starting(text, start):
    m = [l for l in text.split("\n") if l.startswith(start)]; assert len(m) == 1, start; return m[0]

# 1. Introduction: answer and headline in the second paragraph, Figure 1 after it
emp = para_starting(t1, "The empirical framework is an autoregressive distributed lag (ARDL) regression")
t1 = sub(t1, "\n" + emp + "\n", "\n")
new2 = ("This paper proposes and tests a fiscal explanation: that successive governments used the least visible "
        "instrument available to them, controlled immigration, to offset the fiscal cost of projected population ageing. "
        + emp + " Figure 1 shows the pattern the estimates formalise: non-EU inflows rise when the official projections "
        "foresee faster ageing, under governments of both parties and well before 1997.")
first = para_starting(t1, "Across the period studied")
t1 = sub(t1, first + "\n", first + "\n\n" + new2 + "\n\n[[FIGURE1]]\n")
t1 = sub(t1, "This paper proposes and tests a fiscal explanation, motivated by the Office for Budget Responsibility's practice",
             "The explanation is motivated by the Office for Budget Responsibility's practice")
t2 = sub(t2, "\n[[FIGURE1]]\n", "\n")

# 2. Section 2: pointer to the full account
t1 = sub(t1, "a unique capacity “to initiate and force through policies” (Institute for Government, 2024b).",
             "a unique capacity “to initiate and force through policies” (Institute for Government, 2024b). The full institutional history is in Online Appendix E.")

# 5. Proposition 1
pred = para_starting(t1, "with all coefficients positive. The model delivers four predictions.")
t1 = sub(t1, pred, "with all coefficients positive. Online Appendix D proves the following. *Proposition 1.* If taxation is more "
  "salient than migration (sτ > sₘ > 0), adjustment is costly (ψ > 0), the debt limit tightens in the inherited stock, and the "
  "marginal fiscal benefit of migration rises with projected ageing, then: (P1) β₁ > 0, larger the greater the salience gap "
  "sτ/sₘ; (P2) β₂ > 0, provided the salience of a further tax increase rises steeply enough with the burden already borne; "
  "(P3) β₃ > 0, the debt constraint being measured by the recent change in the debt ratio; (P4) β₄ > 0; (P5) ρ ∈ (0, 1); "
  "(P6) inflows the government cannot ration do not respond to the projection; and (P7) the response to the total dependency "
  "ratio, whose child component is fiscally cheap, is attenuated towards zero. P6 and P7 are the falsification tests of "
  "Section 6.5, which a labour-demand or network account of the same inflows would fail.")

# 3-4. Figures 3 and 4, timing placebo, second lag
fr = para_starting(t2, "The remaining checks, every check in the trend specification")
t2 = sub(t2, fr, "The remaining checks, every check in the trend specification, and the alternative tax measures (Mendoza, "
  "Razin and Tesar, 1994) are in Online Appendix B. Figure 3 plots the coefficient at every horizon: the projection term is "
  "significant from three years on and fits best at ten, the one- and two-year changes carry no signal (p = 0.47 and 0.11), "
  "and the total dependency ratio registers at no horizon. Figure 4 collects every variant, including gross inflows, the "
  "projected level, alternative tax measures, the policy-era indicators, sample starts in 1979, 1983 and 1987, and the LTIM "
  "series from 1991: all fifteen intervals exclude zero and both placebo intervals include it. A second lag of the dependent "
  "variable is insignificant (p = 0.65) and leaves the higher-order serial correlation in place, which is why inference is "
  "HAC. Rotating the projection series through the sample years, which keeps its persistence but breaks its timing, gives a "
  "randomisation p-value of 1/43 = 0.023: none of the 42 rotations produces a t-statistic as large as the actual 5.68. What "
  "the battery cannot exclude is a confounder sharing the projection's exact profile, fiscal, pensioner-specific, operative "
  "only on the controlled margin, and timed to the vintages; none has suggested itself, and closing that residual "
  "vulnerability is what the agenda of Section 7 is built for.\n\n[[FIGURE3]]\n\n[[FIGURE4]]")


# Home Office (2008) evidence: the premise stated on the record (Sam, 23 Sep 2026)
ho = para_starting(t1, "For the turn-of-the-millennium expansion the mechanism is documented directly")
t1 = sub(t1, ho, ho + " The premise was later stated on the record: the Home Office's 2008 evidence to the House of Lords "
  "reported its own estimate that migrants paid more in taxes, relative to what they received, than the UK-born, and cited "
  "GAD analysis in which higher migration lowered the National Insurance rate needed in 2060–61 from 27.9 to 26.3 per cent "
  "(Home Office, 2008). The argument requires only that the government assumed this, not that it was true.")
ref06 = para_starting(t2, "Home Office (2006).")
t2 = sub(t2, ref06, ref06 + "\nHome Office (2008). 'Memorandum by the Home Office', in House of Lords Select Committee on "
  "Economic Affairs, *The Economic Impact of Immigration, Volume II: Evidence*, HL Paper 82-II. London: The Stationery Office.")

# HAC Wald F in place of the classical nested F (Sam, 23 Sep 2026)
t2 = sub(t2, "Dropping the projection gives F(1, 37) = 17.82 (p < 0.001), with the information criterion improving by 14.9 points when it is restored",
             "A HAC Wald test of dropping the projection gives F(1, 37) = 32.23 (p < 0.001), and the information criterion improves by 14.9 points when it is restored")
t2 = sub(t2, "(F(1, 37) = 1.83, p = 0.18)", "(HAC Wald F(1, 37) = 2.11, p = 0.16)")

# compensating cuts (30-page limit)
def repl_para(text, start, new):
    p = para_starting(text, start); return sub(text, p, new) if new else sub(text, "\n" + p + "\n", "\n")
t1 = repl_para(t1, "Work permits regulating entry from outside the Empire",
  "The Immigration Act 1971 ended Commonwealth free movement two years before EEC accession created its European replacement, and created the delegated machinery through which all later non-European entry policy has been made (Hatton and Wheatley Price, 1999; Bauer and Salt, 2020). Throughout the sample control was exercised exclusively over the non-EU margin, through two regimes.")
t1 = repl_para(t1, "Within the executive, coordination between the fiscal centre",
  "Within the executive, coordination requires no public act: collective responsibility forecloses public dissent, and the Treasury sets every department's budget. The claim is not that migration targets were written into Budget documents, which Section 3 explains they would not be, but that the machinery existed, was centralised, and operated below the threshold of scrutiny throughout.")
t1 = repl_para(t1, "The projections' forecasting power comes from",
  "Old-age dependency is uniquely forecastable: Britain's baby boom arrived in two waves, the second and larger over roughly 1955–65, each reaching pension age on a schedule known decades ahead (Jones, 1980), so the people who will be pensioners in ten years are alive, resident, and counted today.")
t1 = repl_para(t1, "For the government the NPP is an internal product",
  "For the government the NPP is an internal product: for five decades GAD, a department of government, produced it on assumptions agreed with the Treasury. Toward the end of the sample its use at Budget time became statutory: the Code for Fiscal Stability (HM Treasury, 1998) required illustrative long-term fiscal projections over a horizon of “no less than 10 years into the future” (HM Treasury, 1999, p. 87), the horizon at which the projection variable is measured and, Section 6.1 reports, the one that fits best.")
t2 = repl_para(t2, "A brief account of the path to the final specification",
  "The final specification was not the first. A five-year level-form window produced a significant projection coefficient but failed the Breusch–Godfrey test (p = 0.018), motivating the autoregressive term; no growth measure or gilt yield mattered; and in logs the best-fitting horizon lengthened from five to ten years, the statutory minimum horizon for the government's own long-term projections (Section 4.2).")
t2 = repl_para(t2, "*Recognition of the mechanism elsewhere.*", "")
t2 = repl_para(t2, "The estimated rule acquires content against the sample's political history",
  "The estimated rule acquires content against the sample's political history, because the projection variable moves at times no party-political account selects (Figure 1). The projected ten-year OADR change first turned positive in the early 1980s, and non-EU inflows rose in 1981–85 under a Conservative government publicly committed to restriction; it turned negative around 1990 and inflows stagnated; it returned to positive territory in 1996 and inflows resumed growth before the change of government. In 2002–03 it averaged roughly 25 points as the first baby-boom wave arrived a decade from pension age, and gross non-EU inflows cleared 300,000 for the first time in 2004. Read through the model, the post-1997 turn was the arrival of the baby boomer at the projection horizon, not of a new ideology: had the 1997 election gone the other way, the model expects the expansion regardless.")
t2 = repl_para(t2, "Read through the model, the post-1997 turn", "")
t2 = repl_para(t2, "*The post-2019 episode.*",
  "*The post-2019 episode.* After Brexit a government elected to reduce migration instead liberalised the non-EU regime (Migration Observatory, 2021), amid a pandemic debt shock and projections of renewed ageing (ONS, 2019); net migration rose from 184,000 in 2019 to over 900,000 in 2023, overwhelmingly non-EU (Migration Observatory, 2026; BBC News, 2025). No structural claim rests on it, but the model predicts the direction of the response and the governing party's stated disposition does not.")
t2 = sub(t2, "# 7. Discussion: External Events, Recognition, and Limitations", "# 7. Discussion: External Events and Limitations")
t2 = repl_para(t2, "*Limitations and agenda.*",
  "*Limitations and agenda.* The zero-migration reconstruction treats all net migrants as working-age, the dependent variable pools entry categories, and outside 2000–2001 the evidence for the mechanism is institutional and statistical. The remedies are an event study of visa grants around NPP releases, the Treasury archives (HM Treasury, 1997), and a cross-country panel on UN projection vintages.")
t2 = repl_para(t2, "The finding bears on a question larger than migration policy.",
  "The finding bears on a question larger than migration policy. Democracies facing slow-moving fiscal pressures possess instruments that differ enormously in visibility, from taxes announced from a despatch box to entry rules amended by statutory instrument on a quiet parliamentary afternoon, and the British case suggests that consequential policy can flow through the least visible instrument for decades precisely because it is least visible. As the demographic transition arrives across the rich world, understanding when governments reach for quiet instruments, and what that choice does to democratic accountability, is among the more consequential questions the political economy of public finance can ask.")
t2 = sub(t2, ", conditional on the Treasury's fiscal state variables and with signs matching the salience model.", ".")
t2 = sub(t2, "This paper asked whether the sustained and politically homeless expansion of controlled immigration to the United Kingdom between 1977 and 2019 can be explained within the outlines of fiscal policy. A newly assembled series of the government's own ageing projections predicts non-EU inflows, the margin policy controlled, with",
             "This paper asked whether the politically homeless expansion of controlled immigration to the United Kingdom between 1977 and 2019 can be explained by fiscal policy. The government's own ageing projections predict non-EU inflows, the margin policy controlled, with")

# drop person-author references no longer cited in the 30pp body or the Online Appendix
APPX = io.open("build_online_appendix.js", encoding="utf-8").read()
body_all = t1 + t2.split("[[REFS]]")[0]
head, sep, refs = t2.partition("[[REFS]]")
keep = []
for r in refs.split("\n"):
    m = re.match(r"([A-Z][A-Za-zÀ-ÿ'’\-]+), [A-Z]\.", r)
    if m and m.group(1) not in body_all and m.group(1) not in APPX: continue
    keep.append(r)
t2 = head + sep + "\n".join(keep)
io.open("manuscript_r30_part1.md", "w", encoding="utf-8").write(re.sub(r"\n{3,}", "\n\n", t1))
io.open("manuscript_r30_part2.md", "w", encoding="utf-8").write(re.sub(r"\n{3,}", "\n\n", t2))
print("r30 restructured; words:", len((t1 + t2.split("[[REFS]]")[0]).split()))
