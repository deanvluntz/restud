# -*- coding: utf-8 -*-
"""Manuscript sample moved to 1977-2019 (Sam, 22 Sep 2026; thesis stays 1979-2019).
Applied to the FINAL text of the 45pp and 30pp variants, after every override
layer, so period references are consistent whichever layer supplied the text.
Result figures (coefficients, T in the bias formula, etc.) are swept separately
once the 1977 R run (run_all_1977.R / build_tables_1977.R) is in."""
PAIRS = [
    ("The sample is annual, 1979–2019 (41 observations): 1979 opens a period of continuity in the policy consensus and "
     "is the first year in which net migration turned positive;",
     "The sample is annual, 1977–2019 (43 observations), the earliest the lagged debt term allows, which places the "
     "1976 IMF episode inside the information set of its first observation;"),
    ("whole period 1979–2019", "whole period 1977–2019"),
    ("whole of 1979–2019", "whole of 1977–2019"),
    ("(41 observations)", "(43 observations)"),
    ("at 41 observations", "at 43 observations"),
    ("With 41 observations", "With 43 observations"),
    ("with 41 observations", "with 43 observations"),
    ("forty-one years and seven Prime Ministers", "forty-three years and eight Prime Ministers"),
    ("one country and forty-one years", "one country and forty-three years"),
    ("between 1979 and 2019 can be explained", "between 1977 and 2019 can be explained"),
    ("## 2.3 Control of the non-EU margin, 1979–2019", "## 2.3 Control of the non-EU margin, 1977–2019"),
    ("Starting the sample in 1977 (the earliest possible), 1983 or 1987 returns 0.0082, 0.0087 and 0.0082",
     "Starting the sample in 1979, 1983 or 1987 returns 0.0081, 0.0087 and 0.0082"),
    ("Starting the sample in 1977, 1983 or 1987 returns 0.0082, 0.0087 and 0.0082",
     "Starting the sample in 1979, 1983 or 1987 returns 0.0081, 0.0087 and 0.0082"),
    ("from 96,000 in 1979 to 343,000", "from 73,000 in 1977 to 343,000"),
]
def period1977(text):
    for a, b in PAIRS:
        text = text.replace(a, b)
    return text

# Result figures for the 1977-2019 sample (run_all_1977_output.txt / tables_1977.json,
# 22 Sep 2026), for text that lives in the 45pp/30pp assembler constants rather than
# in manuscript_part1/2.md (which carry the same figures directly).
RESULTS = [
    ("carries no signal (p = 0.57)", "carries no signal (p = 0.47)"),
    ("(1.6 per cent uncorrected)", "(1.5 per cent uncorrected)"),
    ("in 21 of the 41 years", "in 22 of the 43 years"),
    ("in the twenty-one years a new vintage", "in the twenty-two years a new vintage"),
    ("the projection coefficient is 0.0080 (HAC s.e. 0.0014), within a sixth of a standard error of its scaled counterpart,",
     "the projection coefficient is 0.0082 (HAC s.e. 0.0015), identical to its scaled counterpart,"),
    ("(1,369, s.e. 307)", "(1,379, s.e. 318)"),
    ("returns an almost identical coefficient (0.0082 against 0.0081, p < 0.001)",
     "returns an almost identical coefficient (0.0085 against 0.0082, p < 0.001)"),
    ("(−0.0001, p = 0.98; the equal-coefficient restriction is rejected, F(1, 34) = 7.81, p = 0.008)",
     "(0.0002, p = 0.95; the equal-coefficient restriction is rejected, F(1, 36) = 8.28, p = 0.007)"),
    ("gives 0.0089 (p < 0.001) on the inherited part and 0.0040 (p = 0.37) on the revision, not distinguishable from it (F(1, 34) = 1.11, p = 0.30)",
     "gives 0.0092 (p < 0.001) on the inherited part and 0.0029 (p = 0.53) on the revision, not distinguishable from it (F(1, 36) = 1.64, p = 0.21)"),
    ("about −0.06 at T = 41", "about −0.06 at T = 43"),
    ("estimates the bias at −0.105, raising persistence from 0.49 to about 0.60 and the long-run response to the projection from 1.6 to about 2.0 per cent per point, with a recentred percentile interval of [0.9, 3.0].",
     "estimates the bias at −0.11, raising persistence from 0.47 to about 0.58 and the long-run response to the projection from 1.5 to about 2.0 per cent per point, with a recentred percentile interval of [0.9, 2.8]."),
    ("(0.0082, HAC s.e. 0.0014); the trend specification gives 0.0075.", "(0.0082, HAC s.e. 0.0015); the trend specification gives 0.0073."),
    ("associated with 4.8 per cent higher inflows (P2)", "associated with 5.2 per cent higher inflows (P2)"),
    ("The coefficient of 0.57 on the lagged log change in debt/GDP (P3) implies that a one-point faster rise in the debt ratio is followed by roughly 0.6 per cent higher inflows.",
     "The coefficient of 0.54 on the lagged log change in debt/GDP (P3) implies that a one-point faster rise in the debt ratio is followed by roughly 0.5 per cent higher inflows."),
    ("Persistence (ρ ≈ 0.49 and 0.71,", "Persistence (ρ ≈ 0.47 and 0.69,"),
    ("with RESET, Breusch–Godfrey at both orders, Breusch–Pagan and Jarque–Bera all failing to reject in both columns.",
     "with RESET, Breusch–Godfrey at first and second order, Breusch–Pagan and Jarque–Bera all failing to reject at five per cent in both columns (higher-order Breusch–Godfrey tests reject in the fiscal specification; Online Appendix B)."),
    ("It returns 0.0075 (HAC s.e. 0.0013) against 0.0081 with the published projections", "It returns 0.0075 (HAC s.e. 0.0013) against 0.0082 with the published projections"),
    ("The estimate barely moves: 0.0086 (HAC s.e. 0.0015) against 0.0081, significant at 0.1 per cent with diagnostics passing; the zero-migration variant returns 0.0072 (0.0015), the total dependency ratio falls to −0.0003 (p = 0.85) and the EU placebo stays insignificant (p = 0.35).",
     "The estimate barely moves: 0.0090 (HAC s.e. 0.0015) against 0.0082, significant at 0.1 per cent with diagnostics passing; the zero-migration variant returns 0.0076 (0.0016), the total dependency ratio falls to 0.0002 (p = 0.91) and the EU placebo stays insignificant (p = 0.28)."),
    ("Dropping the projection gives F(1, 35) = 18.79 (p = 0.0001), with the information criterion improving by 15.6 points when it is restored,",
     "Dropping the projection gives F(1, 37) = 17.82 (p < 0.001), with the information criterion improving by 14.9 points when it is restored,"),
    ("and without the projection RESET falls to p = 0.098 against p = 0.62 with it.", "and without the projection RESET falls to p = 0.12 against p = 0.41 with it."),
    ("ADF tests reject a unit root in the residuals of both specifications (p = 0.024 and 0.038). The bounds F-test rejects the null of no level relationship at five per cent (F = 4.94, p = 0.03); the t-version is marginal (p = 0.07), inside the inconclusive region between the bounds rather than failing against them.",
     "ADF tests reject a unit root in the residuals of both specifications (p = 0.022 and 0.046). The bounds F-test rejects the null of no level relationship at one per cent (F = 6.16, p < 0.01), and the t-version at five per cent (t = −4.09, p < 0.05)."),
    ("against an uncorrected multiplier of 1.6 per cent (p = 0.001)", "against an uncorrected multiplier of 1.5 per cent (p = 0.002)"),
    ("the coefficient falls to 0.0015 (p = 0.26) and is indistinguishable from zero", "the coefficient falls to 0.0020 (p = 0.16) and is indistinguishable from zero"),
    ("the old-age ratio improves the information criterion by 15.6 points, the total ratio worsens it by 0.9 (F(1, 35) = 0.99, p = 0.33), so the model is better specified with no projection than with the wrong one.",
     "the old-age ratio improves the information criterion by 14.9 points, the total ratio by 0.1 (F(1, 37) = 1.83, p = 0.18), barely earning the degree of freedom it costs."),
    ("returns an OADR-projection coefficient of 0.0028 (p = 0.42)", "returns an OADR-projection coefficient of 0.0030 (p = 0.41)"),
    ("(F(5, 30) = 1.19, p = 0.34), while the projection holds at 0.0078 (p = 0.001)", "(F(5, 32) = 1.09, p = 0.38), while the projection holds at 0.0080 (p = 0.001)"),
    ("the ten-year delta averages 0.9 points before 1998", "the ten-year delta averages 0.6 points before 1998"),
    ("returns a negative interaction (−0.0067, p = 0.37)", "returns a negative interaction (−0.0072, p = 0.34)"),
    ("the projection is the only variable that registers (0.0062, p = 0.099 for non-EU inflows; 0.0072, p = 0.033 for total inflows), while no indicator approaches significance.",
     "no indicator approaches significance, while the projection keeps its sign, if not its precision (0.0050, p = 0.18 for non-EU inflows; 0.0057, p = 0.098 for total inflows)."),
    ("(F(5, 30) = 5.20, p = 0.001)", "(F(5, 32) = 5.50, p < 0.001)"),
    ("(maximum VIF 62;", "(maximum VIF 56;"),
    ("on gross non-EU inflows the coefficient is 0.0080 (HAC s.e. 0.0014),", "on gross non-EU inflows the coefficient is 0.0082 (HAC s.e. 0.0015),"),
    ("The projected level in place of the change gives 0.0082;", "The projected level in place of the change gives 0.0085;"),
    ("on the order of 19 per cent", "on the order of 20 per cent"),
]
_period1977 = period1977
def period1977(text):
    text = _period1977(text)
    for a, b in RESULTS:
        text = text.replace(a, b)
    return text
