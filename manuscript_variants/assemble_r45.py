# -*- coding: utf-8 -*-
"""
Assemble the 45-page ReStud-structured manuscript from the original source
paragraphs. Unchanged paragraphs are pulled by line number from
manuscript_part1.md / manuscript_part2.md so they remain identical to the
original; edited paragraphs are written out here. Cross-references are
remapped to the new numbering at the end. NEW references written in this
file use the form  Section §§n§§  so the remap pass cannot re-hit them.

Structure (template: Dustmann et al. 2019; Bertrand et al. 2019):
  1 Introduction
  2 Institutional Background (2.1 Treasury, 2.2 salience, 2.3 controlled
    margin, 2.4 documentary record)          [old 2.6 dropped; old 2.4 -> 4.2]
  3 Model
  4 Data (4.2 absorbs the NPP background)
  5 Empirical Strategy
  6 Empirical Analysis (6.1 spec dev, 6.2 main, 6.3 identification checks,
    6.4 long run, 6.5 falsification, 6.6 policy era, 6.7 further robustness,
    6.8 history)                             [old 6 + 7 merged]
  7 Discussion (post-2019, recognition, limitations, compressed agenda)
  8 Conclusion
"""
import io, re

P1 = io.open("manuscript_part1_variantsrc.md", encoding="utf-8").read().split("\n")
P2 = io.open("manuscript_part2_variantsrc.md", encoding="utf-8").read().split("\n")

def L1(n): return P1[n - 1].rstrip()
def L2(n): return P2[n - 1].rstrip()
def EQ1(a, b): return "\n".join(P1[a - 1:b])
def EQ2(a, b): return "\n".join(P2[a - 1:b])
S = lambda n: "Section §§" + n + "§§"          # protected new reference

# ---------------------------------------------------------------- edits ----
ROADMAP = ("The paper proceeds as follows. Section 2 sets out the institutional background: the Treasury's position, "
           "the salience asymmetry between the Budget and the Immigration Rules, the instruments of control over non-EU "
           "entry, and the documentary record. Section 3 develops the model; Section 4 describes the data, including the "
           "reconstruction of the projection series; Section 5 the empirical strategy and identification threats. "
           "Section 6 presents the empirical analysis: the main estimates, the identification checks, the long-run "
           "relationship, the falsification tests, the horse race against the structural-change account, and a reading "
           "of the political history through the estimated rule. " + S("7") + " discusses the post-2019 episode, "
           "recognition of the mechanism elsewhere, and limitations; " + S("8") + " concludes.")

DELL = ("The second feature is the Treasury's demonstrated willingness to pursue its doctrine independently of, and "
        "occasionally against, the elected government of the day. The clearest case is the International Monetary Fund's "
        "loan of 1976. With the Labour Cabinet split over its response to a sterling crisis, the Treasury negotiated the "
        "largest loan in the Fund's history on terms of austerity justified by debt figures later shown to have been "
        "grossly overstated in the documents submitted to the IMF. Edmund Dell, then Paymaster General, defended the "
        "exaggeration in print as necessary to secure cuts that he and others in the department believed essential, "
        "against the platform on which the government had been elected (Dell, 1991; López, 2014). An earlier attempt to "
        "divide the department's authority, the Department of Economic Affairs created on Labour's 1964 mandate, ended "
        "with the Treasury re-absorbing its rival within five years (Clifford, 1997). These episodes are offered as "
        "existence proofs rather than as the norm: the department could and did steer major outcomes through its control "
        "of forecasts and figures, without public announcement, across the heads of elected ministers. A milder version "
        "of that disposition, policy shaped by the fiscal centre's doctrine through instruments below the threshold of "
        "public debate, is all the mechanism of this paper requires.")

OBR = ("The Office for Budget Responsibility, created in 2010 as part of the post-crisis fiscal reforms, inherited the "
       "Treasury's forecasting functions and is frequently described as independent. The description warrants "
       "qualification. At creation the OBR had a professional staff of fewer than twenty, a scale at which its statutory "
       "duties are dischargeable only in close cooperation with the Treasury from which it was staffed (Berry and Berry, "
       "2013; Walsh, 2021), and it operates under the Charter for Budget Responsibility, a document the Treasury writes, "
       "which defines the very targets against which the OBR assesses the government. None of this is offered to "
       "disparage the OBR's forecasting; it is offered to establish continuity. The analytical machinery linking "
       "demography to fiscal sustainability existed within the Treasury and the Government Actuary's Department "
       "throughout the sample period. The OBR's published migration variants (OBR, 2014) are the publication of "
       "arithmetic the fiscal centre had long performed internally, and the empirical analysis accordingly treats the "
       "whole period 1979–2019, not merely its final decade, as governed by one informed fiscal centre.")

SAL2 = L1(47).replace("ended the Truss premiership within weeks.",
                      "ended the Truss premiership within weeks (Wamsley, 2026).")

DOC_LEAD = ("The claim that the fiscal centre read the projection machinery, and priced the visibility of its responses, "
            "can be tested against the written record at three points in the sample.")

ANNEX = ("The first statutory long-term projections, published as Annex A of the 1999 Economic and Fiscal Strategy "
         "Report (HM Treasury, 1999; " + S("4.2") + "), illustrate the instrument problem at the centre of this paper. "
         "Its section on long-term policy" + L1(77).split("Its section on long-term policy", 1)[1])

ID_LEAD = ("Three checks bear directly on the identification argument of " + S("5.2") + ", and are taken before the "
           "long-run and falsification results because the latter presuppose them.")

FALS_LEAD = ("Table 4 collects the falsification and robustness exercises. The two placebo tests carry the greatest "
             "evidential weight because they are tests the fiscal hypothesis could have failed.")

AGENDA = ("*Agenda.* Each limitation admits a remedy. The zero-migration reconstruction can be refined with the vintages' "
          "age-specific migration assumptions and the official variant projections published alongside later NPPs. The "
          "biennial NPP releases can be exploited as information shocks: the revision between successive vintages, "
          "decomposed into its mortality, fertility, and migration-assumption components from the published assumption "
          "sets, supplies news orthogonal to migration by construction, and Home Office quarterly visa grants by category "
          "provide an outcome with the frequency to support an event-study design around release dates, sharpening the "
          "news test of " + S("6.3") + ". The mechanism can be taken to the archives: Budget documents, Treasury fiscal-sustainability reports and "
          "the Treasury files already inside the thirty-year window (HM Treasury, 1997). A spending-side measure of the burden would test whether the tax term stands in for the bill it finances. "
          "External validity can be tested on UN projection vintages.")

HIST3 = L2(83).replace("Section 7 runs the comparison formally", S("6.6") + " ran the comparison formally")

NEWS = ("*News versus level.* Splitting the projection, in the twenty-one years a new vintage came into force, into what the "
        "previous vintage had reported and the revision the new one introduced gives 0.0089 (p < 0.001) on the inherited part "
        "and 0.0040 (p = 0.37) on the revision, not distinguishable from it (F(1, 34) = 1.11, p = 0.30): consistent with a "
        "response to news, imprecisely estimated, which the design of " + S("7") + " is for.")

# Compact version of part-1 line 75 for the 45-page variant: the GAD memorandum,
# the 1985-86 recollection and Hemming-Kay in one sentence each.
GAD75 = re.sub(
    r"Its own account to the Treasury Committee leaves no doubt.*?horizon left out\.",
    "GAD described itself to the Treasury Committee as \"created in 1919 as a Chancellor's department\", its objectives, "
    "spending plans and performance all set by or reported to Treasury ministers (House of Commons Treasury Committee, 2001); "
    "in the 1985–86 pension reform the Deputy Government Actuary recalls the Chancellor insisting that GAD advise from inside "
    "the Treasury (Pemberton, 2018); and Hemming and Kay (1982) showed the Actuary's 1978 pension memorandum, running only to "
    "2008–09, masked a cost that emerged beyond its horizon.",
    L1(75))
assert GAD75 != L1(75)

ROB67 = ("*Scaling, measurement, horizon, and sample.* The per-capita scaling is the analyst's decision, not the "
         "policymaker's; on gross non-EU inflows the "
         "projection coefficient is 0.0080 (HAC s.e. 0.0014), within a sixth of a standard error of its scaled "
         "counterpart, and in levels each further projected point is associated with roughly 1,300 to 1,400 additional "
         "gross arrivals a year (1,369, s.e. 307). Tax measures are tested in Online Appendix Tables B4 and B5 (per-capita measures interchangeable, GDP shares weaker; Mendoza, Razin and Tesar, 1994). On the LTIM series from 1991 the coefficient is larger and significant at 0.1 per cent (0.0159, s.e. 0.0040, N = 28). "
         "The projected level of the OADR in place of the projected change returns an almost identical coefficient (0.0082 against 0.0081, p < "
         "0.001), because the level is the current ratio plus the change and the current ratio contributes nothing "
         "once the change is present (−0.0001, p = 0.98; the equal-coefficient restriction is rejected, F(1, 34) = "
         "7.81, p = 0.008); across horizons the projection term is significant at three, five, seven, and ten years, "
         "in changes, levels, and weighted averages, with the ten-year delta fitting best, and the one-year change alone "
         "carries no signal (p = 0.57). Starting the sample in 1977 (the earliest possible), 1983 or 1987 "
         "returns 0.0082, 0.0087 and 0.0082, all significant at 0.1 per cent, and the result is invariant to the trend "
         "(Online Appendix Table B3) and to OLS, HAC, or Newey–West inference.")

SCOPE = ("One clarification of scope. The argument is a claim about incentives, not about coordinated concealment or "
         "intent beyond fiscal management. Each government faced the same constraint set: rising projected age-related "
         "spending, a politically expensive tax instrument, a constrained borrowing margin, and a migration instrument "
         "adjustable at near-zero parliamentary cost; the hypothesis is that each, independently, found the same margin "
         "cheapest, so that no coordination is required for the policy rule to appear stable. The distinction matters "
         "because the observation that migration rose without electoral mandate has been appropriated by conspiratorial "
         "narratives; the mechanism proposed here is ordinary public finance conducted through the instrument that "
         "ordinary politics made cheapest, and its evidential standing rests on tests it could have failed.")

IPS = ("The IPS is a voluntary interview survey conducted at ports, covering roughly 90 per cent of passenger traffic "
       "with a 68 per cent response rate among those approached (ONS, 2023). Its limitations are well documented: "
       "migration status is self-reported intention rather than realised duration, a student who remains after "
       "graduation on a work visa is recorded only under the status held at entry, respondents with reasons to misreport "
       "face no verification, and sampling variance is material for thinly travelled routes, which is why origins are "
       "published as regional groupings. The decennial Census cannot supply annual flows, but the ONS uses it to validate "
       "and where necessary adjust the intervening decade of IPS estimates, and the record of those reconciliations is "
       "the best available evidence on the survey's reliability.")

LIM = ("*Limitations.* Three are material. Identification: the zero-migration reconstruction of " + S("6.3") + " treats "
       "all net migrants as working-age, an approximation the vintages' age-specific assumptions would refine. "
       "Aggregation: the dependent variable pools work, study, and family entry, so the model's sharper implication, "
       "that the response should concentrate where discretion and youth are greatest, is testable only with "
       "administrative data. Mechanism: outside the 2000–2001 episode of " + S("2.4") + " the "
       "evidence remains institutional and statistical.")

CONC2 = ("Beyond its substantive claim, the paper contributes a reusable object and a reusable frame: the reconstructed "
         "vintage series of official British ageing projections from 1975 onward, against which any hypothesis about the "
         "policy influence of demographic expectations can now be tested; and the treatment of policy instruments as "
         "priced by institutional salience, which also predicts the preference for legislating pension-age increases "
         "decades before they bind.")

CONC3 = ("The interpretation is disciplined by what the design cannot yet deliver: the projections are the government's "
         "own and embed migration assumptions; the documentary record covers a single episode; and the sample is one country and forty-one years.")

MODEL_ABS = ("The model abstracts from much: migrants are homogeneous fiscal contributors rather than heterogeneous "
             "workers, wages and prices do not respond to inflows, salience weights are exogenous rather than shaped by "
             "media and opposition strategy, and the projection is taken as the government's belief rather than modelled "
             "as a signal-extraction problem. Each abstraction isolates the mechanism under test and marks a boundary the "
             "conclusions respect: the estimates identify a policy response to projected ageing, not its welfare "
             "consequences or general-equilibrium incidence.")

MARGIN2 = ("Two further features bear on interpretation. The state's influence over non-EU inflows extends beyond "
           "rule-setting: the National Health Service, the country's largest employer, recruits internationally at "
           "scale, and non-British nationals, three-quarters of them from outside the EU, constitute roughly a fifth of "
           "its workforce (House of Commons Library, 2025). And the dependent variable aggregates all long-term non-EU "
           "inflows, including students and family migration, deliberately: from a fiscal standpoint what matters is "
           "the addition of working-age residents, whatever the entry category, and every category's entry criteria "
           "were policy-controlled. Students, the category most often queried, enter with work rights, tuition payments, "
           "and consumption spending, are the youngest entrants, and the official estimates of the period recorded their "
           "departures well below arrivals. What aggregation forecloses is only the estimation of category-level "
           "responses, taken up in " + S("7") + ".")

INTRO3 = L1(7).replace("This possibility is strengthened with an understanding of the OBR's history, elaborated upon in "
                       "Section 2.1. In short, the authority inherited many features from its predecessor in the Treasury, "
                       "including arguably an institutionalised ideology with far reach in the British state.",
                       "Section 2.1 shows that the OBR inherited this arithmetic, and much else, from the Treasury.")

EST = ("The choice of estimator deserves brief defence. An instrumental-variables design would be preferable in "
       "principle, but no valid instrument for UK migration exists (Section 4.4); difference-in-differences and "
       "regression-discontinuity designs are unavailable for want of a control group or an assignment threshold, since "
       "the regime applied to the whole non-EU margin at once and changed continuously; and a cross-country panel would "
       "trade the UK's institutional depth for breadth the hypothesis does not yet need (" + S("7") + "). Within the "
       "single-country time-series setting the ARDL framework is the standard recommended for UK migration data "
       "(Mitchell and Pain, 2003): it accommodates the persistence of flows, nests the static model, supports bounds "
       "testing for level relationships among I(0)/I(1) variables, and delivers long-run multipliers with structural "
       "content under the model of Section 3.")

RECON = L1(160).split(" Multi-vintage records of these projections have been assembled before")[0] + (
        " Multi-vintage records of these projections have been assembled before, but from within the statistical "
        "service that produced them (Shaw, 2007). To my knowledge this is the first horizon-complete, machine-readable panel of projected dependency ratios by "
        "vintage assembled from public sources, and the first use of the projection vintages as an information-set "
        "regressor in a model of policy; outside government the pre-1998 record was effectively unobtainable until the "
        "retrieval route documented here.")

BOOM = ("The demographic history the projections track gives them their forecasting power. Britain's post-war baby boom "
        "arrived in two waves, an immediate post-war surge and a second, larger wave over roughly 1955–65, and it was "
        "already the consensus among demographers at the start of the sample that fertility would not return to those "
        "levels (Jones, 1980). Each wave reaches pension age on a schedule known decades ahead, the first from around "
        "2010 and the second through the 2020s. This is why old-age dependency is, uniquely among fiscal pressures, "
        "forecastable: the people who will be pensioners in ten years are alive, resident, and counted today, so at that "
        "horizon the projection error reduces essentially to mortality surprises. A low dependency ratio is a loan rather "
        "than a gift; the demographic dividend (Farid and Mostari, 2022) is repaid, with interest, when the large cohorts "
        "retire, and the projections priced that repayment for any government that cared to look.")

TVIEW = L1(35).replace("had already been associated with the department for decades.",
                       "had already been associated with the department for decades (Institute for Government, 2024a).")

# ------------------------------------------------------------- assembly ----
part1 = [
 "# 1. Introduction", L1(3), L1(5), INTRO3, L1(9), L1(11), L1(13), L1(15), L1(17), L1(19), SCOPE, L1(23), ROADMAP,
 "# 2. Institutional Background",
 "## 2.1 The Treasury as fiscal centre", L1(31), L1(33), TVIEW, L1(37), DELL, OBR,
 "## 2.2 The salience asymmetry: Budget Day and the Immigration Rules", L1(45), SAL2, L1(49), L1(51),
 "## 2.3 Control of the non-EU margin, 1979–2019", L1(55), L1(57), L1(59), L1(61), L1(63), MARGIN2, L1(67),
 "## 2.4 What the fiscal centre wrote down", DOC_LEAD, L1(87), ANNEX, L1(81), L1(83), L1(85),
 "# 3. A Stylised Model of Fiscal Instrument Choice",
 L1(98), L1(100), EQ1(102, 104), L1(106), L1(108), EQ1(110, 112), L1(114), L1(116), L1(118), EQ1(120, 122),
 L1(124), EQ1(126, 128), L1(130), L1(132), MODEL_ABS, L1(136),
 "# 4. Data", L1(140), "[[TABLE1]]",
 "## 4.1 Dependent variable: non-EU inflows", L1(146), IPS, L1(152), L1(154),
 "## 4.2 The projection series", L1(71), BOOM, GAD75, L1(158), RECON, L1(162),
 "## 4.3 Fiscal and macroeconomic controls", L1(166), L1(168), L1(170), L1(172), L1(174),
 "## 4.4 The absence of an instrument", L1(178),
]
part2 = [
 "# 5. Empirical Strategy",
 "## 5.1 Specification", L2(5), EQ2(7, 9), L2(11), EST, L2(15),
 "## 5.2 Identification: timing, threats, and remedies", L2(19), L2(21), L2(23), L2(25), L2(27), L2(29), L2(31),
 "## 5.3 Diagnostics and specification protocol", L2(35), L2(37), L2(39),
 "# 6. Empirical Analysis",
 "## 6.1 Specification development", L2(45), L2(47), L2(49),
 "## 6.2 Main estimates", L2(53), "[[TABLE2]]", L2(57), L2(59), L2(61), L2(63),
 "## 6.3 Identification checks", ID_LEAD, L2(91), L2(93), NEWS, L2(97),
 "## 6.4 Cointegration and the long run", L2(67), L2(69), "[[TABLE3]]", L2(73),
 "## 6.5 Falsification: the fiscally cheap ratio and the uncontrolled margin", FALS_LEAD, "[[TABLE4]]", L2(99), L2(101), "[[FIGURE2]]",
 "## 6.6 The policy-era alternative", re.sub(r"^\*[^*]+\*\s*", "", L2(105)),   # drop run-in label under its own heading
 "## 6.7 Further robustness", ROB67, L2(117),
 "## 6.8 Reading the history through the model", "[[FIGURE1]]", L2(79), L2(81), HIST3,
 "# 7. Discussion: External Events, Recognition, and Limitations", L2(121), L2(107), LIM, AGENDA,
 "# 8. Conclusion", L2(129), L2(135),   # CONC3 dropped for length (limitations stated in Section 7)
 "[[TITLENOTE]]", "[[REFS]]",
] + [l.rstrip() for l in P2[[l.strip() for l in P2].index('[[REFS]]') + 1:] if l.strip()]   # reference list follows the marker

# --------------------------------------------------- cross-reference map ----
def remap(text):
    # context-specific old "Section 7" (robustness) -> new subsections, protected
    spec = [
        ("Section 7 shows the estimates are unchanged when the projections are rebuilt", S("6.3") + " shows the estimates are unchanged when the projections are rebuilt"),
        ("falsification tests of Section 7", "falsification tests of " + S("6.5")),
        ("placebo test implemented in Section 7", "placebo test implemented in " + S("6.5")),
        ("Section 7 shows the results are insensitive to the start date", S("6.7") + " shows the results are insensitive to the start date"),
        ("Section 7 re-estimates on that basis", S("6.3") + " re-estimates on that basis"),
        ("net-migration assumptions, Section 7 rebuilds", "net-migration assumptions, " + S("6.3") + " rebuilds"),
        ("placebo margins of Section 7", "placebo margins of " + S("6.5")),
        ("LTIM check (Section 7)", "LTIM check (" + S("6.7") + ")"),
        ("Section 7 shows it does neither", S("6.5") + " shows it does neither"),
        (" attained significance in any variant, unsurprising for output per worker given the UK's prolonged productivity stagnation, and consistent with a policy rule oriented to the extensive rather than the intensive margin.", " attained significance in any variant."),
        (" The Budget-time growth forecast was significant at the five per cent level in some variants, but only at the cost of destabilising the remaining coefficients, and was excluded. The inactivity rate hovered at the margin of significance, was nearly time-invariant over the sample, and was dropped when degrees of freedom required it.", " The Budget-time growth forecast destabilised the remaining coefficients and the inactivity rate was nearly time-invariant; both were excluded."),
        (" The demographic arithmetic, meanwhile, is evidently understood well beyond the Treasury.", ""),
        (" The zero-migration reconstruction can be refined with the vintages' age-specific migration assumptions and the official variant projections published alongside later NPPs.", ""),
        (" (all of which the estimation sample requires: the 1975-based is in force for publication-dated 1977)", ""),
        ("sample start dates of 1979, 1983 and 1987 (Section 7)", "sample start dates of 1979, 1983 and 1987 (" + S("6.7") + ")"),
        ("zero-migration reconstruction of Section 7", "zero-migration reconstruction of " + S("6.3")),
        ("noted in Section 8 as a natural extension", "noted in " + S("7") + " as a natural extension"),
        # old 2.4 that stays 2.4: the 1999 annex now lives in new 2.4
        ("reducing long-term spending pressures (Section 2.4)", "reducing long-term spending pressures (" + S("2.4") + ")"),
    ]
    for a, b in spec:
        text = text.replace(a, b)
    gen = [
        (r"Section 2\.5\b", "Section §§2.4§§"),
        (r"Section 2\.4\b", "Section §§4.2§§"),
        (r"Section 6\.4\b", "Section §§6.8§§"),
        (r"Section 6\.3\b", "Section §§6.4§§"),
        (r"Section 8\b",    "Section §§7§§"),
        (r"Section 9\b",    "Section §§8§§"),
    ]
    for a, b in gen:
        text = re.sub(a, b, text)
    return text.replace("§§", "")

from period_1977 import period1977
t1 = period1977(remap("\n\n".join(part1))) + "\n"
t2 = period1977(remap("\n\n".join(part2))) + "\n"
R45_TRIMS = [   # Sam, 22 Sep 2026: room for the VIF paragraph (cut 4, abbreviate 1-3)
    (" The counterfactual is worth stating plainly: had the 1997 election gone the other way, the model expects the "
     "subsequent expansion to have occurred in any case, in different rhetoric.",
     " Had the 1997 election gone the other way, the model expects the expansion regardless, in different rhetoric."),
    (" Part of that rise is mechanical rebalancing across origins under equalised entry conditions, one reason no "
     "structural claim is made on five years of post-regime-change data.",
     " Part of that rise is mechanical rebalancing across origins, so no structural claim rests on it."),
    (", and Home Office quarterly visa grants by category provide an outcome with the frequency to support an "
     "event-study design around release dates, sharpening the news test of Section 6.3.",
     ", and quarterly visa grants by category would support an event study around release dates."),
    (" Advocacy without the instrument carries no price for the inflows themselves, so visibility of the demand",
     " Visibility of the demand"),
]
for _a, _b in R45_TRIMS:
    assert t2.count(_a) == 1, _a[:60]
    t2 = t2.replace(_a, _b)
io.open("manuscript_r45_part1.md", "w", encoding="utf-8").write(t1)
io.open("manuscript_r45_part2.md", "w", encoding="utf-8").write(t2)
body = t1 + t2.split("[[REFS]]")[0]
print("r45 words (body, excl. refs):", len(body.split()))
# unmapped old-robustness references would read "Section 7" followed by robustness verbs
print("suspicious 'Section 7' contexts:",
      re.findall(r"Section 7 (?:shows|re-estimates|rebuilds|runs)[^.]{0,40}", body) or "none")
print("stale refs:", re.findall(r"Section (?:9|2\.5|2\.6)\b", body) or "none")
