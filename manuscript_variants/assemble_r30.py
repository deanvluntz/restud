# -*- coding: utf-8 -*-
"""
Assemble the 30-page ReStud-structured manuscript. Same section architecture
as the 45-page version (see assemble_r45.py); most paragraphs are compressed
rewrites, every estimate and every in-text citation of the original retained
unless noted in the disclosure. Robustness beyond the nested baseline, the
placebos and the policy-era test is summarised and pointed to the Online
Appendix. New section references use  Section §§n§§  (see assemble_r45.py).
"""
import io, re

P1 = io.open("manuscript_part1_variantsrc.md", encoding="utf-8").read().split("\n")
P2 = io.open("manuscript_part2_variantsrc.md", encoding="utf-8").read().split("\n")
def L1(n): return P1[n - 1].rstrip()
def L2(n): return P2[n - 1].rstrip()
def EQ1(a, b): return "\n".join(P1[a - 1:b])
def EQ2(a, b): return "\n".join(P2[a - 1:b])
S = lambda n: "Section §§" + n + "§§"

# ============================================================ 1. INTRODUCTION
I1 = L1(3)
I2 = L1(5)
I3 = ("This paper proposes and tests a fiscal explanation, motivated by the framework of the Office for Budget "
      "Responsibility (OBR). The fiscal watchdog's long-term projections run scenarios on alternative assumptions about "
      "net migration, and larger inflows are associated with greater fiscal sustainability because migrants are "
      "concentrated in working ages and so raise the ratio of taxpayers to dependents: the 2014 exercise projected that "
      "roughly doubling assumed net migration, from 105,000 to 225,000 a year, would leave public sector net debt in "
      "2063–64 at 41 per cent of GDP rather than 83 per cent (OBR, 2014). If past governments internalised this "
      "arithmetic, and if raising migration was politically cheaper than raising taxes or cutting age-related spending, "
      "then official demographic forecasts should predict the migration flows that policy controls. Section 2.1 shows "
      "that the OBR inherited the arithmetic, and much else, from the Treasury.")
I4 = ("The fiscal logic has a second premise, without which arithmetic alone explains nothing: the instrument must be "
      "politically cheaper than its alternatives. Raising taxes or cutting age-related spending requires announcement in "
      "the Budget, the most scrutinised event in the British political calendar, legislated through a Finance Bill whose "
      "defeat is treated as a matter of confidence. Migration rules, by contrast, are amended by statutory instrument, "
      "without debate or vote. A government confronting a projected deterioration in the ratio of pensioners to workers "
      "therefore faces three margins of adjustment priced very differently by its institutions, and the cheapest is the "
      "quietest.")
I5 = L1(11)
I6 = ("The paper makes three contributions. First, it assembles a new annual series of official demographic forecasts: "
      "the projected old-age dependency ratio at horizons of one to ten years, reconstructed from every National "
      "Population Projection vintage from the 1977-based to the 2018-based, most of which survive only in un-indexed "
      "scans on the National Archives. No previous study has compiled the UK's projection vintages into a usable time "
      "series, and none for any country has tested whether such projections predict policy-controlled migration. Second, "
      "it provides a political-economy rationale for the reduced form: a stylised model in which a fiscally dominant "
      "finance ministry allocates adjustment across taxes, borrowing, and controlled migration in proportion to their "
      "political salience, delivering the estimating equation's signs as ex ante predictions. Third, it offers a "
      "documented alternative to the leading explanation of the expansion. Mitchell, Pain and Riley (2011) attribute "
      "around three-quarters of the rise in net immigration in the decade to 2009 to structural change proxied by dummy "
      "variables and attributed to the post-1997 Labour government; I show that non-EU inflows track the projected "
      "ten-year deterioration in the OADR across the whole of 1979–2019, including under the Conservative "
      "administrations of the 1980s and 1990s, which a Blair-specific account cannot explain.")
I7 = ("The empirical framework is an autoregressive distributed lag (ARDL) regression of log non-EU inflows, scaled by "
      "the lagged working-age population, on the projected ten-year change in the OADR and a parsimonious set of fiscal "
      "state variables: the lagged tax take per working-age person, the lagged change in the debt-to-GDP ratio, and the "
      "lagged unemployment rate. A one-point increase in the projected ten-year OADR deterioration is associated with "
      "approximately 0.8 per cent higher non-EU inflows on impact and, once the finite-sample attenuation of the "
      "autoregressive coefficient is corrected, approximately 2.0 per cent in the long run (1.6 per cent uncorrected). "
      "The coefficient is stable across horizons, functional forms, sample start dates, and measurement bases. Two "
      "falsification exercises support a fiscal-policy reading: projections of the *total* dependency ratio, whose child "
      "component is fiscally cheap, have no predictive power, and the same specification estimated on *EU* inflows, "
      "which policy could not ration, yields an OADR coefficient indistinguishable from zero.")
I8 = ("The paper is related to three literatures. In the canonical selection framework migration responds to income "
      "differentials net of moving costs (Borjas, 1987); Hatton and Williamson (2002) add that networks propagate flows, "
      "that source-country youth shares raise out-migration, and that destination-country *policy* is the binding "
      "constraint most quantitative models omit, while Mayda (2007) finds that restrictions make destination conditions "
      "dominate origin push factors, precisely the environment in which the determinants of the policy become the "
      "determinants of the flow. Storesletten (2000) establishes the fiscal arithmetic this paper takes as premise, that "
      "immigration of working-age, sufficiently skilled entrants can in principle sustain fiscal policy through ageing, "
      "and the United Nations (2000) bounds it from the other side, finding the inflows needed to hold support ratios "
      "constant implausibly large: a demonstration of feasibility and of its limit, not a test of use. Whether ageing "
      "destinations pull migrants through deliberate policy has not been tested; that inverse is this paper's subject. "
      "Within the UK branch, Mitchell and Pain (2003) advocate the ARDL approach adopted here and document the "
      "persistence of flows; Hatton (2005) finds unemployment matters in changes rather than levels; Mitchell, Pain and "
      "Riley (2011) supply the structural-change account against which Section 6.6 races the fiscal one. The paper also "
      "draws on the institutional literature on British fiscal governance (Heclo and Wildavsky, 1974; Hood, King, McLean "
      "and Piotrowska, 2023) to specify the Treasury's information set, on critiques of the OBR's independence (Berry and "
      "Berry, 2013; Walsh, 2021) to argue that the projection-to-policy channel predates 2010, and on the political "
      "economy of instrument choice, of which the mechanism is an application in a fiscal setting.")
I9 = ("The argument is a claim about incentives, not about coordinated concealment: each government faced the same "
      "constraint set and, the hypothesis holds, independently found the same margin cheapest, so no coordination is "
      "required for the policy rule to appear stable. The mechanism is ordinary public finance conducted through the "
      "instrument ordinary politics made cheapest, and its evidential standing rests on tests it could have failed. "
      "Three limitations are stated at the outset and addressed as far as the data allow: the projections embed "
      "migration assumptions, so " + S("6.3") + " re-estimates on projections rebuilt under zero net migration; the "
      "sample is short (41 observations), which constrains the specification and the power of time-series tests; and "
      "outside one documented episode (" + S("2.4") + ") the evidence for the mechanism is institutional and "
      "statistical rather than documentary, for reasons the instrument-choice logic itself explains.")
ROADMAP = ("Section 2 sets out the institutional background, Section 3 the model, Section 4 the data, and Section 5 the "
           "empirical strategy. Section 6 presents the estimates, identification checks, long-run relationship, "
           "falsification tests, and the horse race against the structural-change account; " + S("7") + " discusses "
           "external events and limitations, and " + S("8") + " concludes.")

# ====================================================== 2. INSTITUTIONAL BACKGROUND
T1 = ("The mechanism rests on two institutional claims: that a single actor, His Majesty's Treasury, has both the "
      "incentive and the authority to treat migration as an instrument of fiscal policy, and that the instrument could "
      "be adjusted at low political cost. The Treasury centralises powers that other governments distribute across "
      "several ministries: budgeting, tax policy, macroeconomic policy, and the financing of the state, the last "
      "“perhaps the one aspect of twenty-first-century government that a medieval monarch would recognise” "
      "(Hood et al., 2023). The Institute for Government describes its power as exceeding “that of any other part of "
      "government apart from 10 Downing Street,” with a “pervasive” veto and a unique capacity “to "
      "initiate and force through policies” (Institute for Government, 2024b).")
T2 = ("Two features of this authority matter. The first is continuity of preferences across elected governments. The "
      "department maintains a stable doctrine, the “Treasury view”, a phrase Churchill coined as Chancellor "
      "against Keynesian proposals (Hansard, 1929) but describing a fiscal conservatism traceable to Gladstone, who "
      "embedded Treasury officials in every department to police expenditure; decades later the department was still "
      "“imbued with a dreadful spirit of economy” (Higgs, 1924). Heclo and Wildavsky (1974), interviewing "
      "officials in the early 1970s, and Hood et al. (2023), fifty years on, describe substantially the same institution, "
      "whose business is to “save money, not to spend it” and whose chief interest is “to save themselves "
      "the unpopularity of having to put up taxes.” That is the behavioural assumption of the model below, stated by "
      "the institution's own members: fiscal adequacy at minimum political cost, with instruments ranked by that cost. "
      "The same study establishes that annual budgeting took the previous year's estimates as baseline, incorporated "
      "internal forecasts of growth, inflation, and employment, and examined proposals over five-year horizons, facts "
      "that anchor the information set of Section 4. This continuity is what permits a single policy rule to be "
      "estimated over forty-one years and seven Prime Ministers.")
T3 = ("The second feature is the department's willingness to pursue its doctrine independently of, and occasionally "
      "against, the elected government. The clearest case is the IMF loan of 1976, negotiated by the Treasury while the "
      "Labour Cabinet was split, on terms of austerity justified by debt figures later shown to have been grossly "
      "overstated in the documents submitted to the Fund; Edmund Dell, then Paymaster General, defended the "
      "exaggeration in print as necessary to secure cuts the department believed essential, against the platform on "
      "which the government had been elected (Dell, 1991; López, 2014). An earlier attempt to divide the "
      "department's authority, the Department of Economic Affairs of 1964, ended with the Treasury re-absorbing its rival "
      "within five years (Clifford, 1997). These are existence proofs, not the norm; a milder version of the disposition, "
      "policy shaped by the fiscal centre through instruments below the threshold of public debate, is all the mechanism "
      "requires. The Office for Budget Responsibility, created in 2010, inherited the Treasury's forecasting functions "
      "with a staff of fewer than twenty, dischargeable only in close cooperation with the department from which it was "
      "drawn, under a Charter the Treasury writes (Berry and Berry, 2013; Walsh, 2021). The point is continuity: the "
      "machinery linking demography to fiscal sustainability existed within the Treasury and the Government Actuary's "
      "Department throughout the sample, and the OBR's migration variants (OBR, 2014) publish arithmetic the fiscal "
      "centre had long performed internally.")

SA1 = ("The Treasury's annual programme is announced in the Budget, whose salience is constitutive rather than "
       "incidental: the event exists to concentrate accountability for fiscal choices at one maximally visible moment "
       "(UK Parliament, 2026). Its measures are trailed for weeks, delivered to a full House, dissected within hours by "
       "the opposition, the Institute for Fiscal Studies, the press and, since 2010, the OBR, and enacted through a "
       "Finance Bill whose defeat is by convention a matter of confidence. Heclo and Wildavsky (1974) describe Budget "
       "Days as “great political occasions” on which “the pressures on a Chancellor are intense”; "
       "the 2022 fiscal statement delivered without an OBR assessment repriced government debt within days and ended the "
       "Truss premiership within weeks (Wamsley, 2026). The tax and spending responses to a deteriorating dependency "
       "ratio must be announced by the Chancellor, on that stage, and voted on by his colleagues.")
SA2 = ("Migration policy faces no comparable exposure. The Immigration Act 1971 delegated the detail of entry policy to "
       "the Immigration Rules, which the Home Secretary amends by laying a Statement of Changes before Parliament: "
       "secondary legislation requiring no debate, no vote, and no Commons majority, laid several times a year (Home "
       "Office, 2026). The shift of British law-making from primary to secondary legislation accelerated over exactly "
       "this period: the annual count of statutory instruments held near two thousand through the 1970s and 1980s, "
       "stepped up sharply from around 1990 and stayed above three thousand for two decades, while Acts of Parliament "
       "roughly halved (House of Commons Library, 2019; Hansard Society, 2014). The escalation belongs to no single "
       "administration and runs alongside the migration expansion: the quiet instrument became progressively cheaper to "
       "use over the decades in which the fiscal pressure modelled here was building.")
SA3 = ("Within the executive, coordination between the fiscal centre and the Home Office requires no public act, because "
       "Westminster's rejection of the separation of powers integrates the two. The Cabinet is drawn from the "
       "legislature and bound by collective responsibility; the Treasury sets every department's budget, the Home "
       "Office's included; and the Chancellor is by convention the second figure of the government. A departmental "
       "minister weighing a Treasury preference on entry policy therefore faces the department that funds her, a "
       "doctrine that forecloses public disagreement, and a rule-making instrument that requires no parliamentary test "
       "of the outcome. The claim is not that migration targets were written into Budget documents, which Section 3 "
       "explains they would not be, but that the machinery for using migration as a fiscal instrument existed, was "
       "centralised, and operated below the threshold of scrutiny throughout the sample.")

CM1 = ("Work permits regulating entry from outside the Empire date from the aftermath of the First World War (Bauer and "
       "Salt, 2020); imperial subjects held a nominal right of entry policed at the point of departure, so that it was "
       "exercised principally by the settler dominions (Hatton and Wheatley Price, 1999). Decolonisation made the right "
       "real, successive Commonwealth Immigrants Acts restricted it, and the Immigration Act 1971 ended Commonwealth free "
       "movement two years before EEC accession created its European replacement, while creating the delegated "
       "rule-making machinery through which all subsequent non-European entry policy has been made. Throughout the "
       "sample the United Kingdom could not restrict inflows from other member states; control was exercised "
       "exclusively over the non-EU margin, through two successive regimes.")
CM2 = ("Under the work-permit system, in force until 2008, a non-EU national required a job offer from an employer who "
       "had to demonstrate, under the Resident Labour Market Test, that the vacancy could not be filled domestically, and "
       "a separate entry visa could be refused even after a permit was granted. By the mid-2000s more than eighty routes "
       "existed for work, study, or training (Home Office, 2006, p. 7), and the Rules have since grown to some 375,000 "
       "words (Guardian, 2018). Absent published quotas, the state's dual approval gave it continuous discretionary "
       "control over volumes through administrative practice, and it dictates the measurement strategy: a Statement of "
       "Changes may loosen some routes while tightening others, historical permit data are too fragmentary for "
       "statistical use (Bauer and Salt, 2020), and no stringency series spans the period, so policy is measured by its "
       "outcomes on the controlled margin under the maintained assumption that the instrument was effective. That the "
       "lever moved with demography, and was operated quietly, is visible at the administrative level: in 1990 the "
       "Employment Department's Overseas Labour Section recorded that “because of demographic changes the number of "
       "work permits being granted is rising sharply”, with approval rates near 75 per cent, while asking that "
       "“actual figures” be “omitted as they can be misleading” (The National Archives, LAB 109/66).")
CM3 = ("The points-based system introduced from 2008 consolidated the routes into five tiers, allocating entry by points "
       "for salary, qualifications, language and, notably, age: applicants aged 27 or under received 20 points toward the "
       "threshold, the credit declining to 5 points and cutting off at 31 (Home Office, 2006), the closest the regime "
       "comes to a published statement that the age structure of inflows was a policy objective. The system imposed no "
       "aggregate cap, and by removing case-by-case approval it shifted apparent responsibility for volumes from the "
       "government to the labour market, a visibility-reducing design with a family resemblance to the delegation of "
       "monetary policy a decade earlier. Two further features bear on interpretation. The National Health Service, the "
       "country's largest employer, recruits internationally at scale, and non-British nationals, three-quarters from "
       "outside the EU, form roughly a fifth of its workforce (House of Commons Library, 2025). And the dependent "
       "variable aggregates all long-term non-EU inflows, work, study, and family, deliberately: every category's entry "
       "criteria were policy-controlled and every category adds working-age residents; students in particular enter with "
       "work rights and spending power, are the youngest entrants, and recorded departures well below arrivals. The "
       "post-2019 extension of the points-based system to EU nationals (Migration Observatory, 2021) falls outside the "
       "sample and is used in " + S("7") + " as an out-of-sample check.")

DR1 = ("The fiscal centre's reading of the projection machinery, and its sensitivity to the visibility of its responses, "
       "can be tested against the written record at three points. The Treasury's 1984 Green Paper on long-term "
       "expenditure tabulated the official projections and costed the pensioner numbers they implied, naming demography "
       "first among the pressures on future spending (HM Treasury, 1984); the Cabinet Office brief on its drafting warned "
       "of the 'political risks' of illustrating that arithmetic in public; and by November 1996 the Treasury's board had "
       "been briefed that the old-age dependency ratio would nearly double over fifty years (The National Archives, "
       "PREM 19/1190; T 733). An OECD note circulated to Downing Street in 1984 had already remarked on governments' "
       "'expanding use of off-budget measures' whose true costs, 'though less apparent, are not necessarily less', the "
       "selection principle Section 3 formalises.")
DR2 = ("The first statutory long-term projections, Annex A of the 1999 Economic and Fiscal Strategy Report (HM Treasury, "
       "1999; " + S("4.2") + "), name the responses the government was prepared to advertise: productivity, "
       "participation, welfare reform, private saving, and first among them the legislated rise in the female state "
       "pension age from 60 to 65, which “will play a key role in reducing long-term spending pressures” (p. "
       "90). Migration appears nowhere among the remedies, only once among the uncertainties. The published record thus "
       "confirms both that the fiscal centre was analysing ageing at Budget time on a ten-year-plus horizon and the "
       "salience ranking of the model: the pension-age instrument, legislated fifteen years before it began to bind, is "
       "named and claimed; the instrument adjustable by statutory instrument is not.")
DR3 = ("For the turn-of-the-millennium expansion the mechanism can be documented directly, in a drafting record released "
       "under the Freedom of Information Act (Cabinet Office, 2010). In 2000 the Cabinet Office Performance and "
       "Innovation Unit and the Home Office prepared the government's first comprehensive economic analysis of "
       "migration, published in January 2001 (Glover et al., 2001). The preliminary draft of July 2000, classified "
       "RESTRICTED–POLICY, reasons in the terms of Section 3: without net migration the working-age population would "
       "fall by two million over twenty-five years while the over-65 population rose by three million; net migration of "
       "roughly 120,000 a year would hold the working-age population constant and nearly a million a year would stabilise "
       "the support ratio; ageing “will have to be addressed: presumably by some combination of net migration, an "
       "increase in the average working life, changes to entitlements, increases in contributions (either tax-based or "
       "private), and increases in productivity,” since “the alternative to migration may often be tax "
       "increases”; and these pressures would recast immigrants “less like a threat and more as a way to avoid "
       "the more unpleasant alternatives of tax increases, benefit cuts, and economic stagnation.” None of this "
       "survived to publication: the quantified requirements, the instrument menu, the comparison with taxation, and the "
       "“unpleasant alternatives” passage are all absent, and the November 2000 draft shows the excision had "
       "occurred by the autumn in which ministers announced the “managed migration” reframing. The episode "
       "documents both halves of the model, the reasoning and its removal; its limits are that it covers one episode "
       "under one government, and that the drafting hands are the Cabinet Office's and the Home Office's rather than the "
       "Treasury's own, whose parallel paper trail remains to be recovered (" + S("7") + ").")

# ================================================================ 3. MODEL
M1 = ("This section sets out a minimal model whose purpose is to discipline the specification: it delivers the sign and "
      "the conditioning set of the estimating equation as ex ante predictions.")
M2 = L1(100)
M3 = L1(106)
M4 = ("Each instrument carries a political cost proportional to its salience: tax changes are maximally salient, "
      "borrowing is constrained by bond markets and the Treasury's doctrinal aversion to debt, and migration adjustments "
      "are minimally salient, proceeding by statutory instrument. Let the period political cost be")
M5 = ("where sτ ≫ sₘ > 0 are salience weights, τ̄ is the inherited tax burden regarded as "
      "politically neutral, uₜ₋₁ is the unemployment rate observed when policy is set, since visible "
      "inflows during visible joblessness are the one circumstance in which the migration instrument's salience rises "
      "sharply, and the final term is an adjustment cost reflecting the administrative and diplomatic frictions of "
      "changing entry rules quickly. That inflows carry an electoral price at all, so that sₘ is positive and the "
      "instrument quiet rather than free, is well established: exploiting quasi-random refugee allocation across Danish "
      "municipalities, Dustmann, Vasiljeva and Piil Damm (2019) find that local exposure shifts votes toward "
      "anti-immigration parties outside the largest cities. The government minimises expected discounted political cost "
      "subject to the budget identity, a debt limit bₜ ≤ b̄(Bₜ₋₁) that tightens in the "
      "inherited debt stock, and the demographic transition law under which migration today lowers the dependency ratio "
      "at the projection horizon. The salience ranking is Section 2.2 in symbols; the borrowing constraint, tightening in "
      "the inherited stock, is why lagged debt rather than the flow of borrowing enters the rule.")
M6 = L1(118)
M7 = ("equating the marginal political cost of admissions to their marginal fiscal benefit, where λₜ, the shadow "
      "value of fiscal room, rises with the inherited tax burden and with the tightness of the borrowing constraint, and "
      "MB(·) rises with the projected deterioration the inflow offsets. Linearising around the inherited position "
      "yields a policy rule of the form")
M8 = L1(130)
M9 = ("The framework organises fiscal capacity along two margins: on the *intensive* margin the government extracts more "
      "from the existing working-age population, which is why unemployment and inactivity belong in the information "
      "set; on the *extensive* margin it enlarges the base, by migration or by raising the state pension age, which "
      "converts prospective dependents into prospective taxpayers by statute. The sample contains one large realisation "
      "of the second instrument, the Pensions Act 1995, which equalised the female pension age over 2010–2019, enacted "
      "fifteen years before it began to bind, a timing profile itself consistent with salience management, and named in "
      "the 1999 Budget as a key means of reducing long-term spending pressures (" + S("2.4") + "). Because the "
      "projections used here incorporate legislated pension-age changes, the estimated migration response is identified "
      "net of this alternative channel.")
M10 = ("The model abstracts from heterogeneity among migrants, from price and wage responses, from the endogeneity of "
       "salience to media and opposition strategy, and from signal extraction; each abstraction isolates the mechanism "
       "and bounds the conclusions, which concern a policy response to projected ageing and not its welfare consequences. "
       "Two predictions connect it to the institutions. It predicts silence: because the instrument's value derives from "
       "its low salience, no optimising government publicises its use, so the mechanism must be detected in outcomes "
       "rather than announcements, which is why the falsification tests of " + S("6.5") + ", which the reduced form "
       "*can* fail, carry the evidential weight, and which is borne out literally in the excised 2000 draft. And it "
       "predicts which projection should matter: the old-age ratio, whose fiscal price is high, not the total ratio, "
       "whose child component is cheap, yielding the placebo of " + S("6.5") + ".")

# ================================================================= 4. DATA
D0 = ("The estimation sample is annual, 1979–2019 (41 observations): 1979 opens a period of continuity in the governing "
      "policy consensus, is the first year in which net migration turned positive, and excludes the turbulence of the "
      "mid-1970s; " + S("6.7") + " shows the results are insensitive to the start date. Table 1 summarises definitions "
      "and sources.")
DV1 = ("Annual gross inflows of long-term migrants (intended stay of a year or more) by country of last residence are "
       "taken from the International Passenger Survey (IPS), conducted at ports since 1961. The dependent variable is the "
       "log of non-EU inflows per thousand working-age residents, with the denominator lagged one year so that "
       "current-year inflows cannot enter both sides of the equation.")
DV2 = ("The IPS is a voluntary interview survey covering roughly 90 per cent of passenger traffic with a 68 per cent "
       "response rate (ONS, 2023); migration status is self-reported intention rather than realised duration, entrants "
       "are recorded under the status held at entry, and sampling variance is material on thin routes. It is used "
       "nonetheless for three reasons. It was the basis of the official estimates the government itself used throughout "
       "the period (Portes and Forte, 2017), so it measures the flows as policymakers perceived them, and for the same "
       "reason the series is used as published contemporaneously rather than as later revised. The adjusted Long-Term "
       "International Migration series, which the ONS cautions cannot simply be compared with the raw IPS (ONS, 2015), "
       "begins only in 1991 and serves as a robustness check. And the decennial Census, against which the ONS benchmarks "
       "the IPS, has contradicted it materially only once, on post-2004 EU8 arrivals, a discrepancy concerning EU rather "
       "than non-EU flows. Classical error in the dependent variable inflates standard errors without biasing "
       "coefficients, which works against the results below.")
DV3 = ("Gross rather than net inflows are used because policy rations entry, not exit; scaling by the working-age "
       "population expresses the inflow in the units the mechanism cares about, and results for unscaled inflows are "
       "reported so that no population estimate need appear on either side. All entry categories are aggregated because "
       "every category's criteria were policy-controlled and every category adds working-age residents.")
PJ1 = ("The National Population Projections have been produced biennially since 1954, by the Government Actuary's "
       "Department (GAD) until 2006 and by the ONS thereafter, projecting the population by single year of age on stated "
       "fertility, mortality, and migration assumptions and reporting the implied dependency ratios for every projected "
       "year. The measure used here is the old-age dependency ratio: persons of state pension age per thousand of working "
       "age, on the UK definition consistent within each vintage (pension age 60 for women and 65 for men until the "
       "equalisation phased in over 2010–2019). The OADR is the headline ageing measure of the ONS's own bulletins, and "
       "for fiscal purposes the emphasis is well placed: those aged 65 and over were projected to absorb some 42 per cent "
       "of health spending by 2021/22 and draw the state pension besides, while education, the principal fiscal cost of "
       "children, takes roughly nine per cent of public spending (Full Fact, 2018; Institute for Fiscal Studies, 2024). "
       "An anticipated rise in the OADR is, to a close approximation, an anticipated rise in mandatory spending per "
       "worker.")
PJ2 = ("The projections' forecasting power comes from the demographic history they track. Britain's post-war baby boom "
       "arrived in two waves, an immediate surge and a larger wave over roughly 1955–65, and it was already the consensus "
       "at the start of the sample that fertility would not return to those levels (Jones, 1980). Each wave reaches "
       "pension age on a schedule known decades ahead, the first from around 2010 and the second through the 2020s, "
       "which is why old-age dependency is uniquely forecastable: the people who will be pensioners in ten years are "
       "alive, resident, and counted today, so at that horizon projection error reduces essentially to mortality "
       "surprises. The demographic dividend (Farid and Mostari, 2022) is repaid when the large cohorts retire, and the "
       "projections priced that repayment for any government that cared to look.")
PJ3 = ("For the government the NPP is an internal product: for five decades GAD, a department of government grown out "
       "of the actuarial oversight of national insurance, produced it on assumptions agreed with the Treasury, so it "
       "measures the fiscal centre's own demographic expectations. Toward the end of the sample its incorporation into "
       "Budget-time analysis became statutory: the Code for Fiscal Stability (HM Treasury, 1998) required illustrative "
       "long-term fiscal projections within the Budget documents over a horizon of “no less than 10 years into the "
       "future” (HM Treasury, 1999, p. 87), and the first set observed that one in five people would be over 65 by "
       "2026 against one in seven in 1998, costing health care for the over-65s at “over 3½ times the average "
       "for the remainder of the population” (pp. 87–90). The statutory minimum horizon is the horizon at which the "
       "projection variable is measured, and Section 6.1 reports that it is also the horizon that fits best. From 2002 a "
       "dedicated Treasury unit produced the Long-term Public Finance Report, work that by one of its economists' account "
       "brought the Treasury into closer work with the Home Office on the economics of migration (Eich, 2008).")
PJ4 = ("The variable of interest is the projected change in the OADR, Δ̂dₜ(h) = Eₜ[dₜ₊ₕ] "
       "− dₜ, the difference between the ratio projected h years ahead in the most recent vintage and its current "
       "value, at baseline horizon h = 10; horizons of 1, 3, 5 and 7 years, weighted averages, and projected levels are "
       "examined below. For 1997 the operative vintage projected an OADR roughly six points higher in 2007, so "
       "Δ̂d₁₉₉₇(10) ≈ 6.1, then the largest projected ten-year deterioration on record, "
       "though the vintages of the early 2000s would soon quadruple it. The variable is the ageing news confronting the "
       "Budget's author: how much worse, in the government's own published expectation, the ratio of pensioners to "
       "taxpayers will be a decade hence.")
PJ5 = ("Assembling the series required reconstructing the full history of vintages (Online Appendix A). The ONS "
       "publishes complete projections only back to 2014; earlier vintages exist as scans on a National Archives site "
       "that is neither searchable nor indexed, navigable only to 1998. A machine-readable annex to the 2008-based NPP "
       "was found to link every predecessor back to the first, 1954-based projection (ONS, 2016b), and through those "
       "links the projected OADR at each horizon was extracted for every vintage from the 1977-based to the 2018-based. "
       "Between biennial vintages the most recent is carried forward, mirroring the information available to "
       "policymakers; the same convention supplies the working-age denominators. Multi-vintage records have been "
       "assembled before only from within the statistical service (Shaw, 2007); this is, to my knowledge, the first "
       "horizon-complete, machine-readable panel of projected dependency ratios by vintage from public sources, and the "
       "first use of the vintages as an information-set regressor. Two conventions: vintages are assigned by base year, "
       "on the grounds that GAD was an arm of government and the fiscal centre saw the projections before publication, "
       "an assumption confronted in Section 5.2; and the legislated female pension-age rise is embedded in the relevant "
       "vintages, so the projections already incorporate the principal change to the ratio's definition within the "
       "sample.")
CT1 = ("The institutional literature specifies the Treasury's decision inputs as the fiscal aggregates and the labour "
       "market, observed with a lag at Budget time (Heclo and Wildavsky, 1974); the controls are the previous year's "
       "values of three series. *Tax per working-age person*, in thousands of constant 2025 pounds, is national accounts "
       "taxes from the OBR databank (OBR, 2026) divided by the working-age population: the per-taxpayer basis rather "
       "than the tax-to-GDP ratio is the theoretically appropriate measure, since the political cost in the model "
       "attaches to the burden borne per taxpayer and income tax and National Insurance alone account for over half of "
       "receipts, and the variable measures how far the intensive margin has already been used. *The annual log change "
       "in public sector net debt as a share of GDP*, from the same source, measures the rate at which the borrowing "
       "margin is closing; the level appears in the stationarity tests but not the estimating equation. *The ILO "
       "unemployment rate* (ONS, 2026) is the Budget-time reading of the labour market, entering both as a political cost "
       "of visible inflows and as a conventional demand-side determinant. Candidates tested and excluded on statistical "
       "grounds, the inactivity rate, real GDP and productivity growth, ten-year and three-month gilt yields (Federal "
       "Reserve Bank of St. Louis, 2025), and the Budget-time official growth forecast digitised from HM Treasury Budget "
       "reports via the Economic Statistics Centre of Excellence (2026), are documented in Section 6.1.")
CT2 = ("No credible instrument for UK inflows exists: the only proposed proxy, National Insurance registrations (Portes "
       "and Forte, 2017), fails on relevance by the ONS's own assessment, since registrations neither distinguish "
       "short-term from long-term stays nor net out emigration (ONS, 2016a). Identification therefore rests on timing, "
       "predetermined regressors, and falsification, as set out next.")

# ==================================================== 5. EMPIRICAL STRATEGY
E1 = L2(5)
E2 = ("where mₜ is non-EU inflows per thousand lagged working-age residents, Δ̂dₜ(10) the projected "
      "ten-year OADR change from the current vintage, τₜ₋₁ the lagged tax take per working-age person, "
      "Δln Bₜ₋₁ the lagged log change in the debt-to-GDP ratio and uₜ₋₁ the lagged "
      "unemployment rate. The log-level form implies that a given projected deterioration calls forth a given percentage "
      "adjustment of the inflow, and is the form favoured by specification tests (Section 6.1); the lagged dependent "
      "variable captures the adjustment costs of the model and the persistence documented throughout the UK literature "
      "(Mitchell and Pain, 2003; Hatton, 2005). A second specification replaces τₜ₋₁ with a "
      "deterministic trend; the two are collinear (r = 0.98) and cannot be separated statistically, so both are reported "
      "and the interpretation leans only on results common to both. Inference uses heteroskedasticity-and-"
      "autocorrelation-consistent (HAC) standard errors, with Newey–West and OLS errors reported in Online Appendix "
      "Table B2.")
E3 = ("The estimator deserves brief defence. No valid instrument exists (Section 4.4); difference-in-differences and "
      "regression-discontinuity designs are unavailable for want of a control group or an assignment threshold, since "
      "the regime applied to the whole non-EU margin at once and changed continuously; and a cross-country panel would "
      "trade the UK's institutional depth for breadth the hypothesis does not yet need (" + S("7") + "). Within the "
      "single-country setting the ARDL framework is the standard recommended for UK migration data (Mitchell and Pain, "
      "2003): it accommodates persistence, nests the static model, supports bounds testing among I(0)/I(1) variables, "
      "and delivers long-run multipliers with structural content under the model of Section 3.")
ID1 = ("The regressors are predetermined with respect to the current year's inflow shock: the fiscal controls are lagged "
       "realisations and the projection is fixed before the flows it is asked to predict, so within-year reverse "
       "causality is excluded by construction. Three threats survive. The first is the lagged dependent variable, which "
       "makes least squares consistent but biased in finite samples by roughly −(1 + 3ρ)/T (Hurwicz, 1950; "
       "Kendall, 1954), about −0.06 at T = 41 and ρ ≈ 0.5, so the persistence estimates and the long-run "
       "multipliers that divide by (1 − ρ) are lower bounds. A recursive-design residual bootstrap of the fiscal "
       "specification (2,000 replications) estimates the bias at −0.104, raising persistence from 0.49 to about 0.60 "
       "and the implied long-run response to the projection from 1.6 to about 2.0 per cent per point, with a recentred "
       "percentile interval of [0.9, 2.9]; the bias exceeds the analytic value because the equation carries four "
       "persistent, correlated regressors, and a Monte Carlo calibration at the same sample size confirms the "
       "correction is nearly unbiased. The bias operates on the autoregressive term and cannot manufacture significance "
       "of the projection; the Breusch–Godfrey tests reported throughout are what license treating the lagged term "
       "as predetermined.")
ID2 = ("The second is publication timing: vintages are labelled by base year but released roughly a year later, and "
       "base-year assignment assumes policymakers saw them first, an assumption institutionally grounded for the GAD era "
       "and weaker for the ONS era. Dating each vintage from the year after its base reassigns the vintage in 21 of the "
       "41 sample years; " + S("6.3") + " re-estimates on that basis. The third and most fundamental is that the "
       "projections are not mechanically exogenous, since every vintage embeds an assumption about future net migration. "
       "Three points bound the concern. The contamination is attenuating: higher assumed migration lowers the projected "
       "OADR, biasing β₁ toward zero. The demographic arithmetic limits it: the old-age population ten years "
       "ahead consists almost entirely of persons aged 55 and over already resident, so the numerator is essentially "
       "invariant to migration assumptions. And the clean remedy is implemented: " + S("6.3") + " rebuilds the "
       "projected OADR under zero net migration from each vintage's published assumptions and re-estimates with the "
       "purged regressor, with results essentially unchanged.")
ID3 = ("The identification status, stated plainly: the estimates are not experimental. The claim is that a "
       "predetermined official expectation of ageing predicts subsequent policy-controlled inflows conditional on the "
       "fiscal state; the mechanical channel from migration into the expectation is removed outright; and the "
       "confounders most naturally levelled at the result are tested by the placebo margins of " + S("6.5") + ", since "
       "a driver operating through labour demand or general attractiveness should load onto EU inflows, which were "
       "uncontrolled and elastic, and onto the total dependency ratio insofar as it is demographic rather than fiscal. "
       "Classical measurement error in the dependent variable inflates standard errors without biasing coefficients, and "
       "the LTIM check (" + S("6.7") + ") addresses the non-classical components the ONS has documented.")
DG = ("Every specification carries a fixed battery of diagnostics, each of which earns its place here. Ramsey's RESET "
      "screens for neglected nonlinearity and rejected the level form (Section 6.1); Breusch–Godfrey at first and "
      "second order, preferred to Durbin–Watson, which is invalid with a lagged dependent variable, is a condition "
      "for consistency; Breusch–Pagan tests heteroskedasticity, with HAC inference as second line; Jarque–Bera "
      "matters more than usual because exact inference at 41 observations leans on approximate normality; and maximum "
      "variance inflation factors monitor the collinearity a short trending sample invites. Orders of integration are "
      "established by sequential ADF and KPSS testing (Online Appendix Table B1): every regressor is I(0) or I(1), the "
      "condition for bounds testing, and the Pesaran–Shin–Smith bounds test provides the formal test of a level "
      "relationship, its power at this sample size being limited. Because theory pins the conditioning set down only to "
      "broad categories, some specification search was unavoidable; the protocol was fixed in advance, limited to the "
      "variables named by the institutional literature, and every candidate tested is reported, including the failures. "
      "The headline coefficient's robustness across every specification tried, not the significance of any one, is the "
      "basis of the claim.")

# ==================================================== 6. EMPIRICAL ANALYSIS
R1 = ("A brief account of the path to the final specification guards against the impression of a curated result. The "
      "starting point was a five-year projection window, matching the electoral cycle and the Treasury's own review "
      "horizons, implemented as a weighted average of projected OADR changes, in a level-form regression on the tax "
      "take, unemployment, inactivity, and productivity growth. It produced a strongly significant projection coefficient "
      "but failed the Breusch–Godfrey test (p = 0.018), motivating the autoregressive term the adjustment-cost logic "
      "independently implies; an interaction between the projection and the tax burden generated extreme collinearity "
      "(maximum VIF above 100) and was replaced by the additive form. No measure of growth attained significance in any "
      "variant, consistent with a rule oriented to the extensive margin; neither gilt yield mattered, suggesting the debt "
      "stock rather than its price is the constraint; the Budget-time growth forecast was significant in some variants "
      "only at the cost of destabilising the other coefficients; and the inactivity rate, nearly time-invariant, was "
      "dropped when degrees of freedom required. The level-form ARDL passed all diagnostics except RESET, which favoured "
      "logs; in logs the best-fitting horizon lengthens from five to ten years, consistent with the primacy of the "
      "Treasury's planning horizon over the electoral cycle, and matching the statutory minimum horizon for the "
      "government's own long-term projections (" + S("4.2") + "), a useful coincidence: the specification the data "
      "select is the one the fiscal framework mandates.")
R2 = L2(53)
R3 = ("The projection term is the paper's central result. In the fiscal specification (column 1), a one-point increase "
      "in the projected ten-year rise of the OADR, one additional prospective pensioner per thousand working-age "
      "residents, is associated with a 0.8 per cent increase in the current non-EU inflow rate (coefficient 0.0082, HAC "
      "standard error 0.0014); the trend specification gives 0.0075. The controls carry the signs the model predicts. "
      "Each additional thousand pounds of annual tax per working-age person is associated with 4.8 per cent higher "
      "inflows (P2), the substitution effect at the heart of the salience mechanism, a sign worth pausing on because a "
      "naive fiscal-need story might predict the opposite. The coefficient of 0.57 on the lagged log change in debt/GDP "
      "(P3) implies that a one-point faster rise in the debt ratio is followed by roughly 0.6 per cent higher inflows, so "
      "the years of sharpest fiscal deterioration are those followed by the largest expansions of the quiet margin. A "
      "percentage point of lagged unemployment reduces inflows by about 2 per cent (P4), consistent with both the "
      "political-cost channel and conventional labour-demand effects, which cannot be separated here. Persistence "
      "(ρ ≈ 0.49 and 0.71, both understated by the attenuation of Section 5.2) matches the flow persistence "
      "documented across the UK literature.")
R4 = ("The fit is tight (adjusted R² of 0.96), though with a lagged dependent variable and trending data the R² "
      "overstates explanatory success; the meaningful statement is that the projection term remains individually "
      "significant at one per cent under HAC and Newey–West inference alongside the autoregressive term, the trend "
      "or its fiscal twin, and the business-cycle control, with RESET, Breusch–Godfrey at both orders, "
      "Breusch–Pagan and Jarque–Bera all comfortably failing to reject in both columns. The magnitudes sit within "
      "the UK literature where comparison is possible: the unemployment semi-elasticity is of the order found with and "
      "without free-movement flows, its lagged form accords with Hatton's (2005) finding that migration responds to "
      "labour-market changes rather than levels, and the persistence parameter brackets the range in Mitchell and Pain "
      "(2003). The projection term has no published counterpart, which is the paper's point, but it can be benchmarked "
      "internally: between 1997 and 2002 the projected ten-year OADR change rose by some nineteen points, which through "
      "the long-run multiplier of Table 3 implies an eventual expansion of the non-EU inflow rate on the order of "
      "three-tenths of a log point, a substantial share of the observed rise over those years.")
IC0 = ("Three checks bear directly on the identification argument of Section 5.2 and are taken before the long-run and "
       "falsification results because the latter presuppose them.")
IC1 = ("*Zero-net-migration projections.* From each vintage's published components of change, the projected OADR at "
       "every horizon is recomputed under zero net migration from the observation year forward, by removing the "
       "vintage's cumulative assumed net migration from the projected working-age denominator (Online Appendix A), so "
       "that the regressor cannot contain the vintages' migration assumptions. It returns 0.0075 (HAC s.e. 0.0013) in "
       "the fiscal specification and 0.0072 (0.0013) in the trend, against 0.0082 and 0.0075 with the published "
       "projections, with unchanged significance, comparable or cleaner diagnostics, and an implied long-run response of "
       "1.5 per cent. The response of controlled migration to projected ageing is not an artefact of the projections' "
       "own migration assumptions.")
IC2 = ("*Publication dating.* Dating each vintage from the year after its base reassigns the vintage in force in 21 of "
       "the 41 years, moving one observation by more than fifteen points. The estimates barely move: 0.0075 (HAC s.e. "
       "0.0017) in the trend specification and 0.0086 (0.0015) in the fiscal, significant at 0.1 per cent with "
       "diagnostics passing; the zero-migration variant returns 0.0067 (0.0016), the total dependency ratio falls to "
       "−0.0001 (p = 0.94) and the EU placebo stays insignificant (p = 0.40).")
IC3 = L2(97)
LR1 = ("The regressors are a mixture of I(1) and I(0) series (Online Appendix Table B1), which rules out the possibility "
       "that the level relationship is an artefact of common integration order and dictates the bounds procedure of "
       "Pesaran, Shin and Smith (2001), valid for any such mixture. ADF tests reject a unit root in the residuals of both "
       "specifications (p = 0.025 and 0.039). The bounds F-test on the error-correction form rejects the null of no "
       "level relationship at five per cent (F = 5.07, p = 0.03); the t-version is marginal (p = 0.07), falling inside "
       "the inconclusive region between the I(0) and I(1) bounds at five per cent rather than failing against them. The "
       "statistics are exact and the p-values come from a stochastic response surface. This is supportive but not "
       "overwhelming evidence of cointegration: with 41 observations the tests have limited power, and the honest "
       "statement is that the data are consistent with a long-run equilibrium relationship rather than proof of one. "
       "Table 3 reports the implied long-run multipliers.")
LR2 = ("The long-run response to the projection is approximately 2.0 per cent per projected point once the finite-sample "
       "correction of Section 5.2 is applied, some two and a half times the impact effect, against an uncorrected "
       "multiplier of 1.6 per cent (p = 0.001), with the tax-burden and debt multipliers similarly amplified. The "
       "amplification is the cumulative adjustment of a persistent instrument to a persistent signal: with an "
       "autoregressive coefficient near one half, roughly half of any gap between the inflow rate and its target closes "
       "each year, so a sustained deterioration builds over two to three years to twice its impact size, consistent with "
       "the administrative propagation lags of entry policy and implying that annual data do not miss the bulk of the "
       "dynamics.")
F0 = ("Table 4 collects the falsification and robustness exercises. The two placebo tests carry the greatest evidential "
      "weight because they are tests the fiscal hypothesis could have failed.")
F1 = ("*Old-age versus total dependency.* If the mechanism is fiscal, the projection that matters is the old-age ratio, "
      "since pensioners are expensive, not the total ratio, whose child component is cheap (" + S("4.2") + "). "
      "Substituting the projected ten-year change in the total dependency ratio eliminates the result: the coefficient "
      "falls by three-quarters and is indistinguishable from zero (p = 0.21), as it is in every other variant, at every "
      "horizon, in levels as in deltas. Against the no-projection baseline the contrast is sharper still: the old-age "
      "ratio improves the information criterion by 11.9 points, the total ratio worsens it by 0.5 (F(1, 35) = 1.32, p = "
      "0.26), so the model is better specified with no projection than with the wrong one. A generic demographic "
      "confounder has no reason to select the pensioner-specific ratio so cleanly; a fiscal mechanism must.")
F2 = ("*The uncontrolled margin.* Might some unobserved driver, rising UK attractiveness, labour demand, or networks, "
      "correlate with projected ageing and move all migration, with policy irrelevant? EU inflows provide the test, "
      "responding freely to every such driver but not to UK entry policy. Re-estimating the full specification on EU "
      "inflows, constructed identically, returns an OADR-projection coefficient of 0.0022 with p = 0.52; only the "
      "persistence term survives, and adding indicators for accession-wave years weakens the projection further, to "
      "0.0015. Figure 2 shows the contrast as added-variable plots in which the fitted slope is the coefficient: a clear "
      "conditional relationship on the controlled margin, none on the uncontrolled one. Whatever moved non-EU inflows in "
      "step with projected ageing did not move the flows the government could not ration; the confound would have to "
      "operate exclusively on the controlled margin, which is to say it would have to *be* policy.")
PE = ("The structural-change literature attributes the expansion to a liberal policy era after 1997 (Mitchell, Pain and "
      "Riley, 2011), and the horse race can be run directly. Their indicator, POL97, is a step from 1998 onwards, not a "
      "window closing in 2007; the distinction cannot arise in their own work, whose sample ends in 2007, but over the "
      "present sample the step is the better description, since the non-EU inflow rate averages 2.8 per thousand before "
      "1998, 6.5 during the window and 6.7 after it. Tested faithfully, the step carries more weight than the window, "
      "0.14 (p = 0.07), or 0.17 lagged as they enter it (p = 0.05), against 0.012 (p = 0.84) for the window, and the "
      "projection survives either way, at 0.0059 and 0.0071. Their full dummy set does no better: POL97, POL02 for the "
      "2002 skilled-worker route, and the enlargement indicators for 1986, 1995 and 2004 are jointly irrelevant (F(5, 30) "
      "= 1.67, p = 0.17) while the projection holds at 0.0070 (p = 0.011). The collinearity is the point: the ten-year "
      "delta averages 0.9 points before 1998, 14.9 within the window, and −4.5 after 2007, so the indicator nearly "
      "discretises the projection. Interacting the step with the projection makes the direction plainer: the interaction "
      "is negative (−0.0068, p = 0.37), so the projection's effect is larger before 1998 than after, the opposite of "
      "what an account resting on the post-1997 government requires. In Mitchell, Pain and Riley's own transformation, "
      "the annual change in the log migration rate, which discards the persistence carrying most of the variation here "
      "(adjusted R² falls to 0.07) but renders both sides stationary, the projection is the only variable that "
      "registers: 0.0062 (p = 0.099) for non-EU inflows and 0.0072 (p = 0.033) for total inflows, while the lagged "
      "POL97, POL02 and the 2004 enlargement return p = 0.68, 0.79 and 0.71. Conditional on the government's own ageing "
      "projections, the policy era has no explanatory power in their specification or in mine: the earlier literature's "
      "dummies were standing in for the projections.")
FR = ("The remaining checks are reported in full in Online Appendix B and summarised here. The result does not depend "
      "on the per-capita scaling: on gross non-EU inflows the coefficient is 0.0080 (HAC s.e. 0.0014) in the fiscal and "
      "0.0073 (0.0015) in the trend specification, and in levels each further projected point is associated with roughly "
      "1,300 to 1,400 additional gross arrivals a year. On the LTIM series from 1991 it remains significant at five per "
      "cent (0.0103, s.e. 0.0038, N = 28). The projected level in place of the change is significant with a similar "
      "magnitude (0.0080); across horizons the term is significant at three, five, seven, and ten years, with ten "
      "fitting best, and the one-year change alone carries no signal (p = 0.56). Beginning the sample in 1983 or 1987 "
      "returns 0.0069 and 0.0062, both significant at five per cent, and the result is invariant to the choice of "
      "specification and to OLS, HAC, or Newey–West inference. What the battery cannot exclude is a confounder "
      "sharing the projection's exact profile: fiscal in character, specific to the old-age ratio, operative only on the "
      "controlled margin, and timed to the vintages. No candidate has suggested itself; that residual vulnerability is "
      "what the agenda of " + S("7") + " is built to close.")
H1 = ("The estimated rule acquires content when laid against the sample's political history, because the projection "
      "variable moves at times no party-political account selects (Figure 1). The projected ten-year OADR change was "
      "negative at the start of the sample and first turned positive in the early 1980s, as the leading edge of the "
      "post-war birth surge entered the ten-year window; non-EU inflows rose in 1981–85, under a Conservative government "
      "publicly committed to restriction. The delta turned negative around 1990 and inflows stagnated; it returned to "
      "positive territory in 1996 and inflows resumed growth in the final year of the Major administration, before the "
      "change of government. In 1997 the delta reached a then-record 6.1 points, and in 2002–03 it averaged roughly 25 "
      "points as the first baby-boom wave arrived a decade from pension age; through the short-run coefficient, "
      "projections of that size call for expansions on the order of 19 per cent a year from the projection channel "
      "alone, and gross non-EU inflows cleared 300,000 for the first time in 2004, more than triple their level a decade "
      "earlier.")
H2 = ("Read through the model, the celebrated post-1997 policy turn was not the arrival of a new ideology but the arrival "
      "of the baby boomer at the projection horizon: a government of either party, facing the same vintages, is "
      "predicted to have expanded the quiet margin, and the pre-1997 record shows governments of the other party doing "
      "exactly that whenever the projections instructed it. Accelerations timed to the projections rather than to "
      "changes of administration are the pattern a party-specific account leaves unexplained and the fiscal account "
      "predicts, as Section 6.6 showed formally. Had the 1997 election gone the other way, the model expects the "
      "subsequent expansion to have occurred in any case, in different rhetoric.")

# ================================================================ 7. DISCUSSION
X1 = ("*The post-2019 episode.* The sample ends at Brexit, after which the regime changed discretely, and the subsequent "
      "history is directionally consistent with the fiscal account: a government elected on a mandate to reduce "
      "migration instead liberalised the non-EU regime, abolishing the Resident Labour Market Test and lowering salary "
      "thresholds (Migration Observatory, 2021), amid a pandemic that raised the debt ratio by more than ten points in a "
      "year and against projections of renewed ageing as the second baby-boom cohort approached retirement (ONS, 2019). "
      "Net migration rose from 184,000 in 2019 to over 900,000 in 2023, driven overwhelmingly by non-EU entry (Migration "
      "Observatory, 2026; BBC News, 2025). Part of that is mechanical rebalancing under equalised entry conditions, so no "
      "structural claim rests on it; the episode is noted because the model predicts the *direction* of the response, "
      "and the leading alternative, the disposition of a party that had just campaigned the other way, does not.")
X2 = ("*Recognition of the mechanism elsewhere.* The salience model predicts who advertises the instrument. Westminster "
      "held the entry lever throughout and never campaigned on it; Scotland's devolved government, which does not hold "
      "it, campaigns for it explicitly and publishes this paper's regressor as a target, calculating that maintaining "
      "the Scottish dependency ratio requires the working-age population to grow by some 32,000 a year and proposing a "
      "devolved visa to achieve it (Scottish Government, 2018, 2020). Visibility of the demand runs inverse to "
      "possession of the lever, which is what pricing policy by political exposure predicts, on a case the model was not "
      "built from.")
X3 = ("*Limitations and agenda.* Three limitations are material. The zero-migration reconstruction treats all net "
      "migrants as working-age, an approximation the vintages' age-specific assumptions would refine. The dependent "
      "variable pools work, study, and family entry, so the model's sharper implication, that the response should "
      "concentrate where discretion and youth are greatest, is testable only with administrative data. And outside the "
      "2000–2001 episode the evidence for the mechanism is institutional and statistical; the model explains why "
      "documentary confirmation should be scarce, but the drafting record shows the reasoning was written down at least "
      "once and can therefore be found. Each admits a remedy. The biennial NPP releases can be exploited as information "
      "shocks: the revision between vintages, decomposed into its mortality, fertility, and migration-assumption "
      "components, supplies news orthogonal to migration by construction, and Home Office quarterly visa grants by "
      "category provide an outcome with the frequency for an event-study design around release dates, which would be "
      "decisive in a way the annual reduced form cannot be. The mechanism can be taken to the Treasury files already "
      "inside the thirty-year declassification window, which cover the sample's first half, and to text analysis of "
      "Budget documents. External validity can be tested in a cross-country panel on UN projection vintages, where the "
      "model predicts the channel is strongest where fiscal authority is centralised and migration policy "
      "executive-controlled; and the pension-age and migration instruments can be modelled jointly.")

# ================================================================ 8. CONCLUSION
C1 = ("This paper asked whether the sustained and politically homeless expansion of controlled immigration to the United "
      "Kingdom between 1979 and 2019 can be explained within the outlines of fiscal policy. The data are consistent with "
      "that hypothesis. A newly assembled series of the government's own biennial ageing projections predicts non-EU "
      "inflows, the margin policy controlled, with a short-run response of 0.8 per cent per projected point of ten-year "
      "OADR deterioration and a bias-corrected long-run response of approximately 2.0 per cent, conditional on the "
      "Treasury's fiscal state variables and with signs matching the salience model of instrument choice. The "
      "relationship is absent where the fiscal account says it should be: for projections of the fiscally cheap total "
      "dependency ratio, and for EU inflows, which no British government could ration. Beyond the substantive claim the "
      "paper contributes a reusable object and a reusable frame: the reconstructed vintage series of official ageing "
      "projections from 1975, extendable through the route documented in Online Appendix A to the first projection of "
      "1954, and the treatment of policy instruments as priced by institutional salience, which also predicts the "
      "observed preference for legislating pension-age increases decades before they bind.")
C2 = ("The interpretation is disciplined by what the design cannot yet deliver: the projections are the government's own "
      "and embed migration assumptions; the documentary record, though it now includes the released 2000 drafts in which "
      "the reasoning appears and is then removed, covers a single episode; and the sample is one country and forty-one "
      "years. The agenda above is the path from a robust and repeatedly falsifiable association to a causal account.")
C3 = L2(135)

from r30_overrides import *   # second compression pass overrides the definitions above
from r30_overrides2 import *  # third pass
from r30_overrides3 import *  # fourth pass
from r30_overrides4 import *  # fifth pass: fiscal-primary, no enlargement variant, MPR condensed
from r30_overrides5 import *  # sixth pass: trend figures out of the text
from r30_overrides6 import *  # seventh pass: HPFD single-source figures

# ============================================================== ASSEMBLY
part1 = [
 "# 1. Introduction", I1, I2, I3, I4, I5, I6, I7, I8, I9, ROADMAP,
 "# 2. Institutional Background",
 "## 2.1 The Treasury as fiscal centre", T1, T2, T3,
 "## 2.2 The salience asymmetry: Budget Day and the Immigration Rules", SA1, SA2, SA3,
 "## 2.3 Control of the non-EU margin, 1979–2019", CM1, CM2, CM3,
 "## 2.4 What the fiscal centre wrote down", DR1, DR2, DR3,
 "# 3. A Stylised Model of Fiscal Instrument Choice",
 M1, M2, EQ1(102, 104), M3, M4, EQ1(110, 112), M5, M6, EQ1(120, 122), M7, EQ1(126, 128), M8, M9, M10,
 "# 4. Data", D0, "[[TABLE1]]",
 "## 4.1 Dependent variable: non-EU inflows", DV1, DV2, DV3,
 "## 4.2 The projection series", PJ1, PJ2, PJ3, PJ4, PJ5,
 "## 4.3 Fiscal and macroeconomic controls", CT1,
 "## 4.4 The absence of an instrument", CT2,
]
part2 = [
 "# 5. Empirical Strategy",
 "## 5.1 Specification", E1, EQ2(7, 9), E2, E3,
 "## 5.2 Identification: timing, threats, and remedies", ID1, ID2, ID3,
 "## 5.3 Diagnostics and specification protocol", DG,
 "# 6. Empirical Analysis",
 "## 6.1 Specification development", R1,
 "## 6.2 Main estimates", R2, "[[TABLE2]]", R3, R4,
 "## 6.3 Identification checks", IC0, IC1, IC2, IC3,
 "## 6.4 Cointegration and the long run", LR1, "[[TABLE3]]", LR2,
 "## 6.5 Falsification: the fiscally cheap ratio and the uncontrolled margin", F0, "[[TABLE4]]", F1, F2, "[[FIGURE2]]",
 "## 6.6 The policy-era alternative", PE,
 "## 6.7 Further robustness", FR,
 "## 6.8 Reading the history through the model", "[[FIGURE1]]", H1, H2,
 "# 7. Discussion: External Events, Recognition, and Limitations", X1, X2, X3,
 "# 8. Conclusion", C1, C3,   # C2 dropped for length (limitations stated in Section 7)
 "[[TITLENOTE]]", "[[REFS]]",
] + [l.rstrip() for l in P2[[l.strip() for l in P2].index('[[REFS]]') + 1:] if l.strip()]   # reference list follows the marker

from assemble_r45 import remap   # same section numbering, same remap rules
from period_1977 import period1977
t1 = period1977(remap("\n\n".join(part1))) + "\n"
t2 = period1977(remap("\n\n".join(part2))) + "\n"
io.open("manuscript_r30_part1.md", "w", encoding="utf-8").write(t1)
io.open("manuscript_r30_part2.md", "w", encoding="utf-8").write(t2)
body = t1 + t2.split("[[REFS]]")[0]
print("r30 words (body, excl. refs):", len(body.split()))
print("stale refs:", re.findall(r"Section (?:9|2\.5|2\.6)\b", body) or "none")
