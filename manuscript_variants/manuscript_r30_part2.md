# 5. Empirical Strategy

## 5.1 Specification

The estimating equation is the empirical counterpart of the policy rule derived in Section 3: an autoregressive distributed lag model, ARDL(1), in log-level form,

[[EQ]]
ln mₜ = α + ρ ln mₜ₋₁ + β₁ Δ̂dₜ(10) + β₂ τₜ₋₁ + β₃ Δln Bₜ₋₁ − β₄ uₜ₋₁ + εₜ,
[[/EQ]]

where mₜ is non-EU inflows per thousand lagged working-age residents, Δ̂dₜ(10) the projected ten-year OADR change from the current vintage, τₜ₋₁ the lagged tax take per working-age person, Δln Bₜ₋₁ the lagged log change in the debt-to-GDP ratio and uₜ₋₁ the lagged unemployment rate. The log-level form, favoured by specification tests (Section 6.1), implies that a given projected deterioration calls forth a given percentage adjustment; the lagged dependent variable captures the model's adjustment costs and the persistence documented in the UK literature (Mitchell and Pain, 2003; Hatton, 2005). A second specification replaces τₜ₋₁ with a trend; the two are collinear (r = 0.98), so both are reported in Table 2 and the fiscal specification is carried through the rest of the paper. Inference uses HAC standard errors, with Newey–West and OLS errors in Online Appendix Table B2.

The estimator deserves brief defence. No valid instrument exists (Section 4.4); difference-in-differences and regression-discontinuity designs are unavailable for want of a control group or an assignment threshold, since the regime applied to the whole non-EU margin at once and changed continuously; and a cross-country panel would trade the UK's institutional depth for breadth the hypothesis does not yet need (Section 7). Within the single-country setting the ARDL framework is the standard recommended for UK migration data (Mitchell and Pain, 2003): it accommodates persistence, nests the static model, supports bounds testing among I(0)/I(1) variables, and delivers long-run multipliers with structural content under the model of Section 3.

## 5.2 Identification: timing, threats, and remedies

The regressors are predetermined with respect to the current year's inflow shock: the fiscal controls are lagged realisations and the projection is fixed before the flows it is asked to predict, so within-year reverse causality is excluded by construction. Three threats survive. The first is the lagged dependent variable, which makes least squares consistent but biased in finite samples by roughly −(1 + 3ρ)/T (Hurwicz, 1950; Kendall, 1954), about −0.06 at T = 43, so the persistence estimates and the long-run multipliers that divide by (1 − ρ) are lower bounds. A recursive-design residual bootstrap of the fiscal specification (2,000 replications) estimates the bias at −0.11, raising persistence from 0.47 to about 0.58 and the long-run response to the projection from 1.5 to about 2.0 per cent per point, with a recentred percentile interval of [0.9, 2.8]. The bias cannot manufacture significance of the projection.

The second is publication timing: vintages are labelled by base year but released roughly a year later, and base-year assignment assumes policymakers saw them first, well grounded for the GAD era and weaker for the ONS era; dating each vintage from the year after its base reassigns the vintage in 22 of the 43 years, and Section 6.3 re-estimates on that basis. The third and most fundamental is that every vintage embeds an assumption about future net migration. Three points bound the concern: the contamination is attenuating, since higher assumed migration lowers the projected OADR and biases β₁ toward zero; the old-age population ten years ahead consists almost entirely of persons already resident, so the numerator is essentially invariant to migration assumptions; and Section 6.3 rebuilds the projected OADR under zero net migration and re-estimates with the purged regressor, with results essentially unchanged.

The estimates are not experimental. The claim is that a predetermined official expectation of ageing predicts subsequent policy-controlled inflows conditional on the fiscal state; the mechanical channel from migration into the expectation is removed outright; and the natural confounders are tested by the placebo margins of Section 6.5, since a driver operating through labour demand or attractiveness should load onto EU inflows, and onto the total dependency ratio insofar as it is demographic rather than fiscal. Classical measurement error in the dependent variable inflates standard errors without biasing coefficients; the LTIM check (Section 6.7) addresses the non-classical components.

## 5.3 Diagnostics and specification protocol

Every specification carries a fixed battery of diagnostics. Ramsey's RESET screens for neglected nonlinearity and rejected the level form (Section 6.1); Breusch–Godfrey at first and second order, preferred to Durbin–Watson, which is invalid with a lagged dependent variable, is a condition for consistency; Breusch–Pagan tests heteroskedasticity, with HAC inference as second line; Jarque–Bera matters because exact inference at 43 observations leans on approximate normality; and maximum variance inflation factors monitor collinearity. Orders of integration are established by sequential ADF and KPSS testing (Online Appendix Table B1): every regressor is I(0) or I(1), the condition for bounds testing, and the Pesaran–Shin–Smith bounds test provides the formal test of a level relationship. Some specification search was unavoidable; the protocol was fixed in advance, limited to the variables named by the institutional literature, and every candidate tested is reported, including the failures. The headline coefficient's robustness across every specification tried, not the significance of any one, is the basis of the claim.

# 6. Empirical Analysis

## 6.1 Specification development

The final specification was not the first. A five-year level-form window produced a significant projection coefficient but failed the Breusch–Godfrey test (p = 0.018), motivating the autoregressive term; no growth measure or gilt yield mattered; and in logs the best-fitting horizon lengthened from five to ten years, the statutory minimum horizon for the government's own long-term projections (Section 4.2).

## 6.2 Main estimates

Table 2 reports the two final specifications with HAC standard errors.

[[TABLE2]]

The projection term is the paper's central result. In the fiscal specification (column 1), a one-point increase in the projected ten-year rise of the OADR, one additional prospective pensioner per thousand working-age residents, is associated with a 0.8 per cent increase in the non-EU inflow rate (0.0082, HAC s.e. 0.0015); the trend specification gives 0.0073. The controls carry the predicted signs. Each additional thousand pounds of tax per working-age person is associated with 5.2 per cent higher inflows (P2), the substitution effect at the heart of the salience mechanism, where a naive fiscal-need story would predict the opposite. The coefficient of 0.54 on the lagged log change in debt/GDP (P3) implies that a one-point faster rise in the debt ratio is followed by roughly 0.5 per cent higher inflows. A point of lagged unemployment reduces inflows by about 2 per cent (P4), consistent with both the political-cost channel and labour-demand effects. Persistence (ρ ≈ 0.47 and 0.69, both understated by the attenuation of Section 5.2) matches the UK literature.

The fit is tight (adjusted R² of 0.96), though with a lagged dependent variable and trending data the R² overstates explanatory success; the meaningful statement is that the projection term remains significant at one per cent under HAC and Newey–West inference alongside the autoregressive term, the trend or its fiscal twin, and the business-cycle control, with RESET, Breusch–Godfrey at first and second order, Breusch–Pagan and Jarque–Bera all failing to reject at five per cent in both columns (higher-order Breusch–Godfrey tests reject in the fiscal specification; Online Appendix B). The magnitudes sit within the UK literature: the unemployment semi-elasticity is of the order found with and without free-movement flows, its lagged form accords with Hatton (2005), and the persistence parameter brackets the range in Mitchell and Pain (2003). The projection term has no published counterpart, but between 1997 and 2002 the projected ten-year OADR change rose by some nineteen points, which through the long-run multiplier of Table 3 implies an eventual expansion of the inflow rate on the order of three-tenths of a log point, a substantial share of the rise observed over those years.

The fiscal specification's maximum VIF (13.0) exceeds the conventional threshold of 10, but it belongs to the lagged dependent variable (13.0) and the lagged tax burden (10.4), two trending levels that share the growth of the period; the projection term's VIF is 1.3. Rules of thumb for the VIF are in any case poor guides to whether collinearity matters, which depends on whether the estimates are precise enough for the question at hand (O'Brien, 2007). Finally, the trend specification, with a maximum VIF of 6.0, returns a projection coefficient of 0.0073, so nothing in the headline result depends on the collinearity of the fiscal specification.

## 6.3 Identification checks

Three checks bear directly on the identification argument of Section 5.2 and are taken before the long-run and falsification results because the latter presuppose them.

*Zero-net-migration projections.* From each vintage's published components of change, the projected OADR at every horizon is recomputed under zero net migration from the observation year forward, by removing the vintage's cumulative assumed net migration from the projected working-age denominator (Online Appendix A), so that the regressor cannot contain the vintages' migration assumptions. It returns 0.0075 (HAC s.e. 0.0013) against 0.0082 with the published projections, with unchanged significance and diagnostics.

*Publication dating.* Dating each vintage from the year after its base reassigns the vintage in force in 22 of the 43 years, moving one observation by more than fifteen points. The estimate barely moves: 0.0090 (HAC s.e. 0.0015) against 0.0082, significant at 0.1 per cent with diagnostics passing; the zero-migration variant returns 0.0076 (0.0016), the total dependency ratio falls to 0.0002 (p = 0.91) and the EU placebo stays insignificant (p = 0.28).

*The projection's marginal contribution.* With a lagged dependent variable, adjusted R² reaches 0.94 before the projection enters, so the informative comparison is the nested one. A HAC Wald test of dropping the projection gives F(1, 37) = 32.23 (p < 0.001), and the information criterion improves by 14.9 points when it is restored, a difference conventionally taken to leave no support for the restricted model; and without the projection RESET falls to p = 0.12 against p = 0.41 with it. The omission does not merely cost fit; it leaves detectable misspecification.

## 6.4 Cointegration and the long run

The regressors mix I(1) and I(0) series (Online Appendix Table B1), which rules out a level relationship arising from common integration order and dictates the bounds procedure of Pesaran, Shin and Smith (2001), implemented with the R package ARDL (Natsiopoulos, Tzeremes and Finnan, 2026). ADF tests reject a unit root in the residuals of both specifications (p = 0.022 and 0.046). The bounds F-test rejects the null of no level relationship at one per cent (F = 6.16, p < 0.01), and the t-version at five per cent (t = −4.09, p < 0.05). With 43 observations the tests have limited power: the data are consistent with a long-run equilibrium relationship, not proof of one. Table 3 reports the implied long-run multipliers.

[[TABLE3]]

The long-run response to the projection is approximately 2.0 per cent per projected point once the correction of Section 5.2 is applied, some two and a half times the impact effect, against an uncorrected multiplier of 1.5 per cent (p = 0.002). With an autoregressive coefficient near one half, roughly half of any gap between the inflow rate and its target closes each year, so a sustained deterioration builds over two to three years to twice its impact size, consistent with the administrative propagation lags of entry policy.

## 6.5 Falsification: the fiscally cheap ratio and the uncontrolled margin

Table 4 collects the falsification and robustness exercises. The two placebo tests carry the greatest evidential weight because they are tests the fiscal hypothesis could have failed.

[[TABLE4]]

*Old-age versus total dependency.* If the mechanism is fiscal, the projection that matters is the old-age ratio, since pensioners are expensive, not the total ratio, whose child component is cheap (Section 4.2). Substituting the projected ten-year change in the total dependency ratio eliminates the result: the coefficient falls to 0.0020 (p = 0.16) and is indistinguishable from zero, as it is in every other variant, at every horizon, in levels as in deltas. Against the no-projection baseline the contrast is sharper still: the old-age ratio improves the information criterion by 14.9 points, the total ratio by 0.1 (HAC Wald F(1, 37) = 2.11, p = 0.16), barely earning the degree of freedom it costs. A generic demographic confounder has no reason to select the pensioner-specific ratio so cleanly; a fiscal mechanism must.

*The uncontrolled margin.* Might some unobserved driver, rising UK attractiveness, labour demand, or networks, correlate with projected ageing and move all migration, with policy irrelevant? EU inflows provide the test, responding freely to every such driver but not to UK entry policy. Re-estimating the full specification on EU inflows, constructed identically, returns an OADR-projection coefficient of 0.0030 (p = 0.41); only the persistence term survives. Figure 2 shows the contrast as added-variable plots in which the fitted slope is the coefficient: a clear conditional relationship on the controlled margin, none on the uncontrolled one. Whatever moved non-EU inflows in step with projected ageing did not move the flows the government could not ration; the confound would have to operate exclusively on the controlled margin, which is to say it would have to *be* policy.

[[FIGURE2]]

## 6.6 The policy-era alternative

Mitchell, Pain and Riley (2011) attribute the post-1997 expansion to a liberal policy era, captured by a step indicator from 1998, which their 2007 sample end makes indistinguishable from a 1998–2007 window. Adding their full indicator set (POL97, POL02 for the 2002 skilled-worker route, and enlargement indicators for 1986, 1995 and 2004) to the fiscal specification leaves the indicators jointly irrelevant under HAC inference (F(5, 32) = 1.09, p = 0.38), while the projection holds at 0.0080 (p = 0.001). The reason is that the indicator nearly discretises the projection: the ten-year delta averages 0.6 points before 1998, 14.9 within the window and −4.5 after 2007, so the literature's dummies were standing in for the projections. Two checks confirm the reading. Interacting the step with the projection returns a negative interaction (−0.0072, p = 0.34), the opposite of what a post-1997 account requires; and in Mitchell, Pain and Riley's own differenced form no indicator approaches significance, while the projection keeps its sign, if not its precision (0.0050, p = 0.18 for non-EU inflows; 0.0057, p = 0.098 for total inflows). Where the indicators do earn their place is on total inflows including the EU margin the government could not ration (F(5, 32) = 5.50, p < 0.001), with the projection falling to 0.0028 (p = 0.31), the EU placebo arriving from the other direction (maximum VIF 56; Online Appendix B).

## 6.7 Further robustness

The remaining checks, every check in the trend specification, and the alternative tax measures (Mendoza, Razin and Tesar, 1994) are in Online Appendix B. Figure 3 plots the coefficient at every horizon: the projection term is significant from three years on and fits best at ten, the one- and two-year changes carry no signal (p = 0.47 and 0.11), and the total dependency ratio registers at no horizon. Figure 4 collects every variant, including gross inflows, the projected level, alternative tax measures, the policy-era indicators, sample starts in 1979, 1983 and 1987, and the LTIM series from 1991: all fifteen intervals exclude zero and both placebo intervals include it. A second lag of the dependent variable is insignificant (p = 0.65) and leaves the higher-order serial correlation in place, which is why inference is HAC. Rotating the projection series through the sample years, which keeps its persistence but breaks its timing, gives a randomisation p-value of 1/43 = 0.023: none of the 42 rotations produces a t-statistic as large as the actual 5.68. What the battery cannot exclude is a confounder sharing the projection's exact profile, fiscal, pensioner-specific, operative only on the controlled margin, and timed to the vintages; none has suggested itself, and closing that residual vulnerability is what the agenda of Section 7 is built for.

[[FIGURE3]]

[[FIGURE4]]

## 6.8 Reading the history through the model

The estimated rule acquires content against the sample's political history, because the projection variable moves at times no party-political account selects (Figure 1). The projected ten-year OADR change first turned positive in the early 1980s, and non-EU inflows rose in 1981–85 under a Conservative government publicly committed to restriction; it turned negative around 1990 and inflows stagnated; it returned to positive territory in 1996 and inflows resumed growth before the change of government. In 2002–03 it averaged roughly 25 points as the first baby-boom wave arrived a decade from pension age, and gross non-EU inflows cleared 300,000 for the first time in 2004. Read through the model, the post-1997 turn was the arrival of the baby boomer at the projection horizon, not of a new ideology: had the 1997 election gone the other way, the model expects the expansion regardless.

# 7. Discussion: External Events and Limitations

*The post-2019 episode.* After Brexit a government elected to reduce migration instead liberalised the non-EU regime (Migration Observatory, 2021), amid a pandemic debt shock and projections of renewed ageing (ONS, 2019); net migration rose from 184,000 in 2019 to over 900,000 in 2023, overwhelmingly non-EU (Migration Observatory, 2026; BBC News, 2025). No structural claim rests on it, but the model predicts the direction of the response and the governing party's stated disposition does not.

*Limitations and agenda.* The zero-migration reconstruction treats all net migrants as working-age, the dependent variable pools entry categories, and outside 2000–2001 the evidence for the mechanism is institutional and statistical. The remedies are an event study of visa grants around NPP releases, the Treasury archives (HM Treasury, 1997), and a cross-country panel on UN projection vintages.

# 8. Conclusion

This paper asked whether the politically homeless expansion of controlled immigration to the United Kingdom between 1977 and 2019 can be explained by fiscal policy. The government's own ageing projections predict non-EU inflows, the margin policy controlled, with a short-run response of 0.8 per cent per projected point of ten-year OADR deterioration and a bias-corrected long-run response of approximately 2.0 per cent. The relationship is absent where the fiscal account says it should be: for the fiscally cheap total dependency ratio, and for EU inflows, which no British government could ration.

The finding bears on a question larger than migration policy. Democracies facing slow-moving fiscal pressures possess instruments that differ enormously in visibility, from taxes announced from a despatch box to entry rules amended by statutory instrument on a quiet parliamentary afternoon, and the British case suggests that consequential policy can flow through the least visible instrument for decades precisely because it is least visible. As the demographic transition arrives across the rich world, understanding when governments reach for quiet instruments, and what that choice does to democratic accountability, is among the more consequential questions the political economy of public finance can ask.

[[TITLENOTE]]

[[REFS]]

Bauer, V. and Salt, J. (2020). *Managing Foreign Labour: Immigration to the UK*. London: UCL Migration Research Unit.

BBC News (2025). 'Five key impacts of Brexit five years on', 31 January.

Berry, R. and Berry, C. (2013). 'Has the Office for Budget Responsibility achieved genuine independence from government?', *Democratic Audit*, London School of Economics.

Borjas, G. J. (1987). 'Self-selection and the earnings of immigrants', *American Economic Review*, 77(4), pp. 531–553.

Cabinet Office (2010). *Migration, an Economic and Social Analysis: Freedom of Information Release* (drafts of July, September, October and November 2000, Performance and Innovation Unit and Home Office). London: Cabinet Office.

Centre for Social Investigation (2018). *People's Stated Reasons for Voting to Leave the EU*. Oxford: Nuffield College.

Clifford, C. (1997). 'The rise and fall of the Department of Economic Affairs 1964–69: British government and indicative planning', *Contemporary British History*, 11(2), pp. 94–116.

Dell, E. (1991). *A Hard Pounding: Politics and Economic Crisis, 1974–1976*. Oxford: Oxford University Press.

Dustmann, C., Vasiljeva, K. and Piil Damm, A. (2019). 'Refugee migration and electoral outcomes', *Review of Economic Studies*, 86(5), pp. 2035–2091.

Economic Statistics Centre of Excellence (2026). *Historical Fiscal Data*. London: ESCoE.

Eich, F. (2008). 'Five years of the UK's Long-term Public Finance Report: has it made any difference?', in *Fiscal Sustainability: Analytical Developments and Emerging Policy Issues*, Banca d'Italia Public Finance Workshop, pp. 687–706.

Farid, S. and Mostari, M. (2022). 'Population transition and demographic dividend in Bangladesh: extent and policy implication', *Journal of Social and Economic Development*, 24(1), pp. 108–126.

Federal Reserve Bank of St. Louis (2025). *Long-Term Government Bond Yields: 10-Year: United Kingdom* (IRLTLT01GBA156N) and *3-Month Treasury Yields: United Kingdom* (IR3TTS01GBA156N). FRED database.

Full Fact (2018). 'How much of the NHS budget is spent on people over 85?', 6 February.

Glover, S., Gott, C., Loizillon, A., Portes, J., Price, R., Spencer, S., Srinivasan, V. and Willis, C. (2001). *Migration: An Economic and Social Analysis*, RDS Occasional Paper No. 67. London: Home Office.

Government Actuary's Department and Office for National Statistics (1975–2018). *National Population Projections, 1975-based to 2018-based vintages*. London: GAD/ONS; National Archives web archive (see ONS, 2016b and Online Appendix F).

Guardian, The (2018). 'Revealed: immigration rules have more than doubled in length since 2010', 27 August.

Hansard (1929). House of Commons Debates, 15 April 1929: Budget statement (disposal of surplus).

Hansard Society (2014). *The Devil is in the Detail: Parliament and Delegated Legislation*. London: Hansard Society.

Hatton, T. J. (2005). 'Explaining trends in UK immigration', *Journal of Population Economics*, 18(4), pp. 719–740.

Hatton, T. J. and Wheatley Price, S. (1999). 'Migration, migrants and policy in the United Kingdom', IZA Discussion Paper No. 81. Bonn: Institute for the Study of Labor.

Hatton, T. J. and Williamson, J. G. (2002). 'What fundamentals drive world migration?', NBER Working Paper No. 9159. Cambridge, MA: National Bureau of Economic Research.

Heclo, H. and Wildavsky, A. (1974). *The Private Government of Public Money: Community and Policy inside British Politics*. London: Macmillan.

Hemming, R. and Kay, J. A. (1982). 'The costs of the State Earnings Related Pension Scheme', *The Economic Journal*, 92(366), pp. 300–319.

Higgs, H. (1924). 'Treasury control', *Public Administration*, 2(2), pp. 122–130.

HM Treasury (1984). *The Next Ten Years: Public Expenditure and Taxation into the 1990s*, Cmnd 9189. London: HMSO.

HM Treasury (1997). *Forecasts for the UK Economy*. London: HM Treasury.

HM Treasury (1998). *The Code for Fiscal Stability*. London: HM Treasury.

HM Treasury (1999). *Budget 99: Building a Stronger Economic Future for Britain. Economic and Fiscal Strategy Report and Financial Statement and Budget Report*, HC 298. London: The Stationery Office. Annex A, 'Illustrative long-term fiscal projections', pp. 87–93.

HM Treasury (2026). *GDP deflators at market prices, and money GDP: March 2026 (Spring Statement)*. London: HM Treasury.

Home Office (2006). *A Points-Based System: Making Migration Work for Britain*, Cm 6741. London: The Stationery Office.
Home Office (2008). 'Memorandum by the Home Office', in House of Lords Select Committee on Economic Affairs, *The Economic Impact of Immigration, Volume II: Evidence*, HL Paper 82-II. London: The Stationery Office.

Home Office (2026). *Immigration Rules: Statements of Changes* (collection). London: Home Office.

Hood, C., King, M., McLean, I. and Piotrowska, B. M. (2023). *The Way the Money Goes: The Fiscal Constitution and Public Spending in the UK*. Oxford: Oxford University Press.

House of Commons Library (2019). *Acts and Statutory Instruments: The Volume of UK Legislation 1850 to 2019*, Briefing Paper CBP-7438. London: House of Commons.

House of Commons Library (2025). *NHS Staff from Overseas: Statistics*, Briefing Paper CBP-7783. London: House of Commons.

Hurwicz, L. (1950). 'Least-squares bias in time series', in Koopmans, T. C. (ed.), *Statistical Inference in Dynamic Economic Models*. New York: Wiley, pp. 365–383.

House of Commons Treasury Committee (2001). *Government Actuary's Department: Minutes of Evidence, 8 February 2001*, HC 236, Memorandum from the Government Actuary. London: The Stationery Office.

Institute for Fiscal Studies (2024). 'What does the government spend money on?', *TaxLab*. London: IFS.

Institute for Government (2024a). *Treasury Orthodoxy*. London: Institute for Government.

Institute for Government (2024b). 'The orthodoxy of the Treasury's outsized power in government'. London: Institute for Government.

Jones, L. Y. (1980). *Great Expectations: America and the Baby Boom Generation*. New York: Coward, McCann and Geoghegan.

Kendall, M. G. (1954). 'Note on bias in the estimation of autocorrelation', *Biometrika*, 41(3–4), pp. 403–404.

López, T. M. (2014). *The Winter of Discontent: Myth, Memory, and History*. Liverpool: Liverpool University Press.

Mayda, A. M. (2007). 'International migration: a panel data analysis of the determinants of bilateral flows', CReAM Discussion Paper 07/07. London: Centre for Research and Analysis of Migration.

Mendoza, E. G., Razin, A. and Tesar, L. L. (1994). 'Effective tax rates in macroeconomics: cross-country estimates of tax rates on factor incomes and consumption', *Journal of Monetary Economics*, 34(3), pp. 297–323.

Migration Observatory (2021). *The UK's 2021 Points-Based Immigration System: Policy Primer*. Oxford: University of Oxford.

Migration Observatory (2026). *Long-Term International Migration Flows to and from the UK*. Oxford: University of Oxford.

Mitchell, J. and Pain, N. (2003). 'The determinants of international migration into the UK: a panel based modelling approach', NIESR Discussion Paper No. 216. London: National Institute of Economic and Social Research.

Mitchell, J., Pain, N. and Riley, R. (2011). 'The drivers of international migration to the UK: a panel-based Bayesian model averaging approach', *Economic Journal*, 121(557), pp. 1398–1444.

Shaw, C. (2007). 'Fifty years of United Kingdom national population projections: how accurate have they been?', *Population Trends*, 128, pp. 8–23.

Storesletten, K. (2000). 'Sustaining fiscal policy through immigration', *Journal of Political Economy*, 108(2), pp. 300–323.

The National Archives (1984, 1990, 1996). PREM 19/1190: Prime Minister's Office, economic policy (public spending), 1984; LAB 109/66: Department of Employment, General Manpower Division (Overseas Labour): Review of the Work Permit Scheme, former reference OL 106/1990, 1990; T 733: HM Treasury, Personal Tax, Pensions and Savings, born-digital records ZVP4K, ZVP3L, ZVP3F and ZVP3D, 1996. Kew: The National Archives.

Natsiopoulos, K., Tzeremes, N. and Finnan, D. (2026). *ARDL: ARDL, ECM and Bounds-Test for Cointegration*. R package version 0.2.5.

O'Brien, R. M. (2007). 'A caution regarding rules of thumb for variance inflation factors', *Quality & Quantity*, 41(5), pp. 673–690.

Office for Budget Responsibility (2014). 'Migration and fiscal sustainability', in *Fiscal Sustainability Report*. London: OBR.

Office for Budget Responsibility (2026). *Historical Public Finances Database*. London: OBR.

Office for National Statistics (2015). *Long-Term International Migration Estimates: Methodology*. Newport: ONS.

Office for National Statistics (2016a). *Note on the Difference between National Insurance Number Registrations and the Estimate of Long-Term International Migration*. Newport: ONS.

Office for National Statistics (2016b). *National Population Projections, Historic Series*. National Archives web archive.

Office for National Statistics (2019). *Living Longer and Old-Age Dependency: What Does the Future Hold?* Newport: ONS.

Office for National Statistics (2020). *Long-Term International Migration 2.00: Citizenship, UK, 1964 to 2019* [dataset]. Newport: ONS.

Office for National Statistics (2023). *International Passenger Survey: Methodology*. Newport: ONS.

Office for National Statistics (2026). Time series: economic inactivity rate (LF2S); unemployment rate (LF2Q); gross domestic product, chained volume measure (ABMI); output per worker. Newport: ONS.

Pemberton, H. (2018). *The Fowler Inquiry into Provision for Retirement and the Pension Reforms of 1986: Witness Seminar Transcript*. Bristol: University of Bristol Research Portal.

Pesaran, M. H., Shin, Y. and Smith, R. J. (2001). 'Bounds testing approaches to the analysis of level relationships', *Journal of Applied Econometrics*, 16(3), pp. 289–326.

Portes, J. and Forte, G. (2017). 'The economic impact of Brexit-induced reductions in migration', *Oxford Review of Economic Policy*, 33(S1), pp. S31–S44.

UK Parliament (2026). 'The Budget and Parliament'. London: UK Parliament.

United Nations (2000). *Replacement Migration: Is It a Solution to Declining and Ageing Populations?* (ESA/P/WP.160). New York: Population Division, Department of Economic and Social Affairs.

Walsh, C. (2021). 'How independent is the OBR, and why does it matter?', Working paper, Cardiff University.

Wamsley, D. (2026). 'When neoliberalism meets technocracy: intra-elite conflict and the realignment of Britain's macroeconomic regime under Liz Truss', *New Political Economy*, 31(3).
