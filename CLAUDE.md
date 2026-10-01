# Project rules — Sam Hamdane (Quiet Instruments / ReStud manuscript & Humboldt thesis)

## RULE 0 — Every uploaded thesis/manuscript copy IS the new foundation
Standing instruction (Sam, 12 Sep 2026): whenever Sam uploads a copy of the
thesis or manuscript, that copy becomes the new foundation. Diff it against the
current version first; carry recent pipeline-side work forward onto HIS copy,
never the reverse. If the upload appears to be built on a stale base (earlier
merges missing), say so and adjudicate rather than silently adopting either.

## RULE 1 — NEVER reverse manual edits without consent (hardcoded, non-negotiable)
When Sam uploads or supplies a document he has edited by hand, his edits are the
foundation. Do NOT revert, "improve", or overwrite any of them — including
wording, layout, line breaks, spacing, capitalisation and punctuation choices —
without asking him FIRST and receiving explicit consent, item by item.
This applies even when:
- the edit contradicts earlier advice he accepted, or an earlier instruction
  (his latest manual edit wins; if in doubt, ask);
- the edit looks like an accident or artifact (ask — do not assume);
- the edit appears to be a factual mistake (flag it and ask; correct only after
  a yes).
After every merge, disclose EVERY divergence between his copy and the produced
document, stating which side won and why. Purely typographic normalisations
(curly quotes, en dashes) may be applied but must be disclosed and are
reversible on his word.
History: this rule exists because manual edits were once silently reverted with
only partial disclosure. Do not repeat that.

## Standing document facts (September 2026)
- Canonical pipeline: outputs/build_docx.js + manuscript_part1.md /
  manuscript_part2.md; appendix via build_online_appendix.js. Manuscript must
  stay ≤ 45 pages. CURRENTLY 47 (14 Sep 2026): Sam added the principal-agent
  paragraph to §2.6 knowingly and will make the offsetting cuts himself —
  do not trim anything for him unasked.
- Title (Sam's choice, size 12pt ALWAYS, single line, Oxford comma): "Death,
  taxes, and immigration: how Britain’s Treasury delayed the inevitable
  (1977–2019)" for the MANUSCRIPT (Sam, 22 Sep 2026: manuscript sample moved to
  1977–2019, N = 43; the THESIS stays on 1979–2019). Manuscript figures come
  from the *_1977.R scripts (tables_1977.json); thesis figures from the canonical
  scripts (tables.json). period_1977.py applies the period wording to the
  45pp/30pp variants.
- Abstract: Sam's ~150-word version, one paragraph; installed verbatim from his
  latest wording. Manuscript and thesis go to different institutions and MAY
  differ (Sam, 22 Sep 2026): manuscript says "nearly quintupled" (1977-2019),
  thesis keeps "tripled" (1979-2019) — do not "fix" it without
  asking (he declined fixes to "social-security", "lags;", "1970s-onwards").
- Oxford commas: house style everywhere in prose and headings/title, EXCEPT
  citation author lists (Harvard style) and quotations, which never change.
  (Thesis headings briefly had the commas removed on 12 Sep 2026, then restored
  the same day — Oxford commas apply in BOTH documents' headings.)
- Thesis draft: first-person singular voice (We/our/us → I/my/me applied
  throughout; one "us" retained inside a historical quotation).
- Quotations from sources are never altered.
- Accessed dates (REVISED, Sam 22 Sep 2026): in the thesis, ONLY web pages,
  news articles, datasets and official/online publications carry an accessed
  date, randomly "(Accessed: 20 June 2026)" or "(Accessed: 21 June 2026)".
  Academic sources (journal articles, books, chapters, working papers) carry
  NO accessed date. Uncited references are removed.
- Reference indentation (NEW RULE, Sam 15 Sep 2026): every thesis reference
  entry carries the list's hanging indent (left 457200 EMU, first-line
  -457200) so continuation lines are indented; apply to ALL new entries.
- Replication folders (22 Sep 2026): `diss_replication/` is CANONICAL (single-
  source fiscal block from the OBR Historical Public Finances Database + HMT
  March 2026 deflators; dataset Dissertation_Dataset_Final.xlsx; scripts and
  tables.json there feed every document). `archive_march2026/` holds the
  superseded March-databank run. Model objects are named tax_spec / trend_spec.
- Variable naming (Sam, 22 Sep 2026): the debt regressor is the ANNUAL LOG CHANGE
  in debt/GDP; it is called `ln_debt_gdp` everywhere in the code (no `d_` prefix)
  and `L_ln_debt_gdp` when lagged. Text must describe it as a log change, never a
  log level. The 1975-based NPP vintage is now held; dataset rows 1975-78 carry
  the DR/zero-migration/pub-dated columns from the 1975 and 1977 vintages.
- Sample start (22 Sep 2026): 1977 is the earliest estimable start for the full
  specification (L_ln_debt_gdp needs debt/GDP for 1975 and 1976; HPFD debt starts
  1974-75). THESIS estimation sample stays 1979-2019 (1977 start reported as
  robustness, fiscal 0.0082, N = 43); MANUSCRIPT sample is 1977-2019. R package ARDL (Natsiopoulos, Tzeremes and Finnan,
  2026, v0.2.5) is cited wherever the bounds test is described.
- Manuscript restructure (Sam, 22 Sep 2026): full manuscript (manuscript_part1/2.md)
  now has the headline result + Figure 1 in the introduction, a condensed Section 2
  (full text in Online Appendix E), Proposition 1 in Section 3 (proof in Online
  Appendix D), Figures 3 (horizons) and 4 (robustness summary), and the timing-
  placebo and second-lag paragraphs in Section 7 (manuscript_extras_1977.R).
  Online Appendix: A, B, C, D proof, E background, F digital references.
  The 45pp/30pp variants are assembled from the FROZEN pre-restructure sources
  manuscript_part1/2_variantsrc.md until Sam approves propagating the restructure.

- Replication README (Sam, 23 Sep 2026): `diss_replication/README.md` is a LIVING document
  (Social Science Data Editors template). Update it, and its change log, whenever data,
  scripts, exhibits or headline results change.
- Thesis front matter (Sam, 23 Sep 2026, per HU Economic History guidelines): cover (degree
  "M.Sc. Economics", supervisor Professor Nikolaus Wolf, second supervisor Professor Michael Burda) -> abstract -> Contents -> List of
  Figures -> List of Abbreviations, all in section 1 with upper-Roman page numbers (cover
  unnumbered); section break before the Introduction, which is page 1. Contents/figure-list
  page numbers are PAGEREF fields to _Toc_h*/_Fig* bookmarks. Do NOT set updateFields (Word
  updated them before pagination and showed '1' everywhere); cached values come from the
  LibreOffice render and Sam refreshes with Select All + F9. List of Figures and List of
  Abbreviations share ONE page (entries 11pt, as Sam set the Contents). Figures
  are numbered by chapter (5.1, 5.2, 8.1-8.3); add any new figure to the List of Figures.
- Tax-burden measures (Sam, 23 Sep 2026): headline is NAT/WAP (national accounts taxes per
  working-age person). Sam's OADR-denominated measure is L_tax_oadr_0 = tax in GBP bn divided by
  the lagged CURRENT OADR - never tax per WAP divided by OADR (that construction was wrongly
  assumed twice). Do not invent denominators or measures Sam has not asked for; ask first.
  The 18-way grid (tax_grid.R, 1979) is in thesis Appendix A as a robustness check.
- F-test convention (Sam, 23 Sep 2026): THESIS appendix keeps the classical nested F ('F vs no
  proj'), labelled as classical in each panel footer (text quotes those values). MANUSCRIPT
  scripts (appendix_exhibits_1977.R, _labpop_1977.R) report HAC Wald F (hac_F); manuscript text
  now quotes HAC Wald F(1,37) = 32.23 (projection) and 2.11, p = 0.16 (total-DR placebo).
- Thesis Appendix A (Sam, 23 Sep 2026, upload ba2dfd25): Sam's OWN R screenshots; sections now Baseline, 18-way tax grid, OADR vs DR, Level vs Delta, Gross, EU, Zero-migration, Policy dummies, Diagnostics, Stationarity, Bounds. The per-capita/OADR/ratio/taxpayer tax panels were removed by Sam (grid replaces them). Do not re-insert rendered panels.
- Thesis structure (Sam, 23 Sep 2026): 6.2 opens with the estimating equation; DV construction paragraph lives in 7.1; 7.3 (IV proxy) shortened; tax-measure robustness paragraph moved to Section 9 under 'Tax burden measures'; candidate list is Table 7.1 (bookmark _Tab7_1), listed under 'List of Figures and Tables' (contents entry 'List of Figures, Tables, and Abbreviations.'). Healey (1990, Penguin paperback) reference; cited in §5.2.4.
- Thesis (23 Sep 2026, upload 713bf057 adopted): Table 7.2 descriptive statistics (bookmark _Tab7_2, 1979-2019, N=41) follows Table 7.1; §5.2.4 Dell section rewritten by Sam (upload abccc5ac, highlight removed by him); publication-dating check removed by Sam (¶307 no longer promises it). Population figures (workpop/totpop) are NPP estimates, cited as GAD and ONS (1975-2018).
- Thesis section order (Sam, 23 Sep 2026): 1 Introduction, 2 Literature Review, 3 Demographics/Dependency/Migration, 4 Documentary Evidence (auto-numbered list numId 20, startOverride 4), 5 Treasury, 6 Methodology... Roadmap in ¶115 updated accordingly.
- Section 4 (Sam, 23 Sep 2026): new 4.3 "The Government Actuary’s horizon, 1974–1982" (Hemming and Kay,
  1982; bookmark _Toc_h18a) between the 2000 report and the 1984 Green Paper; old 4.3–4.6 are now 4.4–4.7.
  Framing: the projection HORIZON hid SERPS costs — H&K do not allege falsification; never claim they do.
  §7.4 keeps a one-line pointer. SERPS added to the List of Abbreviations.
- Identification (Sam, 23 Sep 2026): 6.3 now states the identifying assumption + threats
  (Sam's zero-migration paragraph kept verbatim); his sample-start paragraph moved verbatim to
  the end of 7.1. Section 9 gained 'Realised spending' (spending_check.R; Sheet2 health_gdp
  corrected) and 'Other determinants of migration' (determinants_check.R). NPP vintages are
  used by BASE year (vintage_for(y) = latest base <= y), NOT publication date — never claim
  the projection was published before the year it explains.
- Fluff pass (Sam, 23 Sep 2026): Thesis_cuts_for_approval.md lists proposed cuts; rows marked
  APPLIED were approved (IV repeats, textbook test explanations, named tangents, signposting,
  #30, #134). Remaining rows (Treasury/OBR/Truss/DEA repeats etc.) are NOT approved — ask.
  One-year horizon p = 0.57 (fiscal, 0.566) is correct; the 0.56 in Section 9 was removed.
- Dell (1991) checked against Sam's OCR'd copy (24 Sep 2026; Dell_1991_research_brief.md): Dell was
  Trade Secretary (Cabinet) from Apr 1976, Paymaster General Mar 1974-Apr 1976; the book does NOT
  support "misrepresented"/"admitted" (PSBR £10.5bn forecast vs £8.5bn outturn, blamed on unknown
  underspending, p.274; deliberate-overstatement charge is Healey's, disputed by Dell, p.185).
  §5.2.4 wording is Sam's — flagged, not changed; await his decision.
- Dell additions (Sam, 25 Sep 2026; page refs on Sam's reading of the book, accept them):
  §6.1 BoP-exclusion ¶ cites Dell p.4 (Bretton Woods dead by Mar 1974, same year as Heclo &
  Wildavsky); §5.1.2 'Insiders concur' sentence (Dell p.24); §5.1.3 Benn "written by the Treasury
  and not by Ministers" (quoted in Dell, 1991, p.34). Barnett (1992) review not cited.
- Citation style (Sam, 25 Sep 2026): NO page numbers in in-text citations (removed from the
  Dell and Hemming & Kay citations). §5.1 Treasury origins now also cites Healey (1990): Richard,
  appointed 1170, first 'Treasurer of England', running the Exchequer.
- 5.1.2 (Sam, 25 Sep 2026): Dell detail cut to one clause ("felt on both sides of the divide
  (Dell, 1991)") followed by the Benn quote; the Benn quote was removed from 5.1.3 to avoid
  repeating it (5.1.3 keeps the Cabinet-saw-it-hours-before point).
- Items 4/5/7 (Sam, 25 Sep 2026): VIF was ALREADY addressed in thesis ¶449 (my item 4 was wrong).
  Thesis 8.2 now has the one-SD magnitude sentence (¶ Delta_OADR_10) and the Kendall/bootstrap
  sentence (¶ lagged migration rate; Kendall 1954 added to references). Manuscript §6 got the
  one-SD sentence (outputs/manuscript_part2.md, rebuilt Hamdane_ReStud_Manuscript.docx, 49 pp;
  copies in manuscript_variants/). stability_check.R + Guide_policy_lever_and_stability_tests.md
  added; Sam is collecting reason-for-migration data (test 2) and work permits (test 1).
  Item 6 (fiscal-impact literature) declined by Sam: OBR evidence + §3.2 suffice.
- 25 Sep 2026 (later): §5.2.4 gained a Rogers (2010) precedent paragraph (Wass/£6 pay norm,
  parsimonious sterling intervention; reference added). Section 9 gained 'Stability over time'
  (recursive/rolling/CUSUM/sup-F from Sam's stability_check.R run; script trimming fixed to
  from = 8 / h = 8 after Bai-Perron errored). stability.png NOT yet in Appendix A / List of Figures.
- Work permits (Sam, 25 Sep 2026): series digitised from Bauer & Salt (2020) Fig. 1 into
  determinants/work_permits.csv (WP+FP 1946-2008; Tier 2 2009-18, different basis — never splice).
  Sam declined splicing onto IPS reason data. permits_check.R: permits DO respond to the projection
  (0.0094, p .03; zm 0.0115; DR placebo null) but permits do not absorb the projection in the inflow
  equation (proj 0.0085 vs 0.0094). Not yet written into thesis/manuscript — ask Sam. Sam may email
  Salt/Bauer for raw figures; swap them in if received. Home Office MJ_D01 non-EU initial leave by
  type 2008-25 saved (Work/Study/Asylum; no Family group in that table).
- IPS by reason (Sam, 25 Sep 2026): ONS table 3.08 gives non-EU inflows by main reason from 1977
  (not 1991) -> determinants/ips_nonEU_inflow_by_reason_1977_2019.csv. Composition test does NOT
  give a clean work/study vs family split (all routes respond to some degree; study weak in levels,
  significant in differences; 'other' strongest). Reported honestly; Sam to decide whether to use.
  LTIM 2.00 from 1964 covers all citizenships only; non-EU starts 1975 (Sam already has it); Sam's
  nEU_inflows = IPS 3.08 non-EU (LTIM 2.00 non-EU differs from 1991 by the LTIM adjustments).
- Permits write-up (Sam, 25 Sep 2026): Section 9 'The policy lever' inserted after 'Stability over
  time' (level form reported, logs for consistency; no first differences; no Tier 2 extension).
  appendix_permits.R produces the Appendix A panel (levels | logs | zero-mig | total DR); Sam adds
  the screenshot under 'Policy lever'. Citation audit: all in-text citations now have entries;
  'Davies, 2022' -> 'Davis, 2022'; ONS/OBR full-name citations abbreviated. Sam: thesis stays
  1979 (title); pre-1979 extension (non-British = all - British from 1964) is manuscript work.
- Permits form (Sam, 25 Sep 2026): GROSS permits in thousands is the primary form (Section 9 text
  and appendix_permits.R col 1 'permits (k)'); then permits/WAP, log permits/WAP, zero-mig, total DR
  (unscaled log permits column removed at Sam's request). Column names must be defined/consistent — never 'rate' unqualified.
- Placebos on uncontrolled flows (25 Sep 2026): Sam's DV = table 3.01 non-EU citizens from non-EU
  residence (exact match). British citizens IN from non-EU respond (+0.0070, p .03) — a PARTIAL
  placebo failure; with that flow as a control the projection is 0.0057 (p<.001) (lower bound);
  log-difference spec null. British OUT, EU-from-non-EU, non-EU-from-EU all null. Sam has not yet
  decided how to write this up. Longer horizons (20/25y) null alongside 10y. Sam rejected: cross-
  country (7), age composition (6); asymmetry/OOS/interaction explained, undecided.
- 25 Sep 2026: British-placebo paragraph now IN Section 9 (after EU vs non-EU); 20/25-year horizon
  clause in 8.1. FLAG for Sam: the ONS (2020) reference entry describes LTIM table 2.00, but the DV
  and the placebo come from IPS table 3.01 (citizenship x country of last residence) — his call.
- appendix_british.R (25 Sep 2026): three Appendix A panels for the British/uncontrolled-flow
  placebos + IPS sampling-error lines; Sam pastes screenshots under a 'British citizens and
  uncontrolled flows' heading. Section 9 British paragraph now has the ±17%/±9% sampling clause.
- British placebo wording (Sam, 25 Sep 2026): descent claim softened to "former parts of the Empire
  ... citizens by descent or registration ... likely" and cited to Sriskandarajah and Drew (2006,
  IPPR; reference added, accessed date). No public data on descent vs UK-born by country exists;
  do not state the descent composition as fact.
- Nationality-law sentence (Sam, 25 Sep 2026) added to the Section 9 British paragraph: BNA 1981
  (Tyler, 2010, Citizenship Studies 14(1) — reference added), BN (Hong Kong) Act 1990 (50,000
  households), NIA Act 2002 (BOCs with no other nationality; children of British mothers pre-1983).
  Sam judged the British Overseas Territories Act 2002 not worth citing. Acts are named in text
  only, no reference entries (house practice, cf. Immigration Act 1971).
- 1964 extension (Sam, 25 Sep 2026, MANUSCRIPT work): pre-1975 vintages copied into diss_replication/
  and parsed -> determinants/npp_1963_1974_oadr.csv (year, vintage, totpop, workpop, pension, oadr_0,
  oadr_10, delta_oadr_10, interpolation note). Method in README change log. Sam's '1964' workbook
  sheet was NOT in the saved xlsx (Excel lock file present) — ask him to save. appendix_british.R
  update() bug fixed.
- 1964 sheet (Sam, 25 Sep 2026): 'inflows' 1964-1990 = NON-BRITISH citizens (3.24), not non-EU.
  Extended series + footnote text + DV options in determinants/README_1964_extension.md; Sam to
  choose option (recommended: non-British to 1972, non-EU from 1975). ONS ABMI GDP file added
  by Sam (series-070426.xls). Manuscript 1964 section not yet written.
- 1964 extension results (25 Sep 2026, replica): dataset determinants/extension_1962_2019_dataset.csv;
  spliced DV B (non-British to 1974 / non-EU from 1975) proj 0.0080-0.0081 on 1965-2019 (N=55), C
  (non-British to 2003) 0.0081, A (non-British to 2019) 0.0054. Deflator pre-1975 = HMT March 2026 series itself
  (runs from 1955-56; uploads/GDP_Deflators_Spring_Statement_March_2026_update.xlsx); unemployment pre-1975 from BoE A50 (joins LF2Q within 0.2pp).
  PSF databank '2024-25 prices' sheet NOT usable as deflator. R script for the extension still to write.
- extension_1964.R (25 Sep 2026): reads sheet '1964' (Sam pasted the extension CSV there; year blank for
  1962/63 rows -> filled by sequence in R; column B empty). Panel A | B | B+step | C | non-EU 1979-2019 |
  A 1965-78 | A 1979-2019. totpop added to extension dataset; npp_1963_1973_totals_by_vintage.csv holds
  total/WAP/pension for every published projection year of the 1963-1973 vintages.
- Thesis upload df5f7f95 adopted (25 Sep 2026): Sam added Appendix A 'Work permit reconstruction' (1 panel)
  and 'British citizen migration' (3 panels) screenshots and removed blank paragraphs after the conclusion
  and the references. His copy pre-dated the IPPR softening and nationality-law sentence in the Section 9
  British paragraph; Sam chose to re-apply both (+ Tyler and Sriskandarajah references). appendix_british.R
  Panel 2 notes cut to one line at Sam's request (explanation belongs in the text, not the appendix).
  Workbook '1964' sheet now saved with years filled and totpop; extension_1964.R reads it directly.
- Thesis upload 38fa2ec4 adopted (25 Sep 2026): only change = ONS (2020) reference now IPS table 3.01
  (Sam's wording: 'International Passenger Survey 3.01: Citizenship, by Country of Last or Next Residence UK,
  1975 to 2019 [dataset]. Newport: ONS.' — no URL, full stop after year; format suggestions offered, not
  applied). LTIM 2.00 entry no longer in thesis; ¶546 IPS-vs-LTIM robustness carries no citation.
  extension_1964.R confirmed in Sam's R: identical to replica.
- ONS 2020a/b (Sam, 25 Sep 2026): thesis references now 'Office for National Statistics (2020a) International
  Passenger Survey 3.01, citizenship by country of last or next residence, UK, 1975 to 2019. Available at: [ONS
  URL] (Accessed: 20 June 2026)' and '(2020b) Long-term international migration 2.00, citizenship, UK, 1964 to
  2019. Available at: [ONS URL] (Accessed: 21 June 2026)'. In-text: 2020a in ¶128, Table 7.2 source note, and the
  British paragraph; 2020b added in the IPS-vs-LTIM paragraph (¶546). Sam's R run of extension_1964.R = replica.
- 1964 extension, MANUSCRIPT NOTES (Sam, 25 Sep 2026; full list in diss_replication/determinants/
  README_1964_extension.md 'Manuscript notes'): headline = series B (+ step column) 0.0080/0.0081, N 55;
  C as pre-accession check; A only to show EU dilution. Step = definitional change (long-run 0.25 = 1975-80 EU
  share). Claim 'unchanged when extended to 1965', NOT 'visible in the 1960s' (1965-78 projection collinear
  with time, r = -0.97; trend alone kills the short-sample coefficient). Strength: 14 years under a different
  regime/DV basis/unemployment source/deflator/projection construction, coefficient moves 0.0001; sample now
  spans net emigration and net immigration. VIF caveat for A 1979-2019 (38) and the N=14 column (19).
  extension_1964_robustness.R + determinants/extension_1962_2019_extras.csv: trend/Act steps, total-DR placebo
  (extended to 1963), growth/inflation/British emigration/network, starts 1965-79, sub-samples; projection
  0.0078-0.0087 throughout; total DR alone 0.0015 (p .001) on the extended sample but 0.0004 (p .48) beside the
  OADR — report both. Maddison/UNWPP determinants NOW extended to 1963 (maddison_gdppc.csv, unwpp_youth_share.csv rebuilt; extras
  carry relinc_dev/relinc_rich/youth_origin); Panel 3b: projection 0.0078-0.0094 with them.
- Robustness R run confirmed (Sam, 25 Sep 2026) = replica. Sam's framing: relinc/youth/network are SUPPLY-SIDE
  determinants, not policy variables — present them in the manuscript as supply-side controls (omitted-variable
  check), never as mechanism. Trend columns: VIF 63-74, sign check only. DR-alone column fails BG(1).
- MANUSCRIPT 1964-2019 VERSION (Sam, 26 Sep 2026): new document quiet-instruments/Hamdane_ReStud_Manuscript_1964.docx
  (57 pp; sources, builder, tables_1964.json, figures and Python replica in manuscript_1964/; README there lists
  status). Title "(1964–2019)"; headline = series B on 1965-2019, N = 55, basis step reported; abstract edited.
  The 1977-2019 manuscript (outputs/manuscript_part1/2.md, Hamdane_ReStud_Manuscript.docx) is UNCHANGED.
  Provisional numbers = Python replica; diss_replication/manuscript_1964.R is the authoritative run (writes
  tables_1964.json incl. bounds test via ARDL with basis step as fixed regressor) -- Sam to run, then rebuild.
  Table 3 + Figure 3 still 1977-2019 (flagged in text). Cuts to 45 pp are Sam's. Bias correction re-run:
  rho 0.46 -> 0.56, long run 1.5 -> 1.8 per cent [1.2, 2.6].
- 26 Sep 2026: Sam ran manuscript_1964.R; tables_1964.json (R) adopted everywhere (identical to replica; bounds
  1965-2019: F = 7.18 p .003, t = -4.95 p .007; LR projection 0.0155). Manuscript_1964 rebuilt (57 pp): 6.3 rewritten,
  Table 3 now 1965-2019, flags removed. Figure 3 (horizons) still 1977-2019 by design (stated in text).
  Thesis conclusion FULL REDRAFT delivered as Thesis_Conclusion_redraft_26Sep2026.md (NOT installed; awaits Sam's
  approval; thesis numbers 1979-2019; refers to Sections 4, 5, 11).
- Brittan (1969) Steering the Economy (Sam, 26 Sep 2026; upload IMG_2833.pdf, 173 spreads OCR'd -> Brittan_1969_
  Steering_the_Economy_OCR.txt; brief Brittan_1969_research_brief.md, page refs from running heads). Book has NO
  mention of immigration/ageing/GAD -- never cite it for demographic interest; use for institutional character,
  secrecy (forecasts unpublished until 1968; 5-yr projections published only 1963/1966; 'Cabinet itself hears the
  Budget only the day before'; 'Budget secrecy has become a cult'), ministers unable to question analysis, DEA
  defeat ('power gradually shifted back to the Treasury'), medium-term projections with working population as
  input. Brittan warns against a monolithic reading ('a collection of individuals') -- thesis text says so.
  Reference: Brittan, S. (1969) Steering the economy: the role of the Treasury. London: Secker & Warburg
  (imprint page missing from scan; Sam to confirm edition/year). Added to thesis 5.1.2 (new ¶ after Benn quote),
  5.1.3 (sentence), 5.2.2 (DEA sentence), 6.1 (forecasting sentence after Hood et al.) + reference; and to both
  manuscripts (2.1 new ¶, 2.2 sentence, 2.4 sentence, reference). Backup outputs/Thesis_before_brittan.docx.
- COMBINED MANUSCRIPT (26 Sep 2026): Hamdane_ReStud_Manuscript_combined.docx (58 pp; builder manuscript_1964/
  build_docx_combined.js; sources ms1964/manuscript_part1/2_combined.md): title 1964-2019; Section 4 defines a CORE
  sample 1977-2019 (N 43) and an EXTENDED sample 1965-2019 (N 55); Table 2 four columns (fiscal/trend x samples),
  Table 3 both samples' LR multipliers and bounds; Table 4/Figures as the 1964 document. Text quotes both
  estimates (0.0082 / 0.0081; LR 2.0 [0.9,2.8] / 1.8 [1.2,2.6]). 1977 manuscript rebuilt with Brittan only (49 pp).
  Replication: run_all_1964.R, run_all_thesis_extras.R; README section 'Documents and the scripts that feed them'.
- BoE Millennium data (Sam, 26 Sep 2026): annual averages built (determinants/boe_millennium_annual_1955_2017.csv;
  boe_extras_1962_2019.csv aligned to the extension). Admin unemployment = M5 col C to 1995 + col E claimant count
  from 1996; vacancies M5 AB; AWE/wage inflation M6 Z/AA (Sam said M7 -- it is M6 in v3.1); T-bill M1 N. ALL END
  2016 -> tests run 1965-2016 / 1977-2016; 2017-19 need ONS BCJE, AP2Y, KAB9 + BoE bill rate (Sam to add to CSV).
  Sam's own additions in the 1964 sheet: lt_inf_exp, st_inf_exp, tfp_growth. extension_1964_boe.R (3 panels) in
  run_all_1964.R. Manuscript (1964 + combined) Section 7 paragraph added; replica numbers pending Sam's R run.
  Findings: projection 0.0064-0.0087 throughout; vacancies/WAP (+0.015 p .02) and ln V/U (+0.11 p .01) matter
  when substituted for unemployment (proj 0.0067/0.0071); wage inflation -0.007 (p .06); rates, expectations,
  TFP, replacement ratio, union density null. Other BoE variables saved but untested: days lost, current grants
  to persons/TME (realised welfare spending from 1900), labour productivity per hour.
- BoE tests v2 (Sam, 27 Sep 2026): boe_tests.R (SAMPLE 1964/1977; wrappers extension_1964_boe.R, core_1977_boe.R).
  PRIMARY sample ends 2016 (BoE data end); SECONDARY to 2019 auto-runs once ONS 2017-19 values are in
  boe_extras_1962_2019.csv. 'Admin' unemployment = M5 col G spliced (primary), C-to-1995/E-from-1996 for
  reference. Added: gross inflows + lagged emigration (LTIM all/British per WAP) -> null; dln V/U beside
  unemployment 0.068 (p .05), proj 0.0077; unem + vac/WAP: vac 0.012 (p .08), proj 0.0071; A27 tax variants
  (determinants/boe_a27_tax_variants_1956_2019.csv, FY t-1/t -> t): NICs 0.0092, inc tax 0.0079, direct 0.0079,
  receipts 0.0074, indirect 0.0055, VAT 0.0072 (1975+), corporation 0.0062. Errors fixed: constant columns
  (step when VAT restricts sample) now dropped in fit(); empty merge when extras already pasted in sheet.
  Manuscript paragraph (1964 + combined) rewritten with these; replica numbers pending Sam's R run.
- boe_tests.R R runs confirmed (Sam, 27 Sep 2026) = replica on both samples. Key readings: vacancies/WAP is the one
  control that lowers the projection (0.0080->0.0067 ext; 0.0081->0.0059 core) and has the best AIC in panel 1;
  unem + dln V/U best HAC F. Tax term on the core sample loses significance with all six controls (VIF 34-46).
  Denominator question raised by Sam (tax/WAP and DV share WAP(t-1)): answered via gross-inflow column (tax term
  survives without any population on the LHS) and tax per resident; suggested Kronmal fix (add ln WAP as regressor).
- 27 Sep 2026 (later): Sam: inflation expectations (NIESR, start-of-year) enter CONTEMPORANEOUSLY, never lagged
  (boe_tests.R updated; with that, core all-six model: ST exp +0.034 p .005, wage infl -0.027 p .01, proj 0.0105).
  Sam: do NOT chase ONS 2017-19 values; primary = to 2016 (secondary machinery dormant). Permits cannot go before
  1963 (earliest vintage). boe_appendix.R = Tables B6/B7; manuscript_1964.R Figure 4 gained 3 rows (Sam must re-run
  and I re-render figure4_1964.png via pyrep/m1964_figs.py). Employee NICs (0.70) >> employer (0.37); EU placebo
  responds to vacancies not projection; Kronmal (ln WAP) null. Kronmal (1993) reference added to manuscripts.
- 27 Sep 2026 (final BoE round): Sam's tables_1964.json (24 Figure 4 rows) adopted; figure4_1964.png re-rendered;
  1964 + combined docs rebuilt. boe_appendix.R slimmed at Sam's request: B6 = baseline | +dln V/U | vacancies for
  unem | NICs total | NICs employee; B7 = EU baseline | EU + vacancies | permits | permits + vacancies; unem+vac,
  employer NICs, emp+er and Kronmal printed as one-line 'text-only variants'. VERDICT given to Sam: primary spec
  unchanged (no candidate is both theory-prior and a >~4-AIC improvement on both samples; vacancies and NICs are
  the two substantive robustness results, reported as such).
- 27 Sep 2026: Sam: NICs fund the state pension (NI Fund) -> employee-NICs result is central to the mechanism;
  manuscript paragraph now says so. Sam had intended vacancies a priori (did not know a long series existed), so a
  vacancies variant is NOT post-hoc tuning; proposed (awaiting his yes): Table 2 gains a 'fiscal + vacancies'
  column per sample (6 columns in the combined doc) with its own bounds test, primary text unchanged otherwise.
  Column names in boe_appendix.R: 'log V/U' (= annual change in log V/U, explained in notes), 'vac.(no unem)',
  'EU+vac.', 'permits+vac.'; width 15. Column names must never run together -- widen or shorten.
- 27 Sep 2026: Sam rejects dropping unemployment for vacancies (documented Treasury input vs not; p-hacking risk).
  Agreed. The 'both' model is unemployment + dln V/U (level of unemployment = political cost; change in tightness
  = labour demand): ext proj 0.0077, dln V/U 0.068 (p .05), unem -0.019 (p .04), best HAC F; core 0.0077, dln V/U
  0.090 (p .12), unem -0.018 (p .13). Levels of unem + vac/WAP are collinear (V/U inverse by construction) so both
  go insignificant together -- never report that as 'both matter'. If a Table 2 column is added it is
  'fiscal + dln V/U', unemployment retained. Awaiting Sam's decision.
- 27 Sep 2026 FINAL: no Table 2 change. Fiscal + dln V/U judged NOT an improvement (AIC +1.7 ext / +1.0 core;
  dln V/U p .12 and unem p .13 on core; loses 2017-19; adds a regressor at N=40). Stays as Figure 4 row + B6 column.
  Primary specification unchanged, full stop.
- 27 Sep 2026 (docs sync): 1977 manuscript (50 pp) now has the BoE paragraph on the core sample (1977-2016) before
  Figure 4, + Kronmal and Thomas & Dimsdale references. 1964 + combined paragraphs end with the 'no addition carried
  into the headline; demand share bounded at ~a quarter' sentence. Combined Section 4 now states each sample's claim
  and limits (core = cleaner object, every check; extended = harder test, spliced DV, coarser early vintages,
  1965-78 collinear). Combined = 60 pp, 1964 = 57-58 pp. All three docs + PDFs in quiet-instruments/.
  NOTE: the variant files manuscript_variants/manuscript_part1/2.md were overwritten with the 1977 sources
  (they had been identical copies since 22 Sep); the FROZEN 45pp/30pp sources (*_variantsrc.md) are untouched.
- extra_tests.R (27 Sep 2026): realised spending (A28 grants/TME, whole sample), disputes, election cycle, OBR
  interaction, and the 'without the projection' comparison. Replica: all null; the projection cannot be replaced
  by realised spending (AIC -70 vs -93 ext) or by the six macro/labour controls (no-proj+six AIC -79 ext / -55 core
  vs baseline -93 / -64; RESET rejects without projection .03/.007 -- the misspecification is the projection's
  absence, not a missing macro control). Election-cycle and OBR-era indicators null (post-2010 x proj -0.014 p .13).
  Sam's reporting question answered: 'first to test migration as fiscal instrument' and 'first to use DR projection'
  are one claim (the second is the operationalisation of the first), justified jointly by the falsification design.
- extra_tests.R R runs confirmed (Sam, 27 Sep 2026) = replica on both samples; manuscript paragraph 'Can anything
  else do the projection's work?' (1964 + combined, Section 7, cites Table B8 = Panel 3) stands. Panel 4 added
  (current fiscal balance: PSNB/GDP, PSND/GDP, tax/GDP, t-1, to 2019): all null beside the projection (0.0078-0.0088);
  PSNB INSTEAD of the projection has the WRONG sign (-0.024/-0.028, higher deficit -> fewer inflows) and fails RESET
  -> the non-demographic 'close the deficit' story does not carry the result. Sam's question (is the baseline missing
  current/projected expenditure?) answered with data suggestions: OBR historical official forecasts database,
  GAD Quinquennial Reviews / annual up-rating reports (projected NI contribution rate), PEWP plans, MTFS, LTPFR/FSR.
- GAD / OBR / debt-dynamics tests (27 Sep 2026): gad_obr_tests.R (SAMPLE 1964/1977), data in determinants/gad/ and
  boe_extras. GAD series = projected 10-yr change in the required joint Class 1 NI rate from the Quinquennial Reviews
  (Sam's uploads; file 'gadqqreview1992.pdf' is the JULY 1999 review, Cm 4406; 2000 Bill report not used as a vintage).
  Earnings basis (E) = demographic cost; price/policy basis (POL) projected the required rate to FALL 1981-2009 (price
  indexation made pensions 'affordable' on the actuary's numbers -- the thesis's H&K/indexation point in data).
  Replica: GAD E predicts inflows ALONE (p .005/.024) but is null beside delta_oadr_10 (projection unchanged 0.0075) and
  11-18 AIC worse in its place; POL null; OBR Budget/medium-term borrowing forecasts null beside the projection, wrong
  sign in its place (as realised PSNB); debt interest/TME (t-1) +0.019 p .05 ext, +0.023 p .01 core, projection UP to
  0.0096/0.0109, AIC -2/-3 -> the one non-demographic forward pressure with a signal; report as robustness, primary
  spec unchanged. r-g, gilt yield, interest/GDP null. Nothing written into manuscripts yet -- Sam to decide after R run.
- TABLE CONVENTIONS (Sam, 27 Sep 2026, binding for every manuscript table): one short name per variable, used identically
  as row label, column title and in text; every name shown is defined in the notes under the panel (tables_common.R DEFS).
  mig_rate / L_mig_rate / mig_gross / L2_mig_rate; L_unem (ILO = baseline), L_unem_boe, L_unem_claim; L_vac_wap, L_vac_unem,
  L_dln_vac_unem; L_wage_infl, L_tbill, st_inf_exp; L_tax_nics(_emp/_er); gad_earn/gad_policy/gad_gap; fc_budget/fc_medium;
  L_int_tme, L_r_g. Column titles: '+name' (added), 'name for X' (replaces X), 'name only' (replaces the projection). Never
  'unem' and 'U' in one table; never mix unemployment measures and vacancy controls in one panel (1a vs 1b). No references
  to 'the thesis' in manuscript scripts -- say 'core sample, 1977-2019'. Population-by-vintage and election-cycle tables dropped.
  data_common.R builds df for SAMPLE 1964/1977; run_all_1964.R/_1977.R set SAMPLE globally (bug: Sam's 27 Sep run_all_1964
  ran the shared scripts on the core sample).
- Extended-sample coverage (27 Sep 2026): extension_1964_robustness.R Panel 6 horizons (replica 1965-2019: h=1 0.0018 p .81;
  3 0.0071 p .04; 5 0.0072 p .001; 7 0.0074; 10 0.0081; AIC -74 -> -99 monotone) and Panel 7 British placebo (LTIM 2.00 British
  inflows all origins: 0.0031 p .08 on 1965-2019; 0.0064 p .03 on 1977-2019; total-DR null; as control projection 0.0076).
  Still core-only by data: zero-migration reconstruction (needs components of change, 1975+ vintages), EU placebo (from 1975),
  publication-dated projection (1978+), IPS-by-reason and permits-by-type. Figure 3 (horizons) could now be drawn for 1965-2019.
- Augmented model (replica, core 1977-2016): 'significant' = L_vac_unem, L_wage_infl, st_inf_exp, L_vac_wap; baseline + these:
  AIC -72.5 vs -64.2, RESET .78, BG .16 (no L2 needed), proj 0.0124; gross inflows AIC -75.9 vs -65.5. Does NOT replicate on the
  extended sample (none of the six significant; +six AIC worse). vac_unem + vac_wap + unem together are collinear by
  construction -> in-sample improvement, not a specification change. GAD up-rating reports (annual, next-year outgo only)
  judged not worth collecting.
- Prop 1 / Prop 2 framing (Sam, 27 Sep 2026): build a fiscal/macro model WITHOUT the projection first, then add it.
  prop1_model.R: pre-stated rule (forward selection, no projection, HAC p<.10 + same sign on BOTH samples, lowest summed dAIC,
  max 5 additions). Replica selects L_wage_infl, L_youth, L_tbill, L_tot, L_int_tme -> projection on top 0.0056 (p<.001,
  dAIC -8.5) extended; 0.0022 (p .18, dAIC +0.5) CORE -- absorbed by L_youth + L_tot (drop either -> 0.0092/0.0067). OOS RMSE:
  extended selected+proj best (0.090 vs 0.104); core selected-no-proj marginally best (0.101 vs 0.106). A-priori fiscal/political
  block (L_int_tme, L_labour_gov) leaves projection 0.0083-0.0087 (dAIC -11 to -18). Exchange rate, current account,
  trade balance, 5-10y inflation expectations null. Claimant count (A50, 1945-2016) tested as baseline: indistinguishable from
  ILO; recommended ILO stays (2017-19, 1980s redefinitions) -- Sam's decision. Permits 1959-2008 via 1958-based projection
  (GAD QR1960 Table B) 0.58; permits+Commonwealth vouchers 1964-2008 0.61. ESCoE adds no digital migration series.
- 27 Sep 2026 (Sam): MAIN results run to 2019 using ONS/BoE/BIS/OBR 2017-19 splices (determinants/ons_extension_2017_2019.csv);
  2016 = SECONDARY check printed under each panel for columns using extended series. Supersedes 'do not chase ONS 2017-19'.
  Party dummy DROPPED from the theory model and pool (not Treasury framework); origin-country youth share/relative income not
  Prop-1 candidates. Unemployment 'of the day' = L_unem_head (M5 admin/claimant as published to 1997, ILO from 1998) + L_post98
  step, tested in boe_tests.R 1a. Replica to 2019: headline fits slightly worse than ILO (AIC +1.8/+1.4); with step, step +0.16-0.17
  (p .04/.07), proj 0.0070-0.0072. Fiscal + int_tme: proj 0.0096 ext / 0.0108 core, dAIC -27/-19. Prop-1 selection (rule unchanged)
  now picks L_wage_infl, L_real_house, L_real_eri -> projection on top 0.0059 (p .002, dAIC -7.2) ext; 0.0055 (p .06, dAIC -2.7)
  core; to 2016: 0.0057 / 0.0046 (p .10). Manuscript text/Figure 4 labels still say 'to 2016' -- update after Sam's R run.
- 28 Sep 2026: consolidated handover written to quiet-instruments/HANDOVER.md (supersedes HANDOVER_ReStud_Project.md); keep it current.
- 28 Sep 2026 (cloud session, repo deanvluntz/restud, branch claude/research-paper-synthesis-wqysr9): quiet-instruments pushed
  to GitHub; R 4.3.3 + ARDL 0.2.5 in the container reproduce Sam's run_all_1964 output to the last digit. Sam's decisions:
  definitions only for variables not yet defined (once per document; step sentence first); robustness order = step table, tax
  burden, unemployment measures (ILO/BoE/claimant only), level vs change, permits (no 1959 data, NO basis step), gross, total DR,
  emigration, then regime steps/horizons/British/sub-samples; origin determinants, network (has further inflow lags) and the
  sample-start panel dropped; British panel loses its total-DR column; manuscript body reports CORE checks, extended as
  corroboration with differences named; the ">4 AIC" rule of 27 Sep was the assistant's, not Sam's — his criterion for extending
  the fiscal model is (a) improves the no-projection model on its own AND (b) improves the model beside the projection;
  L_inflation = GDP-deflator inflation; L_emig_all = all-citizenship emigrants per 1,000 WAP (t-1, LTIM 2.00). int_tme:
  ROBUSTNESS ONLY (lead column of the debt panel; augmented model reported in 7.1); primary_int_tme.R written out of curiosity.
  No proposition language in the text. Scripts: robustness_tables.R, no_projection.R, primary_int_tme.R (new); boe_tests.R /
  extra_tests.R / gad_obr_tests.R = further controls F1-F8; extension_1964_robustness.R, permits_long.R removed;
  replication_1977.R fixed (see README change log). Combined manuscript rebuilt (55 pp) with the new Section 7 (7.1, 7.2);
  Table 4 from tables_1977.json; reference 'OBR (2025) Historical Official Forecasts Database' added — Sam to confirm year/title.
  1964-only manuscript text NOT updated. HANDOVER.md §0/§7 updated.
- 28 Sep 2026 (later, Sam): EXTENDED sample first in the combined manuscript, core as corroboration (Section 7 flipped; Table 4 =
  tables_1964.json). OBR refs: 2026a = Historical Public Finances Database, 2026b = Historical Official Forecasts Database (March
  2026). CPI inflation (Millennium A47 col E, ONS D7G7 2017-19) added as L_cpi_infl: selected by the rule instead of the deflator,
  same verdict (null beside the projection, predicts EU flows). Online Appendix renumbered: replication panels = Tables B6-B32
  (robustness B6-B17, no-projection B18-B23, further controls B24-B32); boe_appendix.R retired; build_online_appendix.js intro
  sentence updated (appendix docx NOT rebuilt). Bauer & Salt (2020) Fig. 1 screenshot saved as determinants/bauer_salt_2020_fig1.png
  = the source of work_permits.csv. Git pull on the Mac blocked by a local HANDOVER.md change: stash / pull / pop.
- 28 Sep 2026 (evening, Sam): log forms of the controls tested (L_ln_x), screen repeated for gross inflows, Treasury-view candidates
  from Macpherson (2014) added (openness, M4 growth, participation, revenue ceiling, real TME growth, BoE independence 1998,
  golden rule 1998-2009, OBR era 2010). RESULT TO PUT TO SAM: log vacancies/WAP (and log V/U) is the one candidate meeting BOTH
  halves of his criterion on both samples and on gross inflows (beside the projection +0.15/+0.19, p .01; dAIC -4.4/-3.6;
  projection 0.0074/0.0067; unemployment becomes imprecise). NOT carried into Table 2 -- his decision (27 Sep: unemployment
  retained; a vacancies variant is not post-hoc). Rule selects CPI + boe_indep (= 1998 policy step). Appendix panels B6-B35;
  Online Appendix docx rebuilt from tables_1977.json (manuscript_variants/tables.json had been empty). Splice sources in
  determinants/SOURCES_2017_2019.md; four ONS BoP/national-accounts columns and debt_interest_tme lack series IDs -- Sam to confirm.
  Combined manuscript 57 pp. 1964-only manuscript untouched (Sam: ignore for now). Sam reads everything at the end.
- 28 Sep 2026 (night, Sam's table notes): TABLE RULES — first column "main"; "+name" adds; bare "name" replaces the baseline variable
  the notes name; NO columns replacing the projection except level/horizon/total-DR; candidate screens, selection, sub-samples,
  second lag, sample starts are PRIVATE (private_tests.R), never published; every table defines its variables (screens run last).
  Published set = B6-B14 robustness_tables.R, B15-B17 without_projection.R, B18-B24 controls_tables.R (categorical: labour market
  with log vacancies, growth, trade, inflation/expectations/money, current+projected spending incl. GAD, borrowing/plans/rules incl.
  psnb for ln_debt_gdp, debt dynamics incl. Bank Rate/bill/10y gilt/consols). Treasury-view variables folded into categories (no own
  table); Macpherson cited as Institute for Government (2024a). New variables: tax_earn, level_dr_10, tot_rate, bank_rate, consol,
  growth_pc, growth_fc (HMT start-of-year forecast), lab_prod. DATA FIX: 2017-19 splices now applied on the extended sample too
  (sheet paste had blocked them) — extended BoE-column numbers changed slightly (N 55). Emigration = text line under B11.
  Sam has NOT yet decided on log vacancies in the headline rule; decision pending. Combined manuscript rewritten (7.1 short: B15/B16/B17;
  7.2 by category); Dynamics paragraph and split-sample sentence removed at Sam's request.
- 28 Sep 2026 (late, Sam): basis step questioned (df cost, not policymaker information). Scaled series mig_rate_s built (pre-1975
  non-British x 0.820, the 1975-80 overlap ratio) as an ALTERNATIVE, headline unchanged until Sam decides after seeing the data
  (determinants/dv_series_comparison_1964_1982.csv). New Table B6 on both samples = DV under the other sample's classification;
  B7-B25 renumbered. Core under the non-British classification: 0.0048 (p .03), 1977-2003 alone 0.0059 n.s. — reported honestly.
- 28 Sep 2026 (night, 2; Sam): Home Office cz_03/cz_04 -> determinants/citizenship_grants_1962_2018.csv (grants overseas 1962-82 ONLY);
  data_common.R has cit_grants_k / cit_reg_k / cit_overseas_k and British inflows by region (IPS 3.01). Table B15 = main | +Act steps (ext)
  | brit_rate | +cit_grants | +cit_reg | +brit_rate | +emig_all | +emig_brit, plus a core-only region panel with grants; emigration lives
  ONLY in B15 now. Grants null everywhere (B15, private P10). SAM'S DV STANCE (record, do not revisit): the DV measures the outcome of
  immigration policy overall; "non-EU" is a convenient label chosen before the extended sample; NO model-based adjustment of the series
  (no precedent) — headline stays unadjusted; step vs no step still his call. parallel_gross_vac.R (Sam's request): the full battery
  with main = UNADJUSTED gross inflows (mig_gross, no step, no rescaling) + L_ln_vac_wap, unemployment retained; both samples; last in
  run_all_1964.R; tables numbered G2-G27 to mirror B2-B25. Not in any manuscript. Sam has NOT yet decided on log vacancies in the headline.
- 28 Sep 2026 (night, 3; Sam): ONE DATASET — Sam: do NOT delete the pasted BoE columns; all data go INTO the workbook. export_dataset.R
  writes sheets data_1964 / data_1977 (assembled canonical variables, pre-lag) into Dissertation_Dataset_Final.xlsx via openxlsx
  (Sam's sheets and 113 formulas verified unchanged); data_common.R reads them when present, CSVs only under REBUILD_DATA <- TRUE.
  RE-RUN export_dataset.R after any determinants/*.csv or assembly change, then commit the workbook. Naturalisations cit_nat_k
  (cz_03 residence + marriage) added: null for British inflows on both samples and in every region; B15/G15 carry grants / nat / reg.
- 28 Sep 2026 (night, 3; Sam): (1) DO NOT update the manuscript text/tables (or rebuild documents) on every code run — Sam is testing;
  outputs change only when he decides. (2) ONE DATASET: keep the pasted BoE columns; export_dataset.R writes sheets data_1964 /
  data_1977 (assembled canonical variables, pre-lag) into Dissertation_Dataset_Final.xlsx via openxlsx (Sam's sheets + 113 formulas
  verified unchanged); data_common.R reads them when present (CSVs only under REBUILD_DATA <- TRUE). RE-RUN export_dataset.R after any
  determinants/*.csv or assembly change and commit the workbook. Code must run locally in RStudio (wd = diss_replication/).
  (3) Grants / naturalisations / registrations of citizenship enter for the CURRENT year (cit_grants_k, cit_nat_k, cit_reg_k), never
  lagged; series end 2018. All null for British inflows on both samples and in every region. (4) Three parallel specifications:
  parallel_spec.R engine (GROSS, VAC switches) + parallel_gross_vac.R, parallel_gross.R (gross, no vacancy term), parallel_rate_vac.R
  (published DV + log vacancies); Tables G2-G27; in run_all_1964.R. Nothing from them is in any manuscript.
- 28 Sep 2026 (night, 4; Sam): British inflow — grants and naturalisations tested in BOTH timings (current + lagged) in B15/G15; P10 (private)
  sweeps sample starts/ends and adds emig_brit_5, law steps (bna1981/hk1990/nia2002), relinc_rich, real_eri, eu_rate. Log changes of the
  tax burden (L_dln_tax_workpop, L_dln_tax_res) added to B8/G8 — null, level stays. Workbook data sheets re-exported (run export_dataset.R
  after any assembly change). No manuscript/run_all outputs regenerated (Sam's rule).
- 28 Sep 2026 (night, 5; Sam): DV provenance CONFIRMED: ReformSPA nEU_inflows = IPS 3.01 NON-EU residence x non-EU citizens exactly (the
  earlier "= IPS 3.08" note is wrong; 3.08 is the by-reason table); no BoE data in any DV. IPS 3.18 (Sam's upload) ->
  determinants/ips_318_birth_by_residence_1975_2019.csv; data_common.R: ukborn_* (UK-born inflows by region) and brit_desc_noneu
  (British from non-EU minus UK-born from non-EU). P10: projection moves brit_desc_noneu (0.0144 p .02), not the UK-born (p .11) ->
  the British response is NOT return migration. tax_voter (per adult = WAP + pensioners via oadr_0) and tax_taxpayer (HMRC counts,
  1979-) in B8/G8: same result as per WAP. Sam's framing: the burden is per INDIVIDUAL (closer to per voter), not a specific share.
- 28 Sep 2026 (night, 6; Sam): Sam's view on the specification: GROSS inflows are the measurable policy outcome ("the model predicts their
  size relative to fiscal and demographic projections rather than the current population"); the TAX denominator is the defensible margin,
  not the DV denominator; what matters is that results survive across BOTH samples, not any single variant. parallel_spec.R has TAXVAR
  (L_tax_workpop / L_tax_voter / L_tax_res); parallel_gross_adult.R = gross + tax per adult (0.0079 / 0.0078). Cmnd 9863 (HO 1985) has no
  citizenship tables; settlement acceptances 1975-85 transcribed to determinants/ho_settlement_1975_1985.csv. BoE A19-A20 = LTIM/IPS
  series already used (nothing new for 1964+).
- 28 Sep 2026 (night, 8; Sam): Cmnd 8325 (Tables of Persons acquiring Citizenship of the UK and Colonies 1980) = the annual series behind
  cz_03/cz_04 (totals match); Table E -> determinants/ho_citizenship_1971_1980_cmnd8325.csv. East African UKPHs were CUKCs already ->
  NOT in citizenship grants; they are the special voucher holders of the immigration-control series. DIAGNOSTICS FINDINGS (report, do
  not hide): RESET as used = powers 2:3; squares-only RESET does not reject the no-projection model; BG(2) rejects for the published
  spec on the extended sample (p .029) and is marginal on the core (.062); L2 of the DV does not cure it. model_search.R (exhaustive,
  both DVs, both samples; eligibility RESET+BG+proj sig+VIF<20 on both) -> best gross: unem + tax(res/voter) + ln_debt_gdp + ln_vac_wap;
  best rate: tax_workpop + ln_debt_gdp + int_tme + ln_vac_wap. Shortlist given to Sam; nothing in any manuscript.
- 28 Sep 2026 (night, 10; Sam): eleven Command Papers on repo MAIN (HO1978-HO1988.pdf; *.pdf gitignored on the working branch). 1978-83 =
  citizenship series (by country x section; Table E/C by region -> determinants/ho_citizenship_by_region_1971_1983.csv); 1984-88 = Control
  of Immigration Statistics (settlement, vouchers, ancestry route -> determinants/ho_settlement_1975_1988.csv, replaces the 1975-85 file).
  data_common.R: settle_k, vouchers_k, ancestry_k, cit_newcw_k, cit_afrcw_k, cit_oldcw_k. P10 triangulation 1975-88: the 1980-82 British
  episode is in UK-born AND non-UK-born arrivals alike; vouchers ~1.3k/yr falling; registration wave 1981-83 = residents; nothing
  administrative explains it. British inflow verdict: general post-recession rise, not a nationality-law artefact.
- 29 Sep 2026 (Sam): NO bootstrap / exact bounds on routine runs (BOOT, EXACT flags in compare_gross_adult.R; final diagnostics only). Sam's
  favoured configuration: gross inflows + tax per capita (adult or resident) + log V/U (in place of unemployment; "or change in log V/U") +
  ln_debt_gdp + int_tme. Result: log V/U replacing unemployment beats it on every criterion on both samples; + int_tme = best in-sample
  (AIC -102.8 / -78.5; proj 0.0082 / 0.0084). Δln V/U weak. p_pub_ext (publication-dated projection to 1965) in data_common.R; G26 both
  samples. Nothing in any manuscript.

- 29 Sep 2026 (4; Sam): compare_gross_final.R: tax per WAP / per adult / per resident indistinguishable in the conceptual model (proj 0.0083-0.0086
  both samples; AIC within 0.5); tax_voter carried forward; per taxpayer (N 40) and per earnings weaker. Full-data sweep under Sam's criterion:
  only wage_infl (and brit_rate, a flow) pass both halves on both samples. External position (BoP, trade balance, nominal/real ERI) null on
  both samples; terms of trade significant on the extended sample only. No addition lets the NO-projection model pass diagnostics on both
  samples (rainbow fails .03/.003 without the projection); projection on top always p<.001. Nothing in any manuscript.
- 29 Sep 2026 (5; Sam): compare_british_wage.R. British equation: projection removed on BOTH samples only by PSNB/GDP and the golden-rule
  step (both predict British inflows p<.01) — British flow tracks the current deficit, not the projection; wage inflation does not remove
  it on the core. Wage inflation is not price inflation (survives beside the deflator; deflator/CPI/real wage null on the core); tax burden
  already real. Stability rule (ext pass + core same sign inside ext CI): ten pass, only wage_infl also significant on core. Sam questions
  the strict both-samples rule; no decision yet. Nothing in any manuscript.
- 29 Sep 2026 (6; Sam): Sam's CORE SPEC now = gross inflows (no step) + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem + L_ln_int_tme +
  delta_oadr_10 (7 parameters). compare_wage_boot.R: wage inflation survives a recursive bootstrap on the extended sample (p .01-.04) but
  not the core (p .18-.26), and its AIC gain (-3.9) equals the median best-of-37 under H0 (reality check p .50) -> robustness only, not
  headline. Ratios do NOT free degrees of freedom (one regressor = one parameter). British inflows vs PSNB/golden rule: cyclical proxy and
  2000s period dummy, not a policy mechanism — the point is asymmetry (non-EU coefficient survives both; British does not). Nothing in any manuscript.
- 29 Sep 2026 (7; Sam): splice series IDs confirmed by Sam's ONS uploads: ca_gdp = AA6H (exact), labour_share = IHXP (growth), trade_bal_gdp
  = FSID / OBR nominal GDP (change), terms_of_trade = JIO7/JIO6 (growth); YBGJ (ToT effect, £m) NOT a fit. Three columns recomputed on
  June/July 2026 vintages, workbook re-exported. debt_interest_tme still to confirm. WAGE INFLATION (Sam's decision): robustness line under
  the labour-market controls; text frames it as the incomes-policy instrument of its era (like the BoP), explanatory but not part of the
  timeless core framework. Not yet written into any manuscript (headline spec still undecided).
- 29 Sep 2026 (8; Sam): ca_gdp = ONS AA6H and labour_share = ONS IHXP for ALL years (Millennium kept as *_mill); int_tme 2017-19 re-spliced
  from Sam's BoE construction NMFX/(ANLP-ANNS+NSRN) (ONS PUSF); full PUSF ratio = int_tme_ons. FLAG: Millennium int_tme and the PUSF ratio
  correlate .75 with 2-3 point gaps in 1965-81 and 2008-13; under the PUSF ratio the debt-interest term is NULL in the core spec on both
  samples (Millennium: significant). Sam to decide the series. parallel_spec.R: LABV + INT switches; parallel_core.R = Sam's core spec
  (gross, tax_workpop, ln_debt_gdp, ln_vac_unem, ln_int_tme, projection) through G2-G27; in run_all_1964.R. Manuscript untouched until Sam finalises.
- 29 Sep 2026 (9; Sam): debt-interest ratio = ANLO / EBFT (PUSF), the Millennium definition — NOT the NMFX construction. Current vintage =
  Millennium to 1996, +1-2 points from 1997 (ONS revisions). Core spec with the reconstruction: 0.146 (p .03) ext / 0.156 (p .007) core,
  best AIC -> the debt-interest term STANDS. int_tme_ons = ANLO/EBFT all years; int_tme 2017-19 growth-spliced from it. Recommend adopting
  the full PUSF series (Sam's "use ONS entirely") — his call.
- 29 Sep 2026 (10; Sam): DECISIONS: int_tme stays MILLENNIUM ("most contemporary figures known at the time"; PUSF reconstruction = int_tme_ons,
  comparison only). Unemployment splice justified (measure change divides the period; results unchanged) -> state explicitly in the manuscript
  when updated. Splice audit done from Sam's downloads: all Millennium 2010-16 values = current sources to rounding; 2017-19 labour-market
  values re-read (tiny changes). OECD destination unemployment (AUS/CAN/NZL/USA; Spain from 1999 unused) -> unem_dest, unem_diff in data,
  B15/G15/P10: null for British inflows on both samples; projection survives. 3-month bill rate discontinued 2017 (Bank-Rate construction stands).
  "IPS by-reason" in my list = table 3.08 main-reason composition test (25 Sep), NOT country of last residence.
- 29 Sep 2026 (11; Sam): IPS 3.08 parsed in full (long CSV); by-reason inflows for non-EU and British in data_common (British home/join coded
  only from 1980/1982 -> brit_nonwork_k consistent). OBR WP1 Chart 3.6 output gap DIGITISED 1972-2010 (gap_pca, gap_agg; +/-0.2). composition_reason.R
  (core sample; in run_all_1964.R): projection acts on the non-EU WORK route (study null); NO British route responds (other negative);
  lagged output gap raises British inflows (+0.054 p .02 core controls; +0.13 for non-UK-born British) but does not remove the projection;
  gap null in the main equation beside log V/U. Precise gap reconstruction later if Sam wants (OBR EFO databank runs from 1972).
- 29 Sep 2026 (12; Sam): ORDERING PRINCIPLE for robustness and manuscript: broadest measure first (1964 inflows by citizenship), narrower/more
  precise sources as robustness when available (work permits, IPS by reason = SEPARATE panel, citizenship vs country of last residence = the
  literature standard, IPS vs LTIM). OBR gap: not worth chasing unless suggestive (it was, for British inflows; his call). Robustness must show
  wage inflation AND the balance of payments together. replication_core_spec.R = the full replication file in that order for Sam to inspect
  and refine before it replaces the manuscript scripts; data_common has res_noneu_k / cit_noneu_k / dv_noneu_k (IPS 3.01 margins).
- 29 Sep 2026 (13; Sam): replication_core_spec.R FIXED for Sam's Mac (his error = local workbook without data_1964/data_1977 sheets: the file
  now prints wd/workbook/sheets and stops with instructions; data_common.R likewise). Sam's main-branch uploads: OBR PSF databank output gap
  (gap_obr, FY 1972-73 on, = cyclical control now; British +0.050 p .03 core, projection intact; null in main), UN DESA UK stocks, HO policy
  timeline (1983-2025), five HO Control of Immigration volumes 1970-2009 (indexed; settlement-by-nationality extraction NEXT, use the 1993/1999
  retrospective Tables 6.5). Trade indicators beside wage/CPI inflation: none on the core; tot marginal on the extended. tot_rate is a FLOW
  (total inflows), never a trade indicator.
- 29 Sep 2026 (14; Sam): HO volumes parsed: settlement by nationality 1983-2007 (retrospective tables) + totals 1960-2006 (Table 5.8) ->
  determinants/ho_settlement_*; data_common settle_total_k, settle_push_k (HK+ZW+ZA+KE+UG) etc. Replication file: Table 2.3b settlement as
  outcome (core responds 0.0122 p .005; extended 1965-2006 null), Table 4.3 push events: NULL for British inflows (r -0.01 with non-UK-born
  British) -> no push-event story. 1970-82 by nationality not parsed (annual volumes); 2008-09 volumes absent.
- 29 Sep 2026 (15; Sam): Sam's second error = a stray REBUILD_DATA <- TRUE in his global environment (left by export_dataset.R) made
  data_common.R rebuild from CSVs. FIX: rebuild is now options(restud.rebuild = TRUE) set only inside export_dataset.R (reset on exit);
  data_common.R ignores REBUILD_DATA; replication_core_spec.R sets the option FALSE and removes a stray REBUILD_DATA. Sam must source()
  the file from diss_replication/, NOT RStudio "Compile Report" (which runs from the file's folder, e.g. Downloads).
- 29 Sep 2026 (16; Sam): LAYOUT RULES for replication_core_spec.R: samples = "Citizen sample (1964-2019)" / "Origin sample (1977-2019)";
  ALL alternative outcomes in log persons (never per WAP); column titles = the variable added/substituted only (no "X for Y"); no gap_pca
  anywhere (gap_obr only); no "to 2016" lines (options(restud.secondary = FALSE)); Tables 1/1.1 = each sample in full with bootstraps;
  Table 1.3 citizenship vs origin on 1977-2019 + correlations; Section 2 = mechanism (with/without projection, permits, by reason);
  3 projection; 4 regressors (4.3 fiscal term vs tbill/term spread/gilt/TME/none); 5 alternative outcomes; 6 uncontrolled flows.
  Findings: tbill beats int_tme on the Citizen sample only; int_tme (level/log) the only fiscal term holding on both. tme_predict.R: TME
  growth predicted by lagged log debt/GDP + unemployment (adj R2 .34-.46), tax per WAP adds nothing. export_dataset.R on.exit bug fixed.
- 29 Sep 2026 (17; Sam): replication_core_spec.R v3 = his corrections: workbook column check with pull message; permits outcome in LEVELS
  (thousands, no log); Section 3 split (3.1 total DR vs OADR, 3.2 level vs change, 3.3 horizon); Section 4 NON-ECONOMIC robustness before
  the economic controls (4.1 p_pub_ext / p_pub / delta_zm_10; 4.2 second lag, lead, lag, timing placebo; 4.3 regime steps; 4.4 recursive,
  rolling, CUSUM, sup-F); 5 economic regressors, 6 alternative outcomes, 7 uncontrolled flows. tme_predict.R step 3: block models plus
  other dataset variables (level/log, ratios, interactions), best three by AIC with each added term HAC p<.10 and VIF<20; tax per WAP
  never predicts TME. Sam's tme error = stale workbook (missing tme_obr_*); his 'replication_core_specR' error = a typo. Nothing in any manuscript.
- 30 Sep 2026 (18; Sam): dv_predict.R = the tme_predict engine on the DV (mig_gross): FIXED L_mig_gross, delta_oadr_10, L_tax_workpop;
  block 1 debt/GDP + unemployment any form, block 2 unemployment any form; + pool additions (level/log, ratios, interactions). VIF screen
  now covers block + added terms only (fixed terms exempt; lagged DV and tax level VIF 10-22 by construction) in both scripts. Results:
  projection 0.0057-0.0105 (p<.001) in every listed model; recurring additions = unem x int_tme (both), unem x wage/CPI inflation
  (Citizen), tbill and unem x labour_gov (Origin), trade balance, d_debt x gap. Nothing in any manuscript.
- 30 Sep 2026 (19; Sam): fiscal_term.R = the debt-interest term examined: not collinear with L_ln_debt_gdp (r -0.19/-0.08, VIF 2.2); int/GDP
  component carries it, TME/GDP null alone and negative beside it; ratio restriction not rejected; NO replacement (int_gdp, debt level, PSNB,
  TME, grants, tax/GDP, bill/gilt/Bank Rate/spread/real rate/r-g, rdebt, rg_debt, stock x price pairs) beats it on both samples; dropping it
  costs only 1.5-1.7 AIC (HAC p .03/.01). WARNING: data_common.R sets a global BASE (published rate spec) -- never name a script's own
  regressor list BASE. Nothing in any manuscript.
- 30 Sep 2026 (20; Sam): Sam regards int_tme as agent-constructed without theory (overfitting risk); wants a GROUNDED interest/expenditure
  term significant on both samples. interest_terms.R: int/TME, int/revenue (IMF fiscal-space ratio), int/GDP, real int per WAP (parallel
  to tax_workpop), primary deficit, programme spending, effective rate; levels/logs/changes. ONLY int/TME passes 5% on both; int per WAP
  .08/.006; int/revenue and int/GDP Origin-only. Grounding offered: debt interest is an expenditure line in the Treasury's own totals; its
  share of TME = the part of the envelope not available for programmes (OBR reports it as share of TME and of revenue). Sam to decide.
- 30 Sep 2026 (21; Sam): Sam finds the LEVEL of int_tme (% of TME) more justifiable as a constraint than the log. level_and_spread.R:
  level vs log indistinguishable (AIC -102.9/-79.2 vs -102.7/-78.4; share p .04/.005; projection 0.0085/0.0088 either way). Yield spread
  (gilt - bill, to 2016) null for British inflows on both samples; gilt/bill also null there; in the non-EU equation the gilt yield and
  bill rate are negative on Citizen only. Unlagged ln_vac_unem / ln_int_tme are NOT in df (the replication file builds them) -- create
  them before calling bounds(). Nothing in any manuscript.
- 30 Sep 2026 (22; Sam): levels_grid.R: with int_tme in level, debt term as pp change or LEVEL of debt/GDP and V/U in level tested.
  Log forms best on both samples; V/U level costs ~6 AIC; pp change of debt/GDP close on Origin but weakens int_tme (p .15); the LEVEL of
  debt/GDP is null, costs 12-18 AIC, kills int_tme (p .3-.5) and lowers the projection. Debt term stays a change; V/U stays in logs.
- 30 Sep 2026 (23; Sam): unem_variant.R: ln_debt_gdp + int_tme LEVEL + ILO unemployment (for log V/U): holds on both samples (unem
  -0.027/-0.026 p<.01; share p .02/.015; projection 0.0099/0.0108) but 3.5/4.9 AIC worse than log V/U; with both labour terms in,
  unemployment is null and log V/U survives on both samples. Nothing in any manuscript.
- 30 Sep 2026 (24; Sam): int_wap_variant.R: real debt interest per WAP in place of int_tme (ln_debt_gdp, log V/U kept): Origin
  0.073 p .006 (AIC -78.9 vs -79.2); Citizen 0.056 p .08 (AIC -101.6 vs -102.9, only 0.6 better than no fiscal term); projection
  0.0089/0.0093; null without the projection. Passes the both-samples rule only at 10%. Nothing in any manuscript.
- 30 Sep 2026 (25; Sam): combined_fiscal.R: common base for tax and interest: / GDP and / TME bases FAIL (projection collapses on Citizen,
  RESET rejects); per-WAP base works. ONE-TERM burden taxint_wap = (taxes + debt interest) per WAP replaces tax_workpop + int_tme with one
  parameter fewer and better AIC on BOTH samples (-103.5/-80.8 vs -102.7/-78.4; p<.001; projection 0.0086/0.0090); equal-coefficient
  restriction on tax and interest per worker not rejected. Sam's dln(tax+debt+int)/GDP: r .96 with ln_debt_gdp, worse fit, kills the
  projection on Citizen when it replaces all three. Nothing in any manuscript.
- 30 Sep 2026 (26; Sam): CORRECTION: without the projection taxint_wap is significant on Citizen (p<.001), marginal on Origin (p .06).
  noproj_additions.R (screen under Sam's two-part criterion with the spare parameter): Citizen passes = tot, wage_infl, cpi/deflator
  inflation, tbill; Origin NONE (psnb_gdp fixes the no-proj burden but hurts the with-proj model; wage_infl improves no-proj fit but
  burden stays n.s.). No addition passes on both samples. Nothing in any manuscript.
- 30 Sep 2026 (27; Sam): spend_infl_screen.R: spare-parameter screen WITHOUT the AIC condition + expenditure/inflation constructions
  (tme_wap, prog_wap, tme_tax, tme_taxint, g_tme_real, infl_debt, adj_psnb, real_int, real_int_tme, tme_earn). AIC was never binding:
  on Origin 19 candidates rescue the no-projection model, 0 stay significant beside the projection. Citizen: TME/taxes (negative) and
  pairs with wage inflation / ToT pass. Nothing passes on both samples. Nothing in any manuscript.
- 30 Sep 2026 (28; Sam): inflation_check.R: inflation IS in ln_debt_gdp (nominal GDP denominator); split, inflation -0.98/-1.08 vs
  nominal debt +0.65/+0.76, 'only through the ratio' not rejected (p .44/.59). Screen with int_tme LEVEL: only d(inflation x debt/GDP)
  passes on both (-0.021/-0.024, p .006; proj 0.0084); int_tme never significant without the projection. Selected from 24 measures x 2
  samples -- a screening result, not a prior. Fails with the one-term burden on Origin. Nothing in any manuscript.
- 30 Sep 2026 (29; Sam): uploads Hatton (2005, JPopE) + Mitchell, Pain & Riley (2011, EJ) to the branch. Standard in both: levels at t-1,
  current-year changes (Hatton: d unemployment; MPR: ARDL levels t-1 + changes t, t-1, lagged DV). timing_bma.R: Sam's SHIFT (projection
  also t-1) fits best on both samples, long run unchanged; current-year change in the projection null -> inflows respond to the
  projection a year earlier (also safer on publication timing). BACE BMA coded (CRAN blocked): lagged projection dominates; Citizen
  core all robust; Origin mode (log V/U run) = core with lagged projection. Timing switch NOT adopted -- Sam's decision; nothing in any manuscript.
- 30 Sep 2026 (30; Sam): diag_shift.R: full battery for timing A / B (shift) / publication-dated at t / t-1. B's rainbow rejection (.045/.015)
  is not functional form (by fitted .39/.83) but post-2008 instability: sup-F .039/.005 (A .52/.11); under B the projection effect vanishes
  from 2009 (net -0.002/-0.003), under A it does not; B forecasts worse (OOS RMSE .119/.122 vs .106/.114). Publication-dated projection at t
  ~ base-year at t-1 (r .95/.92), AIC between A and B, also rejects sup-F. Recommended: keep A. Sam's decision; nothing in any manuscript.
- 30 Sep 2026 (31; Sam): Glover et al. (2001, RDS Occasional Paper 67) + Home Office (2002, Cm 5387, Secure Borders, Safe Haven) uploaded;
  verified passages and page offsets in Glover_2001_HomeOffice_2002_research_brief.md. FLAGGED to Sam: Aim 5 PSA = race equality in public
  services (not entry); IND PSA targets were asylum process targets only (p.17); fn 13 regression = NET non-UK non-EU migration on ILO
  unemployment + lagged net migration (timing unstated); the WP reports Work Permits (UK) as "now part of" IND (transfer 2001, date to
  confirm); entry clearance = HO/FCO Joint Entry Clearance Unit, authorities "generally accept" permits (Sam's indirect-control point, p.19-20).
  regime_timing.R: no break at 2001-04; under t-1 timing the projection effect ends from 2008-10 (PBS / crisis / cap indistinguishable);
  to 2008 t-1 beats t by 19/13 AIC; regime model (t-1 to 2007) best AIC, clean diagnostics; permits also respond at t-1. Nothing adopted.
- 30 Sep 2026 (32; Sam): Sam AGREES on the p.51 PSA reading; DISPUTES the p.13/14 caveats (UN figures sit in the discussion of UN Replacement
  Migration 2000; do not use them to downplay GAD) -- accepted, brief amended; the £2.6bn is ANNUAL (1998/99), i.e. the short-term benefit Sam
  stresses. Transfer dating: MoG announcement 8 Jun 2001; SI 2001/3500 (Transfer of Functions (Miscellaneous) Order 2001, in force 26 Nov 2001)
  per search summaries (legislation.gov.uk blocked here; Sam to check the article); Gemini's Eagle/Hughes written answers UNVERIFIED.
  break_candidates.R: no economic indicator, interaction or pair (780) removes the post-2008 break under t-1 timing on both samples; rates
  help most on Citizen (post-09 change still p .02); unem x proj passes on Origin only (costs BG). Break = regime, not a missing macro variable.
- 30 Sep 2026 (33; Sam): Sam uploaded SI 2001/3500 -- it does NOT cover work permits (moves functions OUT of the Home Office: Lord Chancellor,
  DCMS, DTI, DTLR, DEFRA); earlier search-summary claim was WRONG, brief corrected. Work-permit move = 8 Jun 2001 MoG; legal instrument not
  found (possibly none needed). Sam: regime indicators are inconsistent with the model's theory -- do not propose them. Sam may edit the
  briefs himself: 'Sam's notes' section in the Glover brief + new Heclo_Wildavsky_1981_notes.md (repo root) -- ALWAYS pull before editing
  these and never change Sam's entries without asking. relax_screen.R (restriction ladder R1-R4): under t-1 timing the break can be made
  insignificant only by models that fit worse on Origin, mostly via Bank Rate (flat 2009-19, acts like a regime dummy). WARNING: data_common.R
  defines global LAB as well as BASE -- never reuse either name.
- 30 Sep 2026 (34; Sam): Sam keeps ALL his notes in the 'Sam's notes (any source)' section of Glover_2001_HomeOffice_2002_research_brief.md
  (Heclo_Wildavsky_1981_notes.md deleted). Release order for relaxing the core (Sam): interest share first, then debt/GDP, then the labour term;
  tax stays. relax_ladder.R: from earlier mains, the stable improvement is int_tme + an inflation term (S2 + int_tme + wage inflation: -5.3/-0.7 vs
  current core, clean; deflator version similar); grants/GDP models fail rainbow; under t-1 timing releasing interest and debt never removes the break.
  KEY DATA FACT: from 1977 the Citizen and Origin outcomes are IDENTICAL (dv_noneu_k, corr 1.000) -- the 'Origin sample' is the 1977-2019
  SUBPERIOD of the same series, not a residence-based DV; the DV is ~98% of all non-EU citizens (cit_noneu_k); the literature's residence-based
  series is res_noneu_k. Sam is considering one main reference (Citizen 1965-2019) with 1977-2019 as robustness -- his decision.
- 30 Sep 2026 (35; Sam): PROTOCOL for one main reference (Sam agreed): main = Citizen 1965-2019; same spec on 1977-2019 (no re-selection);
  Chow at 1977 (classical F + HC3; NOT HAC -- over-rejects with 2k params at N 55); residence-based (res_noneu_k) and any-residence citizenship
  (cit_noneu_k) outcomes. screen_engine.R holds run_protocol(); main_spec_protocol.R runs it. Current core passes Chow (F 1.01 p .44).
  reverse_build.R: ordered build (tax, labour, debt, interest) under Sam's rules recovers the current core (tax per adult ~ per WAP); an 8th
  parameter fails the protocol. break_citizen.R: under t-1 timing the break can be removed on Citizen only with bill-rate models (BG(2) fails;
  sup-F on 1977-2019 rejects) -- no t-1 model beats the core at t out of sample. Nothing in any manuscript.
- 30 Sep 2026 (36; Sam): serial_cycle.R. BG(2) failure = modest NEGATIVE residual autocorrelation (-0.16/-0.23), not the debt term (Sam's hypothesis
  tested: re-timing/dropping debt leaves BG(2) failing); consistent with IPS measurement error; ML-AR(2) and IV move the projection to .0072-.0084
  (long run .016-.021). CANDIDATE: tax at t-2 instead of t-1 (same df): AIC -6.5/-5.1, OOS .092 vs .106, BG(2) .046/.035 -- found in a residual
  search, Sam to judge the grounding (latest complete fiscal-year outturn at decision time). No cycle measure removes the post-2008 break or the
  British projection response. Sam's timing wording: the break around the end of work permits and the recession is "potentially interesting",
  but demographic projections are largely predictable ahead of publication (base-year t-1 vs publication-dated t r .95/.92). Nothing in any manuscript.
- 30 Sep 2026 (37; Sam): Sam REJECTS tax at t-2 on grounds. tax_lag_proxy.R: the change in tax/GDP (t-2 -> t-1, L_d_tax_gdp) captures what t-2 did,
  with tax per WAP at t-1 kept (both periods significant; dAIC -7.4/-5.0; OOS .090; Chow 1977 passes). Screened (1 of 30) after the t-2 finding --
  Sam decides. 1964 row: all regressors exist, but L_mig_gross needs 1963 inflows (IPS starts 1964) -> estimation starts 1965 ("1964-2019" = data span).
  Sam's point for the timing paragraph: the demographic unknowns are fertility and migration; fertility cannot move a 10-year OADR (anyone of
  working age in 10 years is alive now) -- caveat raised: MORTALITY at older ages is a third unknown that does move the OADR numerator.
- 30 Sep 2026 (38; Sam): TITLE/LABELS: estimation starts 1965 (1964 is only the lag) -> Sam: the title must say 1965-2019. Script labels now
  "Citizen sample (1965-2019)"; manuscript titles ("(1964–2019)" in the 1964 and combined documents) to change when Sam finalises.
  Sam REJECTS tax/GDP change as the t-2 replacement (both components already in the model; mixes policy and economic change); his reading:
  governments avoid frequent tax changes -- needs a POLICY series (not in the data; suggested Cloyne 2013 UK narrative tax changes, OBR policy
  measures database, IFS historical rates -- check coverage before promising). ltim_serial.R: with LTIM from 1991 the serial correlation goes
  (BG(2) .20/.196 spliced; .45 on 1992-2019 alone); projection .0096/.0109. Outcome choice (IPS vs spliced LTIM) = Sam's decision.
- 30 Sep 2026 (39; Sam): LTIM = ROBUSTNESS CHECK ONLY (IPS stays the outcome). '1964' in titles = clerical error -> 1965-2019 when edited. Sam's feedback: recalibrate
  'practical/plausible' judgements against the thesis/manuscript aims; raise the obvious data comparison (e.g. IPS vs LTIM) immediately. IFS statutory rates
  (determinants/ifs/ -> ifs_tax_rates_1973_2023.csv; NIC % rate from 1975 -> rate tests 1977-2019 only). tax_rates.R: salient level (payslip, avg-earner
  rate, real payment, top rate, wedge), rate changes, fiscal drag (allowance/earnings), projection x burden: NONE passes Sam's criterion; each fits 14-20 AIC
  worse in place of tax per WAP; headline rates fell while real tax per WAP doubled (r -0.84/-0.87) -> P2 works through the burden borne, not announced rates.
  No construct fixes BG(2). Rates CSV not in the workbook (only if adopted). Nothing in any manuscript.
- 30 Sep 2026 (40; Sam): interest_apf.R: Millennium int_tme = public-sector interest INCL. the public-sector banks (ANLO); it jumps 5.7 -> 9.6-10.9% of TME
  in 2008-11 (bank reclassification), while CG interest (NMFX) and CG net of APF (NMFX+MU74, OBR definition; APF from 2009, not 2007) do not. Excluding
  banks or using CG/net-of-APF, the interest term is NULL on 1965-2019 -> its significance rests on 2008-13. Structural deficit (OBR CA series, 1975+) null
  beside the projection, NEGATIVE without it; QE stock/M4 null; Bank Rate Citizen-only. burden_motion.R: 'tax + REAL debt interest' per WAP (one term
  replacing tax + int_tme) meets both halves of Sam's criterion on both periods (Millennium interest); with CG-net-of-APF interest only the no-projection
  half. Headline rates: basic rate only ever cut, corporation tax cut until 2023; rises = NICs/VAT; with employer NIC + top rate, rises are packaged in one
  year and followed by fewer changes than chance for 1-5 years. pop_rate.R: inflows as % of total population -> projection holds (0.0022/0.0026 pp,
  same proportional effect), EU placebo null. Accruals (Sam's ONS note): fiscal block year t = FY (t-1)/t, so tax at t-2 = FY (t-3)/(t-2), the latest
  fiscal year whose accrued receipts were complete (self-assessment balancing payments due 31 Jan t-1) during t-1 -- a real-time-data reading of the t-2
  term, for Sam to judge. Nothing in any manuscript.
- 30 Sep 2026 (41; Sam): DECISION: int_tme is OUT of the results (significant only via the 2008-13 bank reclassification). Core is now
  mig_gross ~ L_mig_gross + delta_oadr_10 + L_tax_workpop + L_ln_debt_gdp + L_ln_vac_unem (6 parameters; CORE_A in screen_engine.R still holds the
  old 7-parameter list -- use setdiff(CORE_A, 'L_int_tme')). core_no_int.R: protocol passes; projection 0.0072 / 0.0067 (p<.001), all terms significant
  on both periods, BG(2) .043 / .019, Chow 1977 F 1.07 p .40. Millennium-interest one-term burdens (taxint, taxrint) dropped with it. NA taxes per WAP are
  already real (HMT GDP deflator, 2024-25 prices). TO MENTION IN TEXT (Sam): headline tax rises were sparse outside the sterling crisis and the first
  Thatcher government (almost yearly 1976-83; then 1991, 1994, 2003, 2010-11, 2022-23; basic rate never raised). Nothing in any manuscript yet.
- 30 Sep 2026 (42; Sam): fiscal_ratio.R: NO clean (ex-banks) borrowing/expenditure ratio can replace int_tme (9 tried; clean interest shares null on
  1965-2019, every borrowing share has the WRONG sign, r .7-.8 with the debt-change term). Sam's fallback, the one-term burden, passes on clean data:
  taxrint_xb = taxes + public-sector debt interest EXCL. banks - inflation x net debt at the start of the year, real GBP k per WAP, dated as tax (FY ending t):
  0.0370 / 0.0460 (p<.001), projection 0.0060 / 0.0051 (p<.001 / .002), dAIC -2.1 / -2.0 vs tax only, -5.5 / -5.9 without the projection; protocol passes
  (Chow 1977 p .42; BG(2) .039 / .060; residence-based outcome 0.0050). Projection lower than tax-only because the 1970s inflation-erosion component correlates
  with it (+.24 / +.29). Nominal version fails. PENDING SAM'S CONFIRMATION of this core before the series go into data_common.R / the workbook.
  density_forecast.R (MPR 2011 s.4.4): models beat AR(1) out of sample; projection log-score gain +0.09 / +0.16, not significant (30 forecasts); timing A
  calibrated (Berkowitz p .12-.30), timing B not (p < .01). Nothing in any manuscript.
- 30 Sep 2026 (43; Sam): british_mpr.R (MPR 2011 Table 6 UK-relative determinants on British inflows): relinc_dev (UK vs IND/PAK/BGD/CHN/NGA/PHL/ZAF GDP
  per head) REMOVES the projection from non-UK-born British arrivals from non-EU (0.0125 -> 0.0035, p .57; relinc_dev +1.12 p .026; dAIC -4.7, 1977-2019)
  and weakens it for all British (LTIM); relinc_rich, youth, unem_diff, return pool (British emigration 5/10 yrs) null. In the MAIN non-EU equation relinc_dev
  is null and the projection unchanged (0.0077 / 0.0062) -> asymmetry: uncontrolled British flow = relative-income pull; controlled flow = projection.
  Caveats: relinc_dev r .33-.36 with the projection, trending after 2001; 1 of 6 tried; IPS sampling error. Nothing in any manuscript.
- 30 Sep 2026 (44; Sam): british_mpr.R Part 2: WDI Gini + unemployment (all countries; determinants/wb/). Unemployment levels (UK ILO, OECD destination mean):
  main null; destination null for British; UK unemployment POSITIVE for non-UK-born British (+0.10 p .02-.04). WDI unemployment starts 1991 and = OECD
  (r 1.000) -> check only. Gini: UK level and UK/(US,CAN) (AUS 13 obs, NZL none; developing origins too sparse -> no UK-to-origin ratio); main null; UK Gini
  +0.046 p .05 for non-UK-born British (projection -> 0.0065 p .34). Reading: the weak (p .06) non-UK-born British projection response is absorbed by any of
  several slow-moving UK-relative series; none touches the controlled equation -> asymmetry holds. Sources used earlier: Maddison 2023 GDP per head,
  UN WPP 2024 youth, OECD destination unemployment (all already in determinants/). Nothing in any manuscript.
- 1 Oct 2026 (45; Sam): occam_british.R (Madigan & Raftery 1994 Occam's window; BIC posteriors, c = 20, 512 models, 1977-2019): MAIN non-EU window = core +
  projection only (PIP 1.00; averaged 0.0067); non-UK-born British: projection PIP 0.00 in the window, relinc_dev 0.73, UK unemployment 0.69 (positive);
  UK-born: core only. Sam: relinc_dev (MPR ln(Y_i)) is the robustness check for the British flow; Gini/unemployment results are not MPR's constructs
  (UK-to-ORIGIN Gini not buildable; UK unemployment wrong sign vs MPR, labour term already in the core). Nothing in any manuscript.
- 1 Oct 2026 (46; Sam): Occam's window is NOT used for the robustness checks (they are only relative; not as refined as the main results). Gini (UK vs
  US/Canada) dropped: not MPR's construct. UK unemployment BESIDE log V/U is invalid as MPR's U (holds V/U fixed); MPR's way = in place of log V/U -> null for
  non-UK-born British (p .77). british_relinc.R: relative income matched to non-UK-born British regions (weights Asia .46, Africa .30, Oceania .18, N America
  .07; 61% covered) = relinc_dev (r 1.00); non-UK-born British: 1.37 p .025, projection 0.0032 p .61. relinc_dev's 7 countries were chosen 23 Sep as main-flow
  origins (determinants_check.R), not for British arrivals -- the region-matched version shows the choice does not matter. Nothing in any manuscript.
- 1 Oct 2026 (47; Sam): influence_cluster.R: tax core robust to leave-one-year-out (0.0059-0.0077), to leaving out every episode (p<.01) and to a
  wild cluster bootstrap by two-year vintage blocks (p<.001 / .007). The clean one-term core is weaker on 1977-2019 (without 2002-07: 0.0035 p .13; cluster
  bootstrap p .019) -- relevant to Sam's pending core decision. Suggested next (not run): DEMIG POLICY index as mechanism/mediation test, specification
  curve for the headline, net non-EU migration outcome. Nothing in any manuscript.
- 1 Oct 2026 (48; Sam): remaining points of failure (IPS lag-2 serial correlation, dating conventions, zero-migration projection 1975+ only) = explain in
  the TEXT, not more tests. Sam's answer on specification search (to refine himself): the core comes from Treasury/OBR/government literature on how fiscal
  policy is set and has been consistent throughout; macro data assessed the other constraints; looking back from the data for missing constraints
  (borrowing/expenditure) was limited and justified by Treasury secrecy and the thin literature. robust_extras.R: specification curve 288/288 positive and
  p<.05 (median 0.0078, range 0.0050-0.0104; specification_curve.png/.csv); net non-EU migration per 1,000 population 0.0158 / 0.0165 (p .016 / <.001; RESET and
  sup-F reject in level form); IPS-precision WLS 0.0064. DEMIG/QuantMig downloads BLOCKED by the network policy (zenodo.org, imi.socsci.ox.ac.uk, etc.) --
  Sam to upload or widen access. Nothing in any manuscript.
