# Manuscript, 1964–2019 version (26 Sep 2026)

`Hamdane_ReStud_Manuscript_1964.docx` (57 pp) is the manuscript rebuilt on the extended sample: series B
(non-British citizens to 1974, non-EU citizens from 1975), 1965–2019, N = 55, with the title dated 1964–2019.
It is a copy of the 1977–2019 manuscript with the timeline, numbers and sections edited; the 1977 document
and its sources are untouched.

Build: `node build_docx_1964.js` in this folder (needs `npm install docx`, or the node_modules of the
1977 build). Sources: `ms1964/manuscript_part1_1964.md`, `ms1964/manuscript_part2_1964.md`
(derived from the 1977 sources by `ms1964/edit_part1.py` / `edit_part2.py`, kept for the diff).
Tables 2, 4 and B2 and the Figure 4 rows are read from `tables_1964.json`; Table 3 from `tables_1977.json`.

STATUS OF THE NUMBERS (26 Sep 2026)
- `tables_1964.json` is Sam's R run of `diss_replication/manuscript_1964.R` (R 4.6.1); it matched the Python
  replica in every cell. Table 3 is now on 1965–2019 (bounds F = 7.18, p = 0.003; t = −4.95, p = 0.007).
- Figure 3 (horizons) remains on 1977–2019 because the pre-1975 vintages do not publish every horizon; the
  text says so. Figures 1, 2 and 4 are Python-rendered (matplotlib); an R rendering is optional.
- Exercises that need data the pre-1975 vintages or surveys do not supply are run on 1977–2019 and say so:
  zero-net-migration, publication dating, news vs level, EU margin, LTIM, horizons, household/VAT tax
  measures, the MPR interaction and differenced checks. Realised spending and the British regional
  breakdown are quoted from the thesis (1979–2019).
- New in this version: Section 7 'The extension itself', 'Stability over time', 'The instrument' (permits
  1965–2008), 'Realised spending', 'Supply-side controls', 'British citizens'; Section 2.1 Healey/Benn/Rogers
  paragraph; Section 2.3 pre-1973 instruments; Section 2.4 Hemming & Kay horizon; Section 4 rewritten for the
  splice, the pre-1975 vintages and the BoE unemployment series; bias correction re-run (1.8 per cent long run).
- Page count 57 against the 45-page limit: cuts are Sam's.
