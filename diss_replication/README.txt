diss_replication/ -- CANONICAL replication folder for the thesis and manuscript
(renamed from hpfd/ on 22 Sep 2026).

Data: Dissertation_Dataset_Final.xlsx (sheet ReformSPA), fiscal block rebuilt
from a single source: OBR Historical Public Finances Database in 2024-25 prices
(HM Treasury GDP deflators, March 2026), plus HMRC income-tax payer counts
(Tables 2.1 and 2.1a) and ONS compensation of employees (DTWM).

Run from this folder (setwd here), fresh session:
  source("build_tables.R")   # sources replication.R, writes tables.json
  source("verify.R")         # halts on any cell that moved vs tables_expected.json;
                             # verify_accept() adopts a deliberate change
  source("run_all.R")        # replication, thesis_replication, appendix_exhibits,
                             # robustness_both_specs, mpr_dummies -> run_all_output.txt

tables.json feeds build_docx.js / build_online_appendix.js (in the parent folder
and manuscript_variants/); copy it there after a change and rebuild.
The earlier March-2026 databank run is kept in ../archive_march2026/.
