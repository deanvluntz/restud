# Guide: testing the policy lever (work permits) and coefficient stability

## A. Stability of the projection coefficient (test 3) — ready to run

Script: `diss_replication/stability_check.R` (needs `install.packages("strucchange")`). Run `START <- 1979; source("stability_check.R")`, then again with 1977. It prints three things and writes `stability.png`:

1. **Recursive estimates:** the model re-estimated on 1979–1993, 1979–1994, … 1979–2019, with HAC 95% bands. Shows when the coefficient settles.
2. **Rolling 25-year windows:** 1979–2003, 1980–2004, … 1995–2019. Shows whether any sub-period drives the result.
3. **Break tests:** OLS-CUSUM (has anything in the model shifted?), the sup-F test for a single break at an unknown date (Andrews, 1993), and Bai–Perron for the number of breaks. No rejection = no evidence of a shift.

What to expect (from my replica of your R setup, 1979 sample):

- Recursive: the coefficient is not identified until the sample reaches the late 1990s (to 1993: −0.0002, s.e. 0.020; to 1997: 0.007, s.e. 0.008), then settles at 0.008–0.012 and stays there from 2001 on (to 2009: 0.0087; to 2019: 0.0081). This is not a break: before the mid-1990s the projections barely moved, so there is nothing for the model to estimate. Expect a referee to ask whether the effect is a post-1997 phenomenon. Your answer is the rolling estimates plus the policy-era check.
- Rolling: 16 of 17 windows significant at 5%, coefficient 0.008–0.012 throughout. The 1979–2003 window gives 0.011.
- Break tests: I could not run `strucchange` here; report whatever the script prints.

How to write it up (Section 9, one paragraph plus the figure as Figure 9.1 in the List of Figures): "Recursive estimates settle at the full-sample value once the sample includes the late 1990s, when the projections begin to vary; every 25-year window but one returns a coefficient between 0.008 and 0.012, significant at 5%; the CUSUM and sup-F tests do not reject stability (p = …)."

## B. The policy lever: work permits (test 1)

### Why it matters
The model links projections to inflows, but the argument runs projection → policy instrument → inflows. Showing the instrument itself responding closes the chain. Work permits are the right instrument: until the points-based system (2008) they were the main lever on non-EU labour migration, issued by the Department of Employment / Overseas Labour Service / Work Permits (UK) on a discretionary basis.

### Where the data are
Annual counts of work permits **issued/approved** exist for the whole sample, but in several places:

| Period | Source | Notes |
|---|---|---|
| 1970s–1990s | Home Office, *Control of Immigration: Statistics, United Kingdom* (annual Command Paper, Cm/Cmnd series) | Has a table of work permits and first-permission approvals, from Department of Employment data. In the House of Commons Library and legal deposit libraries; some years digitised on the National Archives website. |
| 1980s–2000s | J. Salt, *International Migration and the United Kingdom: Report of the United Kingdom SOPEMI Correspondent to the OECD* (annual, UCL Migration Research Unit) | Each report carries a work-permit table with a run of back years. Several are online at the UCL Migration Research Unit site. Your 1989 Department of Employment paper (20,814 in 1984; 34,068 in 1988) is consistent with these. |
| 1995–2002 | Clarke, J. and Salt, J. (2003) 'Work permits and foreign labour in the UK: a statistical review', *Labour Market Trends*, 111(11), pp. 563–574 | The standard reference; includes a long-run chart. |
| 2003–2008 | Salt, J. and Millar, J. (2006) 'Foreign labour in the United Kingdom: current patterns and trends', *Labour Market Trends*, 114(10); Home Office / Work Permits (UK) management information | Permits peak around 2004–06. |
| 2008–2019 | Home Office, *Immigration Statistics*, visas granted for work (Tier 2 / Skilled Worker), entry-clearance tables | Different basis (visas, not permits): use a step indicator at 2008 or splice on overlapping years. |
| Any year | Hansard written answers ("work permits issued in each year since …") | Quickest way to fill gaps; search Hansard for "work permits issued". |

Use permits **issued** (approvals), not applications. Take the long-term (12-month-plus) category if the source splits it, since that matches the IPS definition of a migrant.

### The tests (`diss_replication/permits_check.R`, template below)
Put the series in `determinants/work_permits.csv` with columns `year, permits` (and `basis` if the definition changes).

1. **Does the instrument respond to the projection?** Regress log permits per 1,000 working-age population on the same right-hand side as tax_spec (lagged log permits in place of lagged inflows). A positive, significant projection coefficient is the direct test. Report it alongside the inflows result.
2. **Do inflows follow permits?** Regress the inflow rate on lagged log permits plus the controls; then add the projection. If the projection's coefficient falls once permits are included, permits are the channel (mediation). If it does not, inflows respond to the projection through other routes too (students, family), which is worth saying.
3. **Timing:** permits should respond to the projection at least as fast as inflows do (same year or one year ahead). Regress inflows on permits lagged one year and on the projection lagged one year and compare.
4. **Placebo:** total-DR projection and EU inflows have no permit analogue, but the asylum series you are collecting for test 2 serves the same purpose here: asylum grants should not respond to the projection.

### Status (25 Sep 2026)
Done: `determinants/work_permits.csv` digitised from Bauer and Salt (2020) Figure 1; `permits_check.R` written (replaces the template below, which had a bug: it regressed inflows, not permits, in test 1). Results are in the README change log.

### Template (superseded by permits_check.R)
```r
# permits_check.R -- work permits as the policy lever (template; fill determinants/work_permits.csv)
library(readxl); library(lmtest); library(sandwich)
if (!exists("START")) START <- 1979
raw <- as.data.frame(read_excel("Dissertation_Dataset_Final.xlsx", sheet = "ReformSPA"))
raw <- raw[!is.na(raw$year), ]; raw <- raw[order(raw$year), ]
wp  <- read.csv("determinants/work_permits.csv")            # year, permits [, basis]
lag1 <- function(x) c(NA, head(x, -1))
df <- with(raw, data.frame(year = year, log_mig_rate = loglev_nEU_WAPlagK, delta_oadr_10 = delta_oadr_10,
                           tax_workpop = tax_workpop, unem = unem, ln_debt_gdp = ln_debt_gdp, workpop = workpop))
df$permits <- wp$permits[match(df$year, wp$year)]
df$log_permit_rate <- log(1000 * df$permits / lag1(df$workpop))  # per 1,000 of last year's WAP, like the DV
if ("basis" %in% names(wp)) df$basis <- factor(wp$basis[match(df$year, wp$year)])
for (v in c("log_mig_rate", "log_permit_rate", "tax_workpop", "unem", "ln_debt_gdp", "delta_oadr_10"))
  df[[paste0("L_", v)]] <- lag1(df[[v]])
S <- subset(df, year >= START & year <= 2019 & !is.na(permits))
hac <- function(m) coeftest(m, vcov. = vcovHAC(m))
CT <- c("L_tax_workpop", "L_ln_debt_gdp", "L_unem", if ("basis" %in% names(S)) "basis")
cat("\n1. Permits on the projection\n")
m1 <- lm(reformulate(c("L_log_permit_rate", "delta_oadr_10", CT), "log_permit_rate"), data = S); print(hac(m1)[c("delta_oadr_10"), ])
cat("\n2. Inflows on permits, then with the projection added (mediation)\n")
m2 <- lm(reformulate(c("L_log_mig_rate", "log_permit_rate", CT), "log_mig_rate"), data = S); print(hac(m2)["log_permit_rate", ])
m3 <- lm(reformulate(c("L_log_mig_rate", "log_permit_rate", "delta_oadr_10", CT), "log_mig_rate"), data = S)
print(hac(m3)[c("log_permit_rate", "delta_oadr_10"), ])
m0 <- lm(reformulate(c("L_log_mig_rate", "delta_oadr_10", CT), "log_mig_rate"), data = S)
cat(sprintf("projection without permits %.4f, with permits %.4f\n", coef(m0)["delta_oadr_10"], coef(m3)["delta_oadr_10"]))
cat("\n3. Timing: lagged permits and lagged projection\n")
m4 <- lm(reformulate(c("L_log_mig_rate", "L_log_permit_rate", "L_delta_oadr_10", CT), "log_mig_rate"), data = S)
print(hac(m4)[c("L_log_permit_rate", "L_delta_oadr_10"), ])
```

### What would count as success
- Test 1 positive and significant: the Treasury-era projection moves the instrument, not just the outcome.
- Test 2 with the projection's coefficient falling by a third or more when permits enter: permits are the main channel.
- If test 1 fails but the inflows result holds, the honest reading is that the response ran through routes other than work permits (students and dependants), which Section 3.2 already allows for.

## C. Reason-for-migration placebos (test 2) — once you have the data
IPS/LTIM by main reason (work, study, accompany/join, other) runs from 1991; asylum applications from the Home Office run from 1979 or so. Estimate tax_spec with each series as the dependent variable (log of the rate per 1,000 WAP, 1991–2019). Prediction: work and study respond; family and asylum do not. Twenty-nine observations, so present it as a supporting table, not a headline.
