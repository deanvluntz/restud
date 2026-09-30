# -*- coding: utf-8 -*-
"""45-page manuscript, v2 (22 Sep 2026): derived from the RESTRUCTURED full manuscript
(manuscript_part1/2.md) by paragraph-level cuts, so the 45pp shares the full paper's
structure, figures, Proposition 1 and Section 7 checks. Paragraphs are matched by
their opening words, not line numbers, so edits elsewhere in the source cannot shift them."""
import io, re
P1 = io.open("manuscript_part1.md", encoding="utf-8").read()
P2 = io.open("manuscript_part2.md", encoding="utf-8").read()

REPLACE = {  # paragraph prefix -> replacement text ('' drops the paragraph)
 "Three limitations should be stated at the outset":
  "The design has three limitations, each addressed as far as the data allow: official projections embed migration assumptions, which Section 7 removes by rebuilding them under zero net migration; the annual sample is short (43 observations); and outside the 2000–2001 episode documented in Section 2.5 the evidence for the mechanism is institutional and statistical rather than documentary, as the model predicts (Section 3). Section 8 sets out the archival and quasi-experimental extensions that would convert the association documented here into a fully identified causal claim.",
 "## 2.6 The institutional argument in summary": "",
 "The sections above establish three premises on which the model": "",
 "Evidence framing His Majesty's Treasury as the principal agent": "",
 "The framework can be read as organising fiscal capacity":
  "The framework organises fiscal capacity along two margins: on the *intensive* margin the government extracts more from the existing working-age population; on the *extensive* margin it enlarges the base, through migration or by raising the state pension age. The sample contains one large realisation of the second instrument, the Pensions Act 1995, legislated fifteen years before it began to bind and named in the 1999 Budget as a key means of reducing long-term spending pressures (Section 2.4); because the projections incorporate legislated pension-age changes, the estimated migration response is identified net of this channel.",
 "Three considerations govern the use of the IPS":
  "The IPS is used for three reasons. It was the basis of the official estimates the government itself used (Portes and Forte, 2017), so it measures flows as policymakers perceived them, and it is used as published contemporaneously rather than as later revised. The adjusted Long-Term International Migration series, which the ONS cautions is not directly comparable (ONS, 2015), begins only in 1991 and serves as a robustness check. And the Census has contradicted the IPS materially only once, on post-2004 EU8 arrivals, a discrepancy on EU rather than non-EU flows. Classical error in the dependent variable inflates standard errors without biasing coefficients, which works against the results below.",
 "*Agenda.* Each limitation admits a concrete remedy":
  "*Agenda.* Each limitation admits a remedy. (i) Refine the zero-migration reconstruction with the vintages' age-specific migration assumptions. (ii) Exploit the biennial NPP releases as information shocks, sharpening the news test of Section 7: decompose each revision into its mortality, fertility and migration-assumption components, and estimate the response of Home Office quarterly visa grants by category to the mortality-and-fertility component alone, news orthogonal to migration by construction. (iii) Take the mechanism to the archives: Budget documents, Treasury fiscal-sustainability reports, and the Treasury files already inside the thirty-year window (HM Treasury, 1997). (iv) Test external validity in a cross-country panel on UN projection vintages. (v) Model the pension-age and migration instruments jointly. (vi) Construct a spending-side measure of the burden, to test whether the tax term stands in for the bill it finances. (vii) Estimate category-level responses on administrative data.",
 "The interpretation is disciplined by what the design cannot yet deliver": "",
 "The premise that migration improves the public finances":
  "The premise was also stated on the record: the Home Office's 2008 evidence to the House of Lords reported its own estimate that migrants paid more in taxes, relative to what they received, than the UK-born, and cited GAD analysis in which higher migration lowered the National Insurance rate needed in 2060–61 from 27.9 to 26.3 per cent (Home Office, 2008). The argument requires only that the government assumed this, not that it was true.",
}
INLINE = [  # within-paragraph trims
 (" attained significance in any variant, unsurprising for output per worker given the UK's prolonged productivity stagnation, and consistent with a policy rule oriented to the extensive rather than the intensive margin.", " attained significance in any variant."),
 (" The Budget-time growth forecast was significant at the five per cent level in some variants, but only at the cost of destabilising the remaining coefficients, and was excluded. The inactivity rate hovered at the margin of significance, was nearly time-invariant over the sample, and was dropped when degrees of freedom required it.", " The Budget-time growth forecast destabilised the remaining coefficients and the inactivity rate was nearly time-invariant; both were excluded."),
]
def transform(text, allbody=None):
    body, sep, refs = text.partition("[[REFS]]")
    out = []
    for line in body.split("\n"):
        hit = [k for k in REPLACE if line.startswith(k)]
        assert len(hit) <= 1
        if hit:
            if REPLACE[hit[0]] == "": continue
            line = REPLACE[hit[0]]
        out.append(line)
    body = "\n".join(out)
    body = re.sub(r"\n{3,}", "\n\n", body)
    for a, b in INLINE:
        body = body.replace(a, b)
    if sep:   # drop person-author references no longer cited in the body
        keep = []
        for r in refs.split("\n"):
            m = re.match(r"([A-Z][A-Za-zÀ-ÿ'’\-]+), [A-Z]\.", r)
            if m and m.group(1) not in (allbody or body): continue
            keep.append(r)
        refs = "\n".join(keep)
    return body + sep + refs
t1 = transform(P1)
t2_body = transform(P2.split("[[REFS]]")[0])
APPX = io.open("build_online_appendix.js", encoding="utf-8").read()   # works cited only in the Online Appendix stay listed
t2 = transform(P2, allbody=t1 + t2_body + APPX)
for k in REPLACE:
    assert k in P1 or k in P2, "prefix not found: " + k
io.open("manuscript_r45_part1.md", "w", encoding="utf-8").write(t1)
io.open("manuscript_r45_part2.md", "w", encoding="utf-8").write(t2)
body = t1 + t2.split("[[REFS]]")[0]
print("r45 v2 words (body, excl. refs):", len(body.split()))
