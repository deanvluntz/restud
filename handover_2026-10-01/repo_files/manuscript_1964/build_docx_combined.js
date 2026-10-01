const fs = require('fs');
const {
  Document, Packer, Paragraph, TextRun, FootnoteReferenceRun,
  Table, TableRow, TableCell, WidthType, AlignmentType, BorderStyle,
  PageNumber, Footer, PageBreak, VerticalAlign, ImageRun
} = require('docx');

const FONT = 'Times New Roman';
const SZ = 24; // 12pt
const LINE15 = 360; // 1.5 spacing
const LINE1 = 240;

// ---------- inline parser: *italic* and [[FN:n]] ----------
function runs(text, base = {}) {
  const out = [];
  const parts = text.split(/(\[\[FN:\d+\]\]|\*[^*]+\*)/g).filter(s => s !== '');
  for (const p of parts) {
    const fn = p.match(/^\[\[FN:(\d+)\]\]$/);
    if (fn) { out.push(new FootnoteReferenceRun(parseInt(fn[1]))); continue; }
    if (p.startsWith('*') && p.endsWith('*') && p.length > 2) {
      out.push(new TextRun({ text: p.slice(1, -1), italics: true, font: FONT, size: SZ, ...base }));
    } else {
      out.push(new TextRun({ text: p, font: FONT, size: SZ, ...base }));
    }
  }
  return out;
}

function bodyP(text) {
  return new Paragraph({
    alignment: AlignmentType.JUSTIFIED,
    spacing: { line: LINE15, after: 0 },
    indent: { firstLine: 360 },
    children: runs(text),
  });
}
function h1(text) {
  return new Paragraph({
    spacing: { line: LINE15, before: 300, after: 120 },
    keepNext: true,
    children: [new TextRun({ text, bold: true, font: FONT, size: SZ })],
  });
}
function h2(text) {
  return new Paragraph({
    spacing: { line: LINE15, before: 240, after: 100 },
    keepNext: true,
    children: [new TextRun({ text, bold: true, italics: true, font: FONT, size: SZ })],
  });
}
function eqP(text) {
  return new Paragraph({
    alignment: AlignmentType.CENTER,
    spacing: { line: LINE15, before: 60, after: 60 },
    children: [new TextRun({ text, italics: true, font: FONT, size: SZ })],
  });
}
function refP(text) {
  return new Paragraph({
    alignment: AlignmentType.JUSTIFIED,
    spacing: { line: LINE1, after: 80 },
    indent: { left: 360, hanging: 360 },
    children: runs(text),
  });
}

// ---------- table machinery ----------
const TBLNUM = globalThis.TBLNUM || { A1: 'A1', A2: 'A2' };
const NOB = { style: BorderStyle.NONE, size: 0, color: 'FFFFFF' };
const LINEB = { style: BorderStyle.SINGLE, size: 6, color: '000000' };
const FAINTB = { style: BorderStyle.SINGLE, size: 2, color: 'D9D9D9' };

function tCell(lines, { width, align = AlignmentType.CENTER, bold = false, italics = false, topB = false, botB = false, faintB = false } = {}) {
  const paras = lines.map(t => new Paragraph({
    alignment: align,
    spacing: { line: LINE1, after: 0 },
    children: [new TextRun({ text: t, bold, italics, font: FONT, size: SZ })],
  }));
  return new TableCell({
    width: { size: width, type: WidthType.DXA },
    borders: { top: topB ? LINEB : NOB, bottom: botB ? LINEB : (faintB ? FAINTB : NOB), left: NOB, right: NOB },
    verticalAlign: VerticalAlign.CENTER,
    children: paras,
  });
}

// rows: [{cells:[{lines,align,bold,italics}], topB, botB}]
function makeTable(widths, rows, opts = {}) {
  return new Table({
    columnWidths: widths,
    width: { size: widths.reduce((a, b) => a + b, 0), type: WidthType.DXA },
    borders: { top: NOB, bottom: NOB, left: NOB, right: NOB, insideHorizontal: NOB, insideVertical: NOB },
    rows: rows.map((r, ri) => new TableRow({
      cantSplit: true,
      children: r.cells.map((c, i) => tCell(c.lines, {
        width: widths[i],
        align: c.align || (i === 0 ? AlignmentType.LEFT : AlignmentType.CENTER),
        bold: c.bold, italics: c.italics, topB: r.topB, botB: r.botB,
        faintB: opts.sep !== false && !r.botB && !r.topB && ri < rows.length - 1,
      })),
    })),
  });
}

function caption(num, title) {
  return new Paragraph({
    alignment: AlignmentType.CENTER,
    spacing: { line: LINE1, before: 240, after: 120 },
    keepNext: true,
    children: [
      new TextRun({ text: `Table ${num}. `, bold: true, font: FONT, size: SZ }),
      new TextRun({ text: title, font: FONT, size: SZ }),
    ],
  });
}
function tnotes(text) {
  return new Paragraph({
    alignment: AlignmentType.JUSTIFIED,
    spacing: { line: LINE1, before: 100, after: 240 },
    children: [
      new TextRun({ text: 'Notes: ', italics: true, font: FONT, size: SZ }),
      new TextRun({ text, font: FONT, size: SZ }),
    ],
  });
}

// ---------- figures ----------
function figCaption(num, title) {
  return new Paragraph({
    alignment: AlignmentType.CENTER,
    spacing: { line: LINE1, before: 120, after: 100 },
    children: [
      new TextRun({ text: `Figure ${num}. `, bold: true, font: FONT, size: SZ }),
      new TextRun({ text: title, font: FONT, size: SZ }),
    ],
  });
}
function figure(num, file, wPt, hPt, title, notes) {
  return [
    new Paragraph({
      alignment: AlignmentType.CENTER,
      spacing: { before: 200, after: 0 },
      keepNext: true,
      children: [new ImageRun({
        type: 'png',
        data: fs.readFileSync(file),
        transformation: { width: wPt, height: hPt },
      })],
    }),
    figCaption(num, title),
    tnotes(notes),
  ];
}

// coef cell helper
const cc = (coef, se) => ({ lines: se ? [coef, `(${se})`] : [coef] });
const lbl = t => ({ lines: [t], align: AlignmentType.LEFT });

// ---------- Table 1 ----------
function table1() {
  const w = [2500, 4200, 2660];
  const rows = [
    { topB: true, botB: true, cells: [lbl('Variable'), { lines: ['Definition'], align: AlignmentType.LEFT }, { lines: ['Source'], align: AlignmentType.LEFT }].map((c, i) => ({ ...c, bold: true })) },
    { cells: [lbl('Controlled inflow rate'), { lines: ['Gross long-term inflows of non-EU citizens from non-EU countries (1975–2019) and of all non-British citizens (1964–74), per 1,000 working-age residents (lagged denominator); logged; step indicator for the pre-1975 basis'], align: AlignmentType.LEFT }, { lines: ['IPS / ONS'], align: AlignmentType.LEFT }] },
    { cells: [lbl('EU inflow rate'), { lines: ['As above, EU citizens, 1975–2019; used in falsification test'], align: AlignmentType.LEFT }, { lines: ['IPS / ONS'], align: AlignmentType.LEFT }] },
    { cells: [lbl('Projected OADR change, Δ̂d(h)'), { lines: ['Projected old-age dependency ratio h years ahead minus current ratio, from most recent NPP vintage (1963-based to 2018-based); baseline h = 10; tenth year interpolated where unpublished before 1975'], align: AlignmentType.LEFT }, { lines: ['NPP vintages, GAD/ONS, reconstructed (Section 4.2)'], align: AlignmentType.LEFT }] },
    { cells: [lbl('Tax per working-age person'), { lines: ['National accounts taxes, £000s per working-age resident, 2025 prices; lagged'], align: AlignmentType.LEFT }, { lines: ['OBR databank'], align: AlignmentType.LEFT }] },
    { cells: [lbl('Debt/GDP trajectory'), { lines: ['Annual log change in public sector net debt as a share of GDP; lagged'], align: AlignmentType.LEFT }, { lines: ['OBR databank'], align: AlignmentType.LEFT }] },
    { cells: [lbl('Unemployment rate'), { lines: ['ILO measure, per cent; lagged; Bank of England historical rate before 1975'], align: AlignmentType.LEFT }, { lines: ['ONS (LF2Q); Thomas and Dimsdale (2017)'], align: AlignmentType.LEFT }] },
    { botB: true, cells: [lbl('Candidates tested and excluded'), { lines: ['Inactivity rate; real GDP, GDP per capita, and productivity growth; 10-year and 3-month gilt yields; Budget-time growth forecasts'], align: AlignmentType.LEFT }, { lines: ['ONS; FRED; HM Treasury / ESCoE'], align: AlignmentType.LEFT }] },
  ];
  return [caption(1, 'Variable definitions and sources'), makeTable(w, rows), tnotes('Working-age population and dependency ratios use the UK state-pension-age definition (Section 2.4). Interim years between biennial NPP vintages carry forward the most recent vintage. Sample: annual, 1965–2019.')];
}

// ---------- Table 2 (both samples) ----------
function table2() {
  const w = [2760, 1650, 1650, 1650, 1650];
  const F7 = TBL77.table2.fiscal, T7 = TBL77.table2.trend, F6 = TBL.table2.fiscal, T6 = TBL.table2.trend;
  const C = [F7, F6, T7, T6]; const dash = cc('\u2014');
  const r = (label, term, dp) => ({ cells: [lbl(label), ...C.map(c => (term in c.coef) ? ce(c, term, dp) : dash)] });
  const st = (label, f) => ({ cells: [lbl(label), ...C.map(c => cc(f(c)))] });
  const rows = [
    { topB: true, cells: [lbl(''), { lines: ['(1)', 'Fiscal', '1977\u20132019'] }, { lines: ['(2)', 'Fiscal', '1965\u20132019'] }, { lines: ['(3)', 'Trend', '1977\u20132019'] }, { lines: ['(4)', 'Trend', '1965\u20132019'] }] },
    { botB: true, cells: [lbl('Dependent variable:'), ...C.map(() => ({ lines: ['ln controlled inflow rate'] }))] },
    r('Projected 10-yr OADR change, \u0394\u0302d(10)', 'delta_oadr_10', 4),
    r('ln inflow rate (t\u22121)', 'L_log_mig_rate', 3),
    r('Tax per working-age person (t\u22121)', 'L_tax_workpop', 3),
    r('Linear trend', 'trend', 4),
    r('\u0394ln debt/GDP (t\u22121)', 'L_ln_debt_gdp', 3),
    r('Unemployment rate (t\u22121)', 'L_unem', 3),
    r('Pre-1975 basis (non-British citizens)', 'basis_pre1975', 3),
    r('Constant', '(Intercept)', 3),
    { topB: true, ...st('Observations', c => String(c.n)) },
    st('Adjusted R\u00b2', c => num(c.adjr2, 3)),
    st('Breusch\u2013Godfrey (2), p', c => num(c.bg2, 3)),
    st('Breusch\u2013Pagan, p', c => num(c.bp, 3)),
    st('RESET, p', c => num(c.reset, 3)),
    st('Jarque\u2013Bera, p', c => num(c.jb, 3)),
    st('ADF on residuals, p', c => num(c.adf, 3)),
    { botB: true, ...st('Maximum VIF', c => num(c.maxvif, 1)) },
  ];
  return [caption(2, 'Main estimates: fiscal and trend specifications on the core (1977\u20132019) and extended (1965\u20132019) samples'), makeTable(w, rows),
    tnotes('HAC standard errors in parentheses; Newey\u2013West errors (lag 1) are essentially identical (Online Appendix Table B2). Columns (2) and (4) use inflows of non-British citizens to 1974 and of non-EU citizens from 1975 (Section 4.1), with the pre-1975 basis indicator absorbing the change of definition; columns (1) and (3) use non-EU citizens throughout. The tax burden and the linear trend are collinear (r = 0.98) and cannot be included jointly. *** p < 0.01, ** p < 0.05, * p < 0.10.')];
}

// ---------- Table 3 (both samples) ----------
function table3() {
  const w = [3600, 1440, 1440, 1440, 1440];
  const M7 = TBL77.table3, M6 = TBL.table3;
  const lr = (M, t, dp) => { const i = M.terms.indexOf(t); return [cc(est(M.est[i], M.p[i], dp)), cc(num(M.se[i], dp))]; };
  const dterm = M => M.terms.includes('ln_debt_gdp') ? 'ln_debt_gdp' : 'd_ln_debt_gdp';
  const rows = [
    { topB: true, cells: [lbl(''), { lines: ['1977\u20132019'] }, { lines: [''] }, { lines: ['1965\u20132019'] }, { lines: [''] }] },
    { botB: true, cells: [lbl(''), { lines: ['Long-run multiplier'] }, { lines: ['Std. error'] }, { lines: ['Long-run multiplier'] }, { lines: ['Std. error'] }] },
    { cells: [lbl('Projected 10-yr OADR change, \u0394\u0302d(10)'), ...lr(M7, 'delta_oadr_10', 4), ...lr(M6, 'delta_oadr_10', 4)] },
    { cells: [lbl('Tax per working-age person'), ...lr(M7, 'tax_workpop', 4), ...lr(M6, 'tax_workpop', 4)] },
    { cells: [lbl('\u0394ln debt/GDP'), ...lr(M7, dterm(M7), 3), ...lr(M6, dterm(M6), 3)] },
    { cells: [lbl('Unemployment rate'), ...lr(M7, 'unem', 3), ...lr(M6, 'unem', 3)] },
    { topB: true, cells: [lbl('Bounds F-statistic (H\u2080: no level relationship)'), cc(num(M7.boundsF.stat, 2)), cc('p = ' + num(M7.boundsF.p, 2)), cc(num(M6.boundsF.stat, 2)), cc('p = ' + num(M6.boundsF.p, 2))] },
    { botB: true, cells: [lbl('Bounds t-statistic'), cc(num(M7.boundsT.stat, 2)), cc('p = ' + num(M7.boundsT.p, 2)), cc(num(M6.boundsT.stat, 2)), cc('p = ' + num(M6.boundsT.p, 2))] },
  ];
  return [caption(3, 'Long-run multipliers and bounds tests, fiscal specification, both samples'), makeTable(w, rows),
    tnotes('Long-run multipliers from the error-correction representation of the fiscal specification of Table 2 on each sample; on 1965\u20132019 the pre-1975 basis indicator enters as a fixed regressor. Bounds tests of Pesaran, Shin and Smith (2001), case 3, with exact small-sample p-values from the R package ARDL (Natsiopoulos, Tzeremes and Finnan, 2026). *** p < 0.01, ** p < 0.05, * p < 0.10.')];
}

// ---------- Table 4 ----------
function table4() {
  // Table 4 is typeset in the FISCAL specification once build_tables.R has been
  // re-run (TBL.table4.spec === 'fiscal'); until then the stored trend set is
  // used and labelled as such, so the build never mislabels its own numbers.
  return table4Of(TBL.table4, 4);   // extended sample (Sam, 28 Sep 2026: extended first, core as corroboration)
}
// Shared renderer: the online appendix calls it with TBL.table4_trend as Table B3.
function table4Of(K, tblno) {
  const fiscal = K.spec === 'fiscal', hasLab = fiscal && K.lab;
  const w = hasLab ? [1900, 1240, 1240, 1240, 1240, 1240, 1240] : [2360, 1400, 1400, 1400, 1400, 1400];
  const C = [K.dr, K.eu, K.gross, K.level, K.zm, ...(hasLab ? [K.lab] : [])];
  const proj = ['delta_dr_10','delta_oadr_10','delta_oadr_10','level_oadr_10','delta_zm_10','delta_oadr_10'];
  const lagd = ['L_log_mig_rate','L_eu_log_mig_rate','L_alt_log_mig_rate','L_log_mig_rate','L_log_mig_rate','L_log_mig_rate'];
  const taxv = ['L_tax_workpop','L_tax_workpop','L_tax_workpop','L_tax_workpop','L_tax_workpop','L_tax_lab_pw'];
  const row = (label, term, dp) => ({ cells: [lbl(label), ...C.map((c,i) => ce(c, typeof term === 'function' ? term(i) : term, dp))] });
  const rows = [
    { topB: true, botB: true, cells: [lbl(''), { lines: ['(1)', 'Total DR placebo'] }, { lines: ['(2)', 'EU inflows placebo'] }, { lines: ['(3)', 'Gross controlled inflows'] }, { lines: ['(4)', 'OADR level'] }, { lines: ['(5)', 'Zero-mig. \u0394\u0302'] }, ...(hasLab ? [{ lines: ['(6)', 'Labour tax'] }] : [])] },
    row('Projection term', i => proj[i], 4),
    row('ln dep. var. (t\u22121)', i => lagd[i], 3),
    fiscal ? row('Tax per working-age person (t\u22121)', i => taxv[i], 4) : row('Linear trend', 'trend', 4),
    row('\u0394ln debt/GDP (t\u22121)', 'L_ln_debt_gdp', 3),
    row('Unemployment (t\u22121)', 'L_unem', 3),
    { cells: [lbl('Pre-1975 basis'), ...C.map(c => ('basis_pre1975' in c.coef) ? ce(c, 'basis_pre1975', 3) : cc('\u2014'))] },
    { topB: true, cells: [lbl('Adjusted R\u00b2'), ...C.map(c => cc(num(c.adjr2,3)))] },
    { cells: [lbl('Sample'), ...C.map(c => cc(c.years || '1977\u20132019'))] },
    { botB: true, cells: [lbl('Observations'), ...C.map(c => cc(String(c.n)))] },
  ];
  const cap = fiscal ? 'Falsification and robustness tests, fiscal specification' : 'Falsification and robustness tests, trend specification';
  const notes = fiscal
    ? 'HAC standard errors in parentheses, as in Table 2. Column (1) replaces the projected 10-year OADR change with the projected 10-year change in the total dependency ratio. Column (2) re-estimates the baseline on inflows of EU citizens. Column (3) uses gross non-EU inflows without population scaling. Column (4) replaces the projected change with the projected 10-year level of the OADR. Column (5) replaces the published projection with its zero-net-migration counterpart (Online Appendix A).' + (hasLab ? ' Column (6) replaces national accounts taxes per working-age person with income tax and National Insurance contributions per working-age person (OBR Historical Public Finances Database, 2024–25 prices).' : '') + '' + (hasLab ? ' Column (6) replaces total National Accounts taxes per working-age person with income tax and National Insurance contributions per working-age person (OBR Historical Public Finances Database), the taxes levied on earnings.' : '') + ' The trend-specification counterparts are in Online Appendix Table B3; the LTIM-based check (1992–2019, not shown) returns a projection coefficient significant at the 0.1 per cent level. Columns (1), (3), (4) and (6) are estimated on the extended sample, 1965–2019; columns (2) and (5) use the 1977–2019 years for which EU inflows and components of change exist. The core-sample counterparts are reported in the text. *** p < 0.01, ** p < 0.05, * p < 0.10.'
    : 'HAC standard errors in parentheses, as in Table 2. Column (1) replaces the projected 10-year OADR change with the projected 10-year change in the total dependency ratio. Column (2) re-estimates the baseline on inflows of EU citizens. Column (3) uses gross non-EU inflows without population scaling. Column (4) replaces the projected change with the projected 10-year level of the OADR. Column (5) replaces the published projection with its zero-net-migration counterpart (Online Appendix A); in the fiscal specification the same regressor yields 0.0075 (HAC s.e. 0.0013) with all controls significant at the 5 per cent level under HAC inference. The LTIM-based check (1991–2019, not shown) returns a projection coefficient significant at the 5 per cent level. *** p < 0.01, ** p < 0.05, * p < 0.10.';
  return [caption(tblno, cap), makeTable(w, rows), tnotes(notes)];
}

// ---------- table values, computed rather than typed -------------------------
// Every cell in Tables 2, 3, 4 and B2 is read from tables.json, written by
// build_tables.R from the fitted models. Hand-typing these is how a constant
// once read 0.355 where R returned 0.371, and how two cells of Table 4 carried
// a double-rounding error: 0.003494 shown as 0.004 because 0.0035, R's own
// four-decimal display, had been rounded a second time.
//
// Rounding therefore happens ONCE, here, from full precision. Decimal places
// are fixed per row rather than inferred, because the paper's convention
// differs by row (four places for projection terms and the trend, three
// elsewhere) and inferring it would silently change the published format.
const TBL = JSON.parse(fs.readFileSync(process.env.MS_TABLES || 'tables_1964.json', 'utf8'));
const TBL77 = JSON.parse(fs.readFileSync('tables_1977.json', 'utf8'));   // Table 3 (bounds) until the 1964 R run replaces it

const MINUS = '−';                    // U+2212, not a hyphen
function stars(p) { return p < 0.01 ? '***' : p < 0.05 ? '**' : p < 0.10 ? '*' : ''; }
function num(v, dp) {                      // half-up from full precision
  const s = Math.abs(v).toFixed(dp);
  return (v < 0 ? MINUS : '') + s;
}
function est(v, p, dp) { return num(v, dp) + (p === null ? '' : stars(p)); }

// coefficient over standard error, from a column object emitted by grab()
function ce(col, term, dp) {
  if (!(term in col.coef)) throw new Error('tables.json: no term ' + term);
  return cc(est(col.coef[term], col.p[term], dp), num(col.se[term], dp));
}

// ---------- Table A1 ----------
function tableA1() {
  const w = [2760, 1300, 1300, 1300, 1300, 1400];
  const d = [
    ['Non-EU inflow rate', '>0.10', '<0.01', '>0.10', '>0.10', 'I(1)'],
    ['ln non-EU inflow rate', '>0.10', '<0.01', '0.10', '>0.10', 'I(1)'],
    ['ln gross non-EU inflows', '>0.10', '<0.01', '>0.10', '>0.10', 'I(1)'],
    ['ln EU inflow rate', '>0.10', '<0.01', '>0.10', '>0.10', 'I(1)'],
    ['OADR, current', '>0.10', '>0.10', '>0.10', '>0.10', 'I(1)\u2020'],
    ['Projected 10-yr OADR change', '>0.10', '<0.05', '0.09', '>0.10', 'I(1)'],
    ['Projected 10-yr OADR level', '>0.10', '<0.01', '0.04', '>0.10', 'I(1)'],
    ['Tax per working-age person', '>0.10', '<0.01', '>0.10', '>0.10', 'I(1)'],
    ['Debt/GDP (level, not used)', '>0.10', '<0.05', '<0.01', '0.08', 'I(1)'],
    ['\u0394ln debt/GDP (in model)', '<0.10', '<0.01', '>0.10', '>0.10', 'I(1)'],
    ['Unemployment rate', '<0.05', '<0.05', '>0.10', '>0.10', 'I(0)'],
    ['Inactivity rate', '>0.10', '<0.01', '>0.10', '>0.10', 'I(1)'],
    ['ln real GDP per capita', '<0.05', '<0.01', '>0.10', '>0.10', 'I(0)'],
    ['ln real GDP', '<0.05', '<0.01', '>0.10', '>0.10', 'I(0)'],
    ['ln output per worker', '<0.01', '<0.01', '>0.10', '>0.10', 'I(0)'],
    ['10-year gilt yield', '<0.01', '<0.01', '>0.10', '>0.10', 'I(0)'],
    ['Growth forecast, year 0', '<0.05', '<0.01', '>0.10', '>0.10', 'I(0)'],
  ];
  const rows = [
    { topB: true, botB: true, cells: [lbl('Variable'), { lines: ['ADF p', '(level)'] }, { lines: ['ADF p', '(diff.)'] }, { lines: ['KPSS p', '(level)'] }, { lines: ['KPSS p', '(diff.)'] }, { lines: ['Order'] }].map(c => ({ ...c, bold: true })) },
    ...d.map((r, i) => ({ botB: i === d.length - 1, cells: [lbl(r[0]), { lines: [r[1]] }, { lines: [r[2]] }, { lines: [r[3]] }, { lines: [r[4]] }, { lines: [r[5]] }] })),
  ];
  return [caption(TBLNUM.A1, 'Orders of integration: sequential ADF and KPSS tests'), makeTable(w, rows), tnotes('Sequential classification: ADF with constant and trend on levels, constant only on first differences, lag order by BIC; a series is I(0) if the level test rejects at 5 per cent, I(1) if the level test fails and the difference test rejects. KPSS (null of stationarity) is the confirmatory mirror; where its statistic falls outside the tabled range the p-value is reported as >0.10 or <0.01, as for ADF. \u2020 For this series ADF does not reject a unit root in the first difference. The more powerful ADF-GLS test of Elliott, Rothenberg and Stock does reject at short lag orders: the difference rejects at one and at two lags, while at four lags, where forty-two differenced observations must support four lag terms alongside GLS detrending, it does not. KPSS does not reject stationarity of the difference. Integration of order two is therefore not supported and the series is classified I(1). It is a smooth series on which unit-root tests have little power at T = 43, and it is not a regressor: the current OADR enters only inside the projection variable, a within-vintage difference across horizons that removes the common demographic trend. Tests disagree on the projection variable itself only at the longest lag order: ADF-GLS reads it as trend-stationary at four lags but as I(1) at one and at two, agreeing there with the ADF sequence. Sensitivity of this kind is characteristic of short samples and is the reason the bounds procedure is the framework adopted here: it is valid for any combination of I(0) and I(1) regressors, so the analysis does not rest on resolving these classifications.')];
}

// ---------- Table A2 ----------
function tableA2() {
  const w = [3160, 1700, 1500, 1500, 1500];
  const B = TBL.tableB2, ix = t => B.terms.indexOf(t);
  // dp per row: the projection term is reported to four places, the rest to three
  const r = (label, term, dp) => { const i = ix(term);
    if (i < 0) throw new Error('tables.json: no B2 term ' + term);
    return { cells: [lbl(label), { lines: [num(B.coef[i], dp)] }, { lines: [num(B.ols[i], dp)] },
                     { lines: [num(B.hac[i], dp)] }, { lines: [num(B.nw[i], dp)] }] }; };
  const rows = [
    { topB: true, botB: true, cells: [lbl('Regressor'), { lines: ['Coefficient'] }, { lines: ['OLS s.e.'] }, { lines: ['HAC s.e.'] }, { lines: ['NW s.e.'] }].map(c => ({ ...c, bold: true })) },
    r('ln inflow rate (t\u22121)', 'L_log_mig_rate', 3),
    r('Tax per working-age person (t\u22121)', 'L_tax_workpop', 3),
    r('Projected 10-yr OADR change', 'delta_oadr_10', 4),
    r('\u0394ln debt/GDP (t\u22121)', 'L_ln_debt_gdp', 3),
    r('Unemployment rate (t\u22121)', 'L_unem', 3),
    { ...r('Constant', '(Intercept)', 3), botB: true },
  ];
  return [caption(TBLNUM.A2, 'Fiscal specification: coefficient stability across inference methods'), makeTable(w, rows), tnotes('OLS, heteroskedasticity-and-autocorrelation-consistent (HAC), and Newey–West (lag 1) standard errors for specification (1) of Table 2. The projection term is significant at the 1 per cent level under all three.')];
}

const FIGSCALE = Number(process.env.MS_FIGSCALE || 1);   // 30pp variant shrinks Figures 3-4 to fit its limit
const FIGS = {
  FIGURE1: () => figure(1, 'figure1_1964.png', 458, 324,
    'Controlled inflows and projected old-age dependency, 1964\u20132019',
    'Upper panel: gross long-term inflows of non-British citizens to 1974 and of non-EU citizens from 1975 (IPS; the dotted line marks the change of basis). Lower panel: the projected ten-year change in the OADR from the vintage in force, as published and under zero net migration (Online Appendix A). The shaded band marks 1998–2007, the policy era to which the structural-change literature attributes the expansion (Section 7). The published series understates projected ageing from the mid-1990s because successive vintages assumed higher net migration.'),
  FIGURE2: () => figure(2, 'figure2_1964.png', 458, 204,
    'Added-variable plots: the projection term on controlled and uncontrolled margins',
    'Each panel plots the residuals of the log inflow rate against the residuals of the projected ten-year OADR change, both residualised on the remaining regressors of the trend specification (lagged dependent variable, trend, lagged debt trajectory, lagged unemployment). Fitted slopes equal the regression coefficients: 0.0073 for non-EU inflows, which policy rationed, and 0.0017 for EU inflows, which it could not. N = 43 in each panel.'),
  FIGURE3: () => figure(3, 'figure3.png', Math.round(458 * FIGSCALE), Math.round(225 * FIGSCALE),
    'The projection coefficient by horizon: old-age against total dependency',
    'Each pair of estimates comes from the fiscal specification with the projected change in the dependency ratio at horizon h in place of the ten-year change: filled markers the old-age ratio, open markers the total dependency ratio (placebo). Bars are 95 per cent HAC intervals. 1977–2019, N = 43 in every regression.'),
  FIGURE4: () => figure(4, 'figure4_1964.png', Math.round(458 * FIGSCALE), Math.round(390 * FIGSCALE),
    'The projection coefficient across all robustness variants',
    'Each row re-estimates the fiscal specification with one change, as described in Section 7; filled markers are variants of the test, open markers the placebos; the total-dependency ratio is shown on its own and alongside the old-age ratio (Section 7). Bars are 95 per cent HAC intervals; the dashed line marks the baseline estimate. N = 43 except where the sample is shortened (1979: 41; 1983: 37; 1987: 33; LTIM: 28).')
};
const TABLES = { TABLE1: table1, TABLE2: table2, TABLE3: table3, TABLE4: table4, TABLEA1: tableA1, TABLEA2: tableA2 };

// ---------- parse manuscript ----------
// Input/output are overridable so the ReStud-structured variants build from the same pipeline:
//   MS_PART1=manuscript_r45_part1.md MS_PART2=manuscript_r45_part2.md MS_OUT=Hamdane_ReStud_Manuscript_45pp.docx node build_docx.js
const MS_PART1 = process.env.MS_PART1 || 'ms1964/manuscript_part1_combined.md';
const MS_PART2 = process.env.MS_PART2 || 'ms1964/manuscript_part2_combined.md';
const MS_OUT   = process.env.MS_OUT   || 'Hamdane_ReStud_Manuscript_combined.docx';
const raw = fs.readFileSync(MS_PART1, 'utf8') + '\n' + fs.readFileSync(MS_PART2, 'utf8');
const lines = raw.split('\n');

const children = [];
const footnoteDefs = {};
let mode = 'body';
let eqBuf = [];

for (let i = 0; i < lines.length; i++) {
  const line = lines[i].trimEnd();
  if (line === '[[FOOTNOTES]]') { mode = 'footnotes'; continue; }
  if (mode === 'footnotes') {
    if (line.trim() === '') continue;
    const m = line.match(/^(\d+)\|(.*)$/);
    if (m) footnoteDefs[parseInt(m[1])] = m[2];
    continue;
  }
  if (line === '[[TITLENOTE]]') { children.push(new Paragraph({ spacing: { before: 1800, line: LINE15 },
    children: [new TextRun({ text: 'The titular claim is given further empirical backing in Online Appendix C.', italics: true, font: FONT, size: SZ })] })); continue; }
  if (line === '[[REFS]]') { mode = 'refs';
    // MS_REFS_BREAK=0 runs the references straight on from the conclusion (journal style); default keeps the page break.
    // pageBreakBefore on the heading (not a separate break paragraph) so a body that ends exactly at
    // the foot of a page cannot leave an empty page before the references.
    const refBreak = (process.env.MS_REFS_BREAK || '1') !== '0';
    children.push(
    new Paragraph({ pageBreakBefore: refBreak, spacing: { line: LINE15, before: 300, after: 120 }, keepNext: true, children: [
      new TextRun({ text: 'References', bold: true, font: FONT, size: SZ }),
      new TextRun({ text: '          (Digital references and URLs are found in Online Appendix F)', font: FONT, size: SZ }),
    ] })); continue; }
  if (line === '[[APPENDIX]]') { mode = 'body'; continue; }
  if (mode === 'refs') {
    if (line.trim() === '') continue;
    children.push(refP(line));
    continue;
  }
  if (line === '[[EQ]]') { mode = 'eq'; eqBuf = []; continue; }
  if (line === '[[/EQ]]') { mode = 'body'; eqBuf.forEach(l => children.push(eqP(l))); continue; }
  if (mode === 'eq') { if (line.trim() !== '') eqBuf.push(line.trim()); continue; }
  const tm = line.match(/^\[\[(TABLE[A-Z0-9]+)\]\]$/);
  if (tm && TABLES[tm[1]]) { TABLES[tm[1]]().forEach(el => children.push(el)); continue; }
  const fm = line.match(/^\[\[(FIGURE[0-9]+)\]\]$/);
  if (fm && FIGS[fm[1]]) { FIGS[fm[1]]().forEach(el => children.push(el)); continue; }
  if (line.startsWith('## ')) { children.push(h2(line.slice(3))); continue; }
  if (line.startsWith('# ')) { children.push(h1(line.slice(2))); continue; }
  if (line.trim() === '') continue;
  children.push(bodyP(line));
}

// ---------- footnotes object ----------
const footnotes = {};
for (const [id, text] of Object.entries(footnoteDefs)) {
  footnotes[id] = {
    children: [new Paragraph({
      alignment: AlignmentType.JUSTIFIED,
      spacing: { line: LINE1, after: 60 },
      children: [new TextRun({ text: ' ' + text, font: FONT, size: SZ })],
    })],
  };
}

// ---------- title page ----------
const titlePage = [
  new Paragraph({ spacing: { before: 480, line: LINE15 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: 'Death, taxes, and immigration: how Britain’s Treasury delayed the inevitable (1964–2019)', bold: true, font: FONT, size: 24 })] }),
  new Paragraph({ spacing: { before: 360, line: LINE15 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: 'Sam Hamdane', font: FONT, size: SZ })] }),
  new Paragraph({ spacing: { line: LINE15 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: 'Institute of Economic History, Humboldt University of Berlin', font: FONT, size: SZ })] }),
  new Paragraph({ spacing: { line: LINE15 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: 'Marienhöhe 25, 15569 Woltersdorf, Brandenburg, Germany', font: FONT, size: SZ })] }),
  new Paragraph({ spacing: { line: LINE15 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: 'Corresponding author: samhamdane@icloud.com', font: FONT, size: SZ })] }),
  new Paragraph({ spacing: { line: LINE15 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: '| DE +49 17632562965 | UK +44 7473603712', font: FONT, size: SZ })] }),
  new Paragraph({ spacing: { line: LINE15, after: 240 }, alignment: AlignmentType.CENTER, children: [new TextRun({ text: 'September 2026', font: FONT, size: SZ })] }),
  new Paragraph({ spacing: { before: 120, line: LINE15 }, alignment: AlignmentType.CENTER, keepNext: true, children: [new TextRun({ text: 'Abstract', bold: true, font: FONT, size: SZ })] }),
  new Paragraph({
    alignment: AlignmentType.JUSTIFIED, spacing: { line: LINE15 }, indent: { left: 360, right: 360 },
    children: [new TextRun({ text: 'Successive governments campaigned to cut or control immigration from outside Europe, yet gross controlled inflows fell for a decade after 1964 and then nearly quintupled over the next four. Means and motive implicate the Treasury, whose budgetary authority spans the Home Office and social-security. I argue immigration is deployed to stall deterioration of Britain’s old-age dependency ratio (OADR), thereby delaying unpopular tax increases that population ageing implies — a trade-off declassified Whitehall records document. Labour-supply growth diffuses the fiscal burden, avoiding reform. My model prices instruments by political exposure: Budget rejection topples governments; entry-rule changes find empty benches in Parliament. I reconstruct official OADR forecasts from the 1960s onwards (OADR: 1 point = 1 pensioner per 1,000 working-age adults). ARDL estimates over 1965–2019 predict 0.8 per cent higher controlled inflows on impact for each additional point the ten-year projection foresees — conditional on one-year lags; tax burden, debt, and unemployment — and the estimate is unchanged by adding fourteen years under a different entry regime. Robustness and falsification agree: zero-net-migration projections yield near-identical estimates; work permits, the instrument itself, respond; the total dependency ratio carries nothing beside the old-age ratio, and EU inflows show no response.', font: FONT, size: SZ })],
  }),
  new Paragraph({ spacing: { before: 240, line: LINE15 }, indent: { left: 360, right: 360 }, children: [
    new TextRun({ text: 'Keywords: ', italics: true, font: FONT, size: SZ }),
    new TextRun({ text: 'immigration policy; fiscal policy; population ageing; dependency ratio; political economy of policy instruments.', font: FONT, size: SZ }),
  ] }),
  new Paragraph({ spacing: { line: LINE15 }, indent: { left: 360, right: 360 }, children: [
    new TextRun({ text: 'JEL codes: ', italics: true, font: FONT, size: SZ }),
    new TextRun({ text: 'E62, F22, H68, J11.', font: FONT, size: SZ }),
  ] }),
  new Paragraph({ children: [new PageBreak()] }),
];

const doc = new Document({
  creator: 'Sam Hamdane',
  title: 'Death, taxes, and immigration: how Britain’s Treasury delayed the inevitable (1964–2019)',
  subject: 'Manuscript',
  lastModifiedBy: 'Sam Hamdane',

  styles: { default: { document: { run: { font: FONT, size: SZ } } } },
  footnotes,
  sections: [{
    properties: { page: { size: { width: 12240, height: 15840 }, margin: { top: 1440, bottom: 1440, left: 1440, right: 1440 } } },
    footers: {
      default: new Footer({
        children: [new Paragraph({
          alignment: AlignmentType.CENTER,
          children: [new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: SZ })],
        })],
      }),
    },
    children: [...titlePage, ...children],
  }],
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync(MS_OUT, buf);
  console.log('written', buf.length, 'bytes');
});
