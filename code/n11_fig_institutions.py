# Institutional figure (TikZ): stages of a housing project, contracts, price regulation, finance (panel a)
# and the 2019-2025 episode with the starts/permit ratio (panel b).
# Data-driven parts: high-cost regime dates (out/markov_probs.csv) and starts/permit ratios (out/conversion.csv).
# Output: figures/fig_institutions.tex -> .pdf (vector, Figure 1) and .png (preview)
import csv, os, subprocess, shutil, tempfile
os.chdir(os.path.dirname(os.path.abspath(__file__)))

mk = list(csv.DictReader(open("out/markov_probs.csv")))
reg = [r["Monthly2"][:7] for r in mk if r["D50"] == "1"]
r0, r1 = reg[0], reg[-1]
lab_regime = f"{r0[:4]}:{int(r0[5:7])}--{r1[:4]}:{int(r1[5:7])}"
conv = list(csv.DictReader(open("out/conversion.csv")))

W = 16.0
T0y, T1y = 2019, 2025
NM = (T1y - T0y + 1) * 12
def mpos(s, end=False):
    y, m = int(s[:4]), int(s[5:7]); k = (y - T0y) * 12 + (m - 1) + (1 if end else 0)
    return W * k / NM
def mid(s): return 0.5 * (mpos(s) + mpos(s, True))
steps = []
for r in conv:
    a, b = r["period"].split("-"); ya, ma = a.split(":"); yb, mb = b.split(":")
    a = f"{ya}-{int(ma):02d}"; b = f"{yb}-{int(mb):02d}"
    if b < f"{T0y}-01": continue
    steps.append((max(a, f"{T0y}-01"), min(b, f"{T1y}-12"), float(r["ratio"])))
YB0, YB1 = -7.0, -3.9                     # plot area for ratio 0.4 .. 1.3
def ty(v): return YB0 + (YB1 - YB0) * (v - 0.4) / 0.9

L = []
A = L.append
A(r"""\documentclass[tikz,border=4pt]{standalone}
\usepackage[T1]{fontenc}
\usepackage{helvet}\renewcommand{\familydefault}{\sfdefault}
\usetikzlibrary{arrows.meta,positioning,calc}
\definecolor{ink}{HTML}{0B0B0B}\definecolor{ink2}{HTML}{52514E}\definecolor{grid}{HTML}{E6E5E1}
\definecolor{surf}{HTML}{F0EFEC}\definecolor{costc}{HTML}{EB6834}\definecolor{costl}{HTML}{FCE4D9}
\definecolor{credc}{HTML}{2A78D6}\definecolor{credl}{HTML}{DCEAFB}
\begin{document}
\begin{tikzpicture}[x=1cm,y=1cm,>={Stealth[length=2mm]},font=\footnotesize,
  stage/.style={draw=ink,line width=0.6pt,fill=surf,rounded corners=2pt,text width=2.12cm,minimum height=1.25cm,align=center,inner sep=2.5pt},
  lane/.style={font=\scriptsize\bfseries,text=ink2,anchor=east},
  bar/.style={rounded corners=1.5pt,minimum height=0.44cm,inner sep=2pt,font=\scriptsize,align=center,anchor=west},
  tag/.style={font=\scriptsize\itshape,text=ink2,align=center,anchor=north}]""")
# ------------------------------------------------------------- panel (a)
A(r"\node[anchor=west,font=\small\bfseries,text=ink] at (0,6.35) {(a) Stages of a housing project and where costs, prices and credit enter};")
st = [("Permit", r"approval of the\\housing plan"), ("Contract", r"lump-sum price,\\often no escalation"),
      ("Start", r"start notice\\filed"), ("Presale", r"HUG guarantee,\\price fixed at sale"),
      ("Construction", r"two to three years,\\outlays over time"), ("Completion", r"use approval,\\balance paid")]
xs = [1.25 + 2.7 * i for i in range(6)]
HW = 1.2                                   # half width of a stage box
for i, ((t, d), x) in enumerate(zip(st, xs)):
    extra = ",line width=1.2pt" if t == "Start" else ""
    A(rf"\node[stage{extra}] (s{i}) at ({x:.2f},3.55) {{\textbf{{{t}}}\\[1pt]\scriptsize {d}}};")
for i in range(5):
    A(rf"\draw[->,ink,line width=0.7pt] (s{i}.east) -- (s{i+1}.west);")
A(rf"\node[tag] at ({xs[0]:.2f},2.8) {{permits (MOLIT)}};")
A(rf"\node[tag,font=\scriptsize\bfseries\itshape,text=ink] at ({xs[2]:.2f},2.8) {{starts (MOLIT)}};")
A(rf"\node[tag] at ({xs[3]:.2f},2.8) {{presale price (HUG)\\unsold units (MOLIT)}};")
# cost shock: wide callout with straight arrows into Contract, Start, Construction; dashed into Presale
A(rf"\node[draw=costc,fill=costl,line width=0.8pt,rounded corners=2pt,text width=10.6cm,align=center,inner sep=3pt,font=\scriptsize] (cost) at ({(xs[1]+xs[4])/2:.2f},5.35) "
  rf"{{\textbf{{Cost surge, {lab_regime}}}: input prices rise, raising contract prices and outlays (Propositions 1 and 2); presale prices follow only at revisions of the capped basic cost (Proposition 3)}};")
for i in (1, 2, 4):
    A(rf"\draw[->,costc,line width=0.9pt] ({xs[i]:.2f},4.98) -- (s{i}.north);")
A(rf"\draw[->,costc,line width=0.9pt,dash pattern=on 2.5pt off 1.5pt] ({xs[3]:.2f},4.98) -- (s3.north);")
# lanes below the stages
A(r"\node[lane] at (-0.1,1.75) {Contract};")
A(rf"\node[bar,draw=ink2,fill=white,minimum width={xs[4]+HW-(xs[1]-HW):.2f}cm,text width={xs[4]+HW-(xs[1]-HW)-0.1:.2f}cm] at ({xs[1]-HW:.2f},1.75) "
  r"{fixed price: input-price changes after signing fall on the contractor;\\the standard private contract has specified escalation methods since August 2023};")
A(r"\node[lane] at (-0.1,0.95) {Price rule};")
A(rf"\node[bar,draw=ink2,fill=white,minimum width={2*HW:.2f}cm,text width={2*HW-0.1:.2f}cm] at ({xs[3]-HW:.2f},0.95) {{cap: land + basic\\construction cost + additions}};")
A(rf"\node[font=\scriptsize,text=ink2,anchor=west,align=left] at ({xs[3]+HW+0.12:.2f},0.95) {{public land, and designated private land 2019--2023;\\basic construction cost revised every March and September}};")
A(r"\node[lane] at (-0.1,0.1) {Finance};")
A(rf"\node[bar,fill=credl,minimum width={xs[2]-HW-(xs[0]-HW):.2f}cm] at ({xs[0]-HW:.2f},0.1) {{bridge loan for land, often funded with ABCP}};")
A(rf"\node[bar,fill=credl,minimum width={xs[5]+HW-(xs[2]-HW):.2f}cm] at ({xs[2]-HW:.2f},0.1) {{main project-finance loan, with credit support from the contractor}};")
A(r"\node[lane] at (-0.1,-0.5) {Buyers};")
A(rf"\node[bar,draw=ink2,fill=white,minimum width={xs[5]+HW-(xs[3]-HW):.2f}cm] at ({xs[3]-HW:.2f},-0.5) {{deposit about 10\%, interim payments about 60\%, balance at completion}};")
# credit shock
A(rf"\node[draw=credc,fill=credl,line width=0.8pt,rounded corners=2pt,text width=6.4cm,align=center,inner sep=3pt,font=\scriptsize,anchor=north] (cred) at ({xs[2]-HW:.2f},-0.98) "
  r"{\textbf{Project-finance freeze, from October 2022}:\\bridge loans can no longer be refinanced into main loans};")
A(rf"\draw[->,credc,line width=0.9pt] (cred.north) -- ({xs[2]-HW:.2f},-0.13);")
# ------------------------------------------------------------- panel (b)
A(r"\node[anchor=west,font=\small\bfseries,text=ink] at (0,-2.55) {(b) The 2021--2023 episode: monthly private starts relative to the trailing twelve-month mean of permits};")
A(rf"\fill[costl] ({mpos(r0):.3f},{YB0:.2f}) rectangle ({mpos(r1, True):.3f},{YB1+0.35:.2f});")
A(rf"\node[font=\scriptsize\bfseries,text=costc,anchor=south,fill=costl,inner sep=1.2pt] at ({0.5*(mid('2021-08')+mid('2022-09')):.3f},{YB0+0.06:.2f}) {{high-cost regime}};")
for y in range(T0y, T1y + 1):
    x = mpos(f"{y}-01")
    if y > T0y: A(rf"\draw[grid,line width=0.4pt] ({x:.3f},{YB0:.2f}) -- ({x:.3f},{YB1+0.35:.2f});")
    A(rf"\node[font=\scriptsize,text=ink2,anchor=north] at ({x + W/(2*(T1y-T0y+1)):.3f},{YB0-0.05:.2f}) {{{y}}};")
for v in (0.5, 0.75, 1.0, 1.25):
    A(rf"\draw[{'ink2' if v == 1.0 else 'grid'},line width={'0.5' if v == 1.0 else '0.4'}pt] (0,{ty(v):.3f}) -- ({W:.2f},{ty(v):.3f});")
    A(rf"\node[font=\scriptsize,text=ink2,anchor=east] at (-0.05,{ty(v):.3f}) {{{v:.2f}}};")
A(rf"\node[font=\scriptsize,text=ink2,anchor=south east] at ({W-0.05:.2f},{ty(0.4)+0.03:.3f}) {{national series, averages by period}};")
pts = []
for a, b, v in steps: pts += [(mpos(a), ty(v)), (mpos(b, True), ty(v))]
A(r"\draw[ink,line width=1.2pt] " + " -- ".join(f"({x:.3f},{y:.3f})" for x, y in pts) + ";")
for a, b, v in steps:
    A(rf"\node[font=\scriptsize\bfseries,text=ink,anchor=south] at ({0.5*(mpos(a)+mpos(b, True)):.3f},{ty(v)+0.03:.3f}) {{{v:.2f}}};")
events = [("2021-08", "credc", "August 2021: policy rate raised from 0.50\\%"),
          ("2022-09", "credc", "September 2022: commercial-paper default by a province-backed developer; project-finance freeze"),
          ("2023-01", "ink2", "January 2023: policy rate reaches 3.50\\%; price cap lifted outside four Seoul districts"),
          ("2023-08", "ink2", "August 2023: standard private contract specifies escalation methods")]
for k, (d, col, _) in enumerate(events, 1):
    x = mid(d)
    A(rf"\draw[{col},line width=0.8pt,dash pattern=on 2pt off 1.5pt] ({x:.3f},{YB0:.2f}) -- ({x:.3f},{YB1+0.62:.2f});")
    A(rf"\node[circle,draw={col},fill=white,line width=0.7pt,inner sep=0.6pt,minimum size=0.36cm,font=\scriptsize\bfseries,text={col}] at ({x:.3f},{YB1+0.8:.2f}) {{{k}}};")
key_y = YB0 - 0.62
for k, (d, col, txt) in enumerate(events, 1):
    row, colx = divmod(k - 1, 2)
    A(rf"\node[anchor=north west,font=\scriptsize,text width=7.8cm,align=left] at ({colx*8.1:.2f},{key_y - row*0.62:.2f}) "
      rf"{{\textcolor{{{col}}}{{\textbf{{{k}}}}}\ {txt}}};")
A(r"\end{tikzpicture}")
A(r"\end{document}")
os.makedirs("figures", exist_ok=True)
open("figures/fig_institutions.tex", "w", encoding="utf-8").write("\n".join(L))
tmp = tempfile.mkdtemp(); shutil.copy("figures/fig_institutions.tex", tmp)
subprocess.run(["pdflatex", "-interaction=nonstopmode", "fig_institutions.tex"], cwd=tmp, stdout=subprocess.DEVNULL)
shutil.copy(os.path.join(tmp, "fig_institutions.pdf"), "figures/fig_institutions.pdf")
subprocess.run(["pdftoppm", "-png", "-r", "200", "-singlefile", os.path.join(tmp, "fig_institutions.pdf"), os.path.join(tmp, "fig_institutions")])
shutil.copy(os.path.join(tmp, "fig_institutions.png"), "figures/fig_institutions.png")
log = open(os.path.join(tmp, "fig_institutions.log"), encoding="latin-1").read()
print("errors:", log.count("\n! "), "| overfull:", log.count("Overfull"), "| regime", lab_regime, "| steps", [(a, b, round(v, 2)) for a, b, v in steps])
