---
title: "Wissenschaftliche Grundlagen und Aufgaben"
output: 
  html_document:
    keep_md: true
    code_folding: hide
bibliography: references.bib    
---


## Wissenschaftliche Grundlagen

### linearer Trend

Ein einfaches lineares Regressionsmodell kann als:

  $$ \hat{y}_i = a + b \cdot x_i $$

geschrieben werden. Wobei $\hat{y}_i$ die geschetzten WErte der abhängigen Variable, $x_i$ die unabhängige Variable, $a$ der Schnittpunkt mit der y-Achse und $b$ die Steigung der Geraden sind. Um die Parameter $a$ und $b$ zu ermitteln wird normalerweiße die Summe der Abweichugnsquadrate minimiert.

#### Residuen

Als Ressiduen $r_i = y_i - \hat{y}_i$ werden die Differenz zwischen den gemessenen werten $y_i$ und den geschätzten Werten $\hat{y}_i$ bezeichnet."

#### Vorraussetzungen linearer Trend

Das lineare Modell geht davon aus dass:

 1. Die Unabhängige Variable $x$ ist deterministisch (fest gegeben)
 2. Für jedes $x_i$ ist $y_i$ ist eine Zufallsvariable
 3. $y$ ist unabhängig (keine Autokorelation) und identisch verteilt (jedes $y_i$ hat die gleiche Standardverteilung)
 4. Die Residuen $r$ sind normalverteilt
  
### LOESS Glätter

LOESS steht für locally estimated scatterplot smoothing, zu deutsch: lokal gewichtete Regression-Streudiagramm-Glätter. Glätter werden verwendet,um eine Ausgleichslinie durch einen verrauschten Datensatz zu erhalten, z.B. zur bloßen Visualisierungdes  „Trends“  in  einem  Diagramm,  zur  Abtastung  des  mittleren  Verlaufes  oder  auch  zur  Schätzung  vonVertrauensintervallen.

### MannKendall Test

Der MannKendall Test ist ein test für monotone Trends in einer Zeitreihe, basierend auf der Kendall Rang Korrelation der Zeitreihe. Dabei wird die Stärke des monotonen Zusammenhangs zwischen einer Abhängigen und Unabhängigen variable getestet. Der Test ist besonders in Umweltwissenschaften beliebt, da die Bewertungs-Funktion $S$ für kleine $n$ nahezu normalverteilt ist.

## Übungsaufgaben



## Literaturverzeichis

Kendall, M.G. (1976). Rank Correlation Methods. 4th Ed. Griffin.

Hipel, K.W. and McLeod, A.I., (2005). Time Series Modelling of Water Resources and Environmental Systems. Electronic reprint of our book orginally published in 1994. http://www.stats.uwo.ca/faculty/aim/1994Book/.
