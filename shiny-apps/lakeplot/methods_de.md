---
title: "Wissenschaftliche Grundlagen und Aufgaben"
output: 
  html_document:
    keep_md: true
    code_folding: hide
bibliography: references.bib    
---



## Wissenschaftliche Grundlagen

### Thermokline und 10 Grad Celsius-Isotherme

Für die Plots wird die Thermokline mit Hilfe des **R**-Pakets
[rLakeAnalyzer](https://cran.r-project.org/web/packages/rLakeAnalyzer/)
berechnet.

Generell existieren für die Berechnung der Sprungsschicht ($z_{mix}$)
bzw.  Thermokline unterschiedliche Definitionen und
Berechnungsverfahren, siehe z.B.  [Winslow et
al. 2019](https://cran.r-project.org/web/packages/rLakeAnalyzer/vignettes/sm_algorithm.html).

Als einfache Faustregel wird oft die Tiefe angegeben, bei der der
Temperaturgradient mehr als 1 Grad pro Meter (1K/m) beträgt.  Die
vorliegende App nutzt den **rLakeAnalyzer**-Algorithmus, das
Verständnis der Funktiosweise belassen wir als Aufgabe (siehe unten).

In der Realität sind Durchmischungstiefe bzw. Thermokline physikalisch
komplex gesteuert. Außerdem sind die physikalischen Gradienten
(z.B. Temperatur, Dichte, Turbulenz) in der Realität kontinuierlich und
ändern sich dynamisch.  Feste Grenzen gibt es eigentlich nicht, als
Indikatoren und als Werkzueg für weitere Berechnungen sind sie jedoch
sehr nützlich.  Ein weiterer noch einfacherer pragmatischer Indikator
ist die 10 Grad Celsius-Isotherme.

### Sauerstoffsättigungskonzentration

Die Sauerstoffsättigung ist das Verhältlis zwischen der gemessenen
Sauerstoffkonzentration (in mg/L bzw. mmol/L) und der theoretischen
Konzentration von sauerstoffgesättigtem Wasser bei einem bestimmten
Luftdruck und einer bestimmten Temperatur.  Zur Abschätzung existieren
verschiedene empirische Formeln, z.B. die recht einfache Formel von
Mortimer (1981):

\[
C_{O2, sat} = \exp\left( 7.7117 − 1.31403 \cdot \log\left(T + 45.93\right)\right) \cdot
\frac{p}{1013.25}
\]

mit Temperatur $T$ in Grad Celsius und Luftdruck $p$ in Hektopascal.
Die Sättigungskonzentration nach dieser Formel hat die Maßeinheit g
O/m$^3$ bzw. mg O/L.  Weitere Formeln und entsprechende
Literaturangaben finden sich im R-Paket
[marelac](https://CRAN.R-project.org/package=marelac).

### Unterwasserlichtprofil und euphotische Zone

Die Unterwasser-Lichtintensität $I_z$ ($I=$ irradiation) in einer
bestimmten Tiefe $z$ ergibt sich aus der Lichtintensität unmittelbar
unter der Wasseroberfläche $I_0$ über das Lambert-Beer'sche Gesetz:

\[
I_z = I_0 \cdot e^{-\varepsilon \cdot z}
\]

Hierbei wird angenommen, dass der Lichtextinktionskoeffizient
$\varepsilon$ (in manchen Büchern $k_d$ genannt) über die Tiefe
konstant ist. Das ist aus mehreren Gründen eine Vereinfachung, weil:

* die Lichtextinktion von der Wellenlänge abhängig ist und
* Färbung und Partikel im Wasser nicht gleichmäßig verteilt sind.

Aus diesem Grund ist der mit Hilfe eines Unterwasserlichtsensors
gemessene $\varepsilon$-Wert ein Mittelwert über die Tiefe und über
einen bestimmten Spektralbereich, z.B. das sichtbare Licht oder den
photosynthetisch aktiven Bereich (photosynthetisch aktive Strahlung
PAR). Man spricht deshalb vom "mittleren vertikalen und mittleren
spektralen Lichtextinktionskoeffizient".

Der Koeffizient kann mit Hilfe der logarithmisch-transformierten Form
des Lambert-Beerschen Gesetzes:

\[
\ln(I_z) = \ln(I_0)  -\varepsilon \cdot z
\]

bestimmt werden, analog einer linearen Regression. Die Steigung $b$ der Gerade 
entspricht  dem Extinktionskoeffizienten $\varepsilon$:

\[
y = a  - b \cdot x
\]

### Limnologische Wechselwirkungen zwischen Licht, Temperatur, Sauerstoff und Leitfähigkeit

Die hydrophysikalischen, chemischen und biologischen Variablen
aquatischer Ökosysteme werden durch meteorologische, hydrologische und
andere saisonale Faktoren gesteuert und beeinflussen sich
gegenseitig. Eine umfassende Beschreibung würde den Rahmen dieses
Textes überschreiten, deshalb wird auf die Vorlesung und die
Lehrbücher verwiesen.

Als Anregung für Wiederholung und Selbststudium dienen die folgenden Stichworte und Fragen:

* Saisonalität und Schichtungsmuster, z.B. dimiktisch, monomiktisch, polymiktisch
* Einfluss der Schichtung auf die Sauerstoffverfügbarkeit und die Form des Sauerstoffprofils
* Einfluss des Gewässertrophie  auf die Sauerstoffkurve
* Einfluss der photosynthetischen Aktivität auf pH-Wert und Leitfähigkeit (siehe Kalk-Kohlensäure-Gleichgewicht)
* Einfluss der Klimaerwärmnug auf die Dauer der Sommerstratifikation
* Einfluss der Schichtungsdauer auf den Sauerstoffhaushalt im Hypolimnion
* usw.


## Aufgaben und Übungen

* Laden sie sich die Daten von der Kurs-Homepage herunter und
  vergleichen Sie die Temperatur- und Lichtprofile. Suchen Sie im
  Internet nach den Charakteristika der jeweiligen Seen
  bzw. Talsperren und diskutieren Sie den Zusammenhang zwischen
  beobachteten Profilen und den Seeneigenschaften. Gibt es
  Übereinstimmungen oder Widersprüche? Sind diese plausibel oder
  überraschend?
* Vergleichen Sie die vom rLakeAnalyzer abgeschätzten Werte der
  Thermokline mit der 1K/m-Regel
* Versuchen Sie, die Ergebnisse mit LibreOffice, Microsoft Excel oder
  eigenen R-Skripten nachzuvollziehen
* Vergleichen sie die Extinktionskoeffizienten mit Werten aus
  Lehrbüchern, z.B. Lampert and Sommer (2007), Abbildung 3.4

## (Bonus) R-Scripte für die Vertikalprofile

Anbei finden Sie ein R-Script für die Berechnung der 10°C-Isotherme,
der Thermokline und der 1%-Lichttiefe. Ein volles Verständnis von R
oder einer anderen Skriptsprache ist kein Bestandteil dieser
Übung. Andererseits können Kenntnisse in einer Datenanalysesprache wie R
für Ihre zukünftige (akademische oder praktische) Karriere
nützlich sein.  Im Internet finden Sie zahlreiche gute Quellen für das
Selbststudium.




```r
# required libraries
library(readxl)
library(reshape2)
library(ggplot2)
library(rLakeAnalyzer)

# read in the data
DF <- read_excel("data.xlsx")

# reshape the data to "long" format
df_long <- melt(DF, id.vars = "Depth")

# add a column to create the different subplots
df_long <- merge(df_long, data.frame(variable = c("Temp", "Oxygen", "pH", "Cond", "chla", "Turb",
                                                  "Light"),
                                     plot = c("Temp & O2", "Temp & O2", "pH", "Conductivity",
                                              "Chlorophyl-a", "Turbidity", "Light")))

# calculate 10Â°C isotherme
z_iso10 <- approx(DF$Temp, DF$Depth, 10)$y

## calculate thermocline using rLakeAnalyzer
# first remove NAs (Not Available)from the data
DF_valid <- na.omit(DF[c("Depth", "Temp")])
# calculate thermocline depth
z_thermo <- thermo.depth(DF_valid$Temp, DF_valid$Depth)

## calculate 1% light depth
# first fit a liner model to the logarithmic light data
m <- lm(log(DF$Light) ~ DF$Depth)
# then calculate the depth where 1% light is left
z_light <- log(0.01) / m$coefficients[2]

## first plots (Temp, O2, ph, and cond)
# create subset of the data containing only Temp, O2, ph, and cond
dat_p1 <- subset(df_long, df_long$variable %in% c("Temp", "Oxygen", "pH", "Cond"))
# plot the data using ggplot
p1 <- ggplot(dat_p1, aes(x = Depth, y = value, col = variable)) + geom_line() +
  geom_point() + coord_flip()  +
  theme(legend.position="bottom") + xlab("Depth (m)")  +
  scale_x_continuous(trans = "reverse") +
  facet_grid(.~plot, scales = "free")

# add the thermocline depth, 10Â°C isotherme and 1% light depth to the plot

p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 Â°C isotherme"),
                      aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
             aes(xintercept = x, col = variable), linetype = "dashed")


# show the plot
p1

## create chla and Trub plot
# subset of data containing chla and Turb
dat_p2 <-  subset(df_long, df_long$variable %in% c("chla", "Turb"))

# plot the data
p2 <- ggplot(dat_p2, aes(x = Depth, y = value, col = variable)) + geom_line() +
  geom_point() + coord_flip()  +
  theme(legend.position="bottom") + xlab("Depth (m)")  +
  scale_x_continuous(trans = "reverse") +
  facet_grid(.~plot, scales = "free")

# add the thermocline depth, 10Â°C isotherme and 1% light depth to the plot

p2 <- p2 + geom_vline(data = data.frame(x = z_iso10, variable = "10 Â°C isotherme"),
                      aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
             aes(xintercept = x, col = variable), linetype = "dashed")

# show the plot
p2


## create the light plots
# create subset with light data
dat_p3 <- subset(df_long, df_long$variable %in% c("Light"))

# plot the data (allready with the thermocline depth, 10Â°C isotherme and 1% light depth)
p3 <- ggplot(dat_p3, aes(x = Depth, y = value, col = variable)) +
  geom_line() + geom_point() + coord_flip() +
  theme(legend.position="bottom") + xlab("Depth (m)")  +
  scale_x_continuous(trans = "reverse") +
  ggtitle("Light") +
  geom_vline(data = data.frame(x = z_iso10, variable = "10 Â°C isotherme"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
             aes(xintercept = x, col = variable), linetype = "dashed")

# show the plot
p3
## now plot log light and add the linear fit
# get the coefficients from the linear model to show the equation in the plot
eqt <- paste0("y = ", round(coef(m)[1], 2), " ",
              round(coef(m)[2], 2), " * x")
# plot log light (allready with the thermocline depth, 10Â°C isotherme and 1% light depth)
p4 <- ggplot(dat_p3, aes(x = Depth, y = value, col = variable)) +
  geom_point() + coord_flip() + scale_y_log10() +
  theme(legend.position="bottom") + xlab("Depth (m)")  +
  scale_x_continuous(trans = "reverse") +
  geom_smooth(method = "lm", aes(col = "linear fit"))  + ggtitle("log(light) with linear fit") + 
  geom_vline(data = data.frame(x = z_iso10, variable = "10 Â°C isotherme"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_text(data = data.frame(Depth = 1, value = 1, variable = "linear fit"),
            parse = FALSE, label = eqt, color = "black")

# show the plot
p4  
```


## Literaturverzeichis

<!-- Lampert Sommer, Wikipedia, R packages /-->


Lampert, Winfried, and Ulrich Sommer. 2007. Limnoecology: The Ecology
of Lakes and Streams. Oxford university press.

Mortimer, C. H. 1981. The oxygen content of air-saturated fresh waters
over ranges of temperature and atmospheric presure of limnological
interest. Mitteilungen Internationale Vereinigung für theoretische und
angewandte Limnologie, 22:1–23.

Winslow, Luke, Jordan Read, Richard Woolway, Jennifer Brentrup, Taylor
Leach, Jake Zwart, Sam Albers, and Doug Collinge. 2019. RLakeAnalyzer:
Lake Physics Tools. https://CRAN.R-project.org/package=rLakeAnalyzer.

