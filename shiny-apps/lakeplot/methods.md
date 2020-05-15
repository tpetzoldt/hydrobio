---
title: "Background and Tasks"
output: 
  html_document:
    keep_md: true
    code_folding: hide
bibliography: references.bib    
---



## Scientific background

### Thermocline and 10 degrees Celsius isotherme

The thermocline in the plots is calculated using the **R** package
[rLakeAnalyzer](https://cran.r-project.org/web/packages/rLakeAnalyzer/). Different
definitions and calculation methods exist for mixing depth ($z_{mix}$)
and thermocline, see for example the references in the rLakeAnalyzer
[documentation](https://cran.r-project.org/web/packages/rLakeAnalyzer/vignettes/sm_algorithm.html)
(Winslow et al. 2019)
As a simple rule of thumb, mixing depth can be estimated as the first
depth where the temperature gradient is more than one degree per meter
(1K/m).
The app uses the rLakeAnalyzer algorithm, to understand what
it does is left as an exercise (see below).

Both mixing depth and thermocline are somewhat complex. In reality,
hydrophysical gradients are continuous and change dynamically. "Fixed
boundaries" do not really exist, but they are good indicators and
helpful for further computations.  Another practical indicator is the
"10 degrees Celsius isotherme", that is even simpler and less
influenced by complex interactions.

### Oxygen saturation

Oxygen saturation is given by the ratio of the actual oxygen
concentration and the oxygen saturation concentration at the given
temperature. The saturation concentration can be approximated as a
function of water temperature and barometric pressure using the
equation of Mortimer (1981):

\[
C_{O2, sat} = \exp\left( 7.7117 − 1.31403 \cdot \log\left(T + 45.93\right)\right) \cdot
\frac{p}{1013.25}
\]

where $T$ is the temperature in degrees Celsius, and $p$ is the air
pressure in hectopascal. The resulting saturation concentration is in
units of gO/m$^3$ or mgO/l.

### Underwater light profile and euphotic zone

The underwater light $I_z$ ($I=$ irradiation) in a particular depth
$z$ can be estimated from the light intensity immediately below the
water surface $I_0$ using Lambert-Beer's law:

\[
I_z = I_0 \cdot e^{-\varepsilon \cdot z}
\]

where it is assumed that the light extinction coefficient
$\varepsilon$ (in some books also named $k_d$) is constant over
depth. This is of course an approximation for mainly two reasons:

* light extinction depends on the light wavelength
* color and particles are evenly distributed over depth

The $\varepsilon$-value measured with an underwater light sensor is a
mean value over depth and over a certain spectral range (the visible
light or the photosynthetic active part), so it can be called the
"mean vertical and mean spectral extinction coefficient".

It can be directly calculated from Lambert-Beer's law by linear
regresion of the log-transformed equation

\[
\ln(I_z) = \ln(I_0)  -\varepsilon \cdot z
\]

that is equivalent to a linear regression, where the coefficient $b =
\varepsilon$:

\[
y = a  - b \cdot x
\]

### Limnological interactions between light, temperature, oxygen pH and conductivity

In aquatic ecosystems, hydrophysical, chemical and biological
variables are influenced by hydrology, meteorology and seasonal
forcing and influence each other. As a comprehensive description would
exceed the space here, we refer to the hydrobiology lecture and the
textbooks.

You may consider the following keywords and questions:

* seasonality and stratification patterns, e.g. dimictic, monomictic, polymictic
* influence of stratification on oxygen availability, oxygen consumption and production
* influence of trophic state on the shape of oxygen profile
* influence of phytosynthetic activity on pH and conductivity (refers to the calcium-carbonate balance)
* influence of climate warming on stratification duration
* influence of stratification duration on oxygen in the hypolimnion
* and more ...


## Exercises

* Download data from the course home page and compare temperature and
  light profiles. Search the internet for background information about
  the particular Lakes and Reservoirs and discuss how the profile
  characteristics are related to the lakes. Can you find agreements
  and disagreements? Are they plausible or surprising?
* Compare the rLakeAnalyzer thermocline with the 1K/m rule.
* Try to reproduce the results with Libreoffice, Excel or an own R
  script.
* Compare the extinction coefficients with data from a limnology
  textbook e.g., Lampert and Sommer (2007), Figure 3.4

## (Bonus) Recreate the plots in R

Below, the source code to calculate the 10°C isotherme, thermocline
depth and 1% light depth, and create the plots shown in this
application is provided. It is not required to understand this or to
be able to recreate the plots in R. Nevertheless, some skills using
the statistical programming language R can be advantageous in your
future (academic) career and there are lots of good resources to learn
it online.



```r
# required libraries
library(readxl)
library(reshape2)
library(ggplot2)
library(rLakeAnalyzer)

# an example data set can be downloaded from:
# https://github.com/tpetzoldt/hydrobio/blob/master/data/lake_profile.xlsx

# read in the data
DF <- read_excel("lake_profile.xlsx")

# reshape the data to "long" format
df_long <- melt(DF, id.vars = "Depth")

# add a column to create the different subplots
df_long <- merge(df_long,
    data.frame(variable = c("Temp", "Oxygen", "pH", "Cond", "chla", "Turb", "Light"),
               plot = c("Temp & O2", "Temp & O2", "pH", "Conductivity",
                        "Chlorophyl-a", "Turbidity", "Light")))

# first remove NAs (Not Available)from the data
DF_valid <- na.omit(DF[c("Depth", "Temp")])

# calculate 10Â°C isotherme
z_iso10 <- approx(DF_valid$Temp, DF_valid$Depth, 10, ties=mean)$y

## calculate thermocline using rLakeAnalyzer
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

## References

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

