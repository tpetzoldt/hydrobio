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

# calculate 10°C isotherme
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

# add the thermocline depth, 10°C isotherme and 1% light depth to the plot

p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
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

# add the thermocline depth, 10°C isotherme and 1% light depth to the plot

p2 <- p2 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
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

# plot the data (allready with the thermocline depth, 10°C isotherme and 1% light depth)
p3 <- ggplot(dat_p3, aes(x = Depth, y = value, col = variable)) +
  geom_line() + geom_point() + coord_flip() +
  theme(legend.position="bottom") + xlab("Depth (m)")  +
  scale_x_continuous(trans = "reverse") +
  ggtitle("Light") +
  geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
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
# plot log light (allready with the thermocline depth, 10°C isotherme and 1% light depth)
p4 <- ggplot(dat_p3, aes(x = Depth, y = value, col = variable)) +
  geom_point() + coord_flip() + scale_y_log10() +
  theme(legend.position="bottom") + xlab("Depth (m)")  +
  scale_x_continuous(trans = "reverse") +
  geom_smooth(method = "lm", aes(col = "linear fit"))  + ggtitle("log(light) with linear fit") +
  geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
             aes(xintercept = x, col = variable), linetype = "dashed") +
  geom_text(data = data.frame(Depth = 1, value = 1, variable = "linear fit"),
            parse = FALSE, label = eqt, color = "black")

# show the plot
p4
