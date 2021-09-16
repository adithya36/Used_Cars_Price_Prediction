rm(list = ls())
library(rio)
library(ggplot2)
library(tidyverse)
library(stats)
library(dplyr)
library(DataExplorer)
Cars <- import("Group6_Cars.csv")
Cars_final <- select(Cars, -c("Index", "V1"))

# Dataframe with factor columns instead of characters
Cars_final_fac <- Cars_final
Cars_final_fac$Manufacturer_name <- as.factor(Cars_final$Manufacturer_name) 
Cars_final_fac$Gear <- as.factor(Cars_final$Gear) 
Cars_final_fac$Fuel <- as.factor(Cars_final$Fuel) 
Cars_final_fac$Country <- as.factor(Cars_final$Country) 

head(Cars_final_fac, 5)
str(Cars_final_fac)
# => 407.552 columns & 7 relevant variables 
## character variables (bzw. factor): Manufacturer_name; Gear; Fuel; Country
## numeric: Odometer (km); Price ($)
## integer: year
summary(Cars_final)
summary(Cars_final_fac)
class(Cars_final)


### Code for creating an function for "Mode" ----
Mode = function(x,na.rm=FALSE) {
  if(na.rm){
    x = na.omit(x)
  }
  ux = unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}



## Character/Factor:----

# Manufacturer_name
## As character less info: 
### summary(Cars_final$Manufacturer_name)
### unique(Cars_final$Manufacturer_name)
### table(Cars_final$Manufacturer_name)
### Mode(Cars_final$Manufacturer_name)

## As factor
summary(Cars_final_fac$Manufacturer_name)
unique(Cars_final_fac$Manufacturer_name)
table(Cars_final_fac$Manufacturer_name)
Mode(Cars_final_fac$Manufacturer_name)

ggplot(Cars_final_fac, aes(Manufacturer_name))+
  geom_bar()+
  theme_bw()+
  scale_y_continuous(labels = scales::comma)



# Gear
## As factor
summary(Cars_final_fac$Gear)
unique(Cars_final_fac$Gear)
table(Cars_final_fac$Gear)
Mode(Cars_final_fac$Gear)

ggplot(Cars_final_fac, aes(Gear))+
  geom_bar()+
  theme_bw()+
  scale_y_continuous(labels = scales::comma)

# Fuel
## As factor:
summary(Cars_final_fac$Fuel)
unique(Cars_final_fac$Fuel)
table(Cars_final_fac$Fuel)
Mode(Cars_final_fac$Fuel)

ggplot(Cars_final_fac, aes(Fuel))+
  geom_bar()+
  theme_bw()+
  scale_y_continuous(labels = scales::comma)

# Country
## As factor
summary(Cars_final_fac$Country)
unique(Cars_final_fac$Country)
table(Cars_final_fac$Country)
Mode(Cars_final_fac$Country)

ggplot(Cars_final_fac, aes(Country))+
  geom_bar(aes(color=..x.., fill=..x..))+
  theme_bw()+
  labs(title="Origin country of data", y="Count")+
  coord_cartesian(ylim = c(0, 350000))+
  scale_y_continuous(labels = scales::comma)
  
  


ggplot(Cars_final_fac, aes(Country))+
  geom_density(aes(color=Country))+
  theme_bw()
ggplot(Cars_final_fac, aes(Country))+
  geom_boxplot()+
  theme_bw()

### Numeric: ----
# Odometer (km)
summary(Cars_final$`Odometer (km)`)
mean(Cars_final$`Odometer (km)`)  #No NAs
median(Cars_final$`Odometer (km)`)
quantile(Cars_final$`Odometer (km)`)

Mode(Cars_final$`Odometer (km)`)

var(Cars_final$`Odometer (km)`)
sd(Cars_final$`Odometer (km)`)
cor(Cars_final$`Odometer (km)`,Cars_final$`Price ($)`, method="pearson")


ggplot(Cars_final, aes(`Odometer (km)`))+
  geom_boxplot()+
  theme_bw()+
  coord_cartesian(xlim = c(0, 500000))+
  labs(title="Boxplot: Odometer")+
  scale_x_continuous(labels = scales::comma)+
  theme(
    legend.position = "right",
    plot.title = element_text(
      face = "bold", colour = "black"
    ))

ggplot(Cars_final, aes(`Odometer (km)`))+
  geom_histogram(binwidth=50000, , color="black")+
  theme_bw()+
  coord_cartesian(xlim = c(0, 500000))+
  labs(title="Histogram: Odometer")+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  theme(
    legend.position = "right",
    plot.title = element_text(
      face = "bold", colour = "black"
    ))

ggplot(Cars_final, aes(`Odometer (km)`))+
  geom_density(bw=10000)+
  theme_bw()+
  coord_cartesian(xlim = c(0, 500000))+
  labs(title="Density: Odometer")+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  theme(
    legend.position = "right",
    plot.title = element_text(
      face = "bold", colour = "black"
    ))

#Streudiagramm mit Preis
my_colors <- c("black", "black", "black", "black")
myplot <- ggplot(Cars_final_fac, aes(`Odometer (km)`, `Price ($)`))+
  geom_point(alpha=0.1, size=0.05,aes(color=Country))+
  geom_smooth(method="lm")+
  geom_hline(yintercept=17985, linetype='dashed', color='red', size=0.5)+
  scale_color_manual(values = my_colors)+
  theme_bw()+
  coord_cartesian(xlim = c(0, 500000), ylim=c(0, 150000))+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(title="Scatterplot: Odometer ~ Price")+
  theme(
    legend.position = "right",
    plot.title = element_text(
      face = "bold", colour = "black"
    ))
myplot
myplot %+% 
  subset(Cars_final_fac, Country %in% c("Belarus"))
myplot %+% 
  subset(Cars_final_fac, Country %in% c("Germany"))
myplot %+% 
  subset(Cars_final_fac, Country %in% c("India"))
myplot %+% 
  subset(Cars_final_fac, Country %in% c("US"))
summary(Cars_final_fac$Country)

#LR
SLR_odometer <- lm( `Price ($)` ~ `Odometer (km)`, Cars_final_fac)
summary(SLR_odometer)

#LR Country-wise
Cars_Belarus <- filter(Cars_final_fac, Country=="Belarus")
str(Cars_Belarus)
SLR_odometer_Belarus <- lm( `Price ($)` ~ `Odometer (km)`, Cars_Belarus)
summary(SLR_odometer_Belarus)
Cars_Germany <- filter(Cars_final_fac, Country=="Germany")
SLR_odometer_Germany <- lm( `Price ($)` ~ `Odometer (km)`, Cars_Germany)
summary(SLR_odometer_Germany)
Cars_India <- filter(Cars_final_fac, Country=="India")
SLR_odometer_India <- lm( `Price ($)` ~ `Odometer (km)`, Cars_India)
summary(SLR_odometer_India)
Cars_US <- filter(Cars_final_fac, Country=="US")
SLR_odometer_US <- lm( `Price ($)` ~ `Odometer (km)`, Cars_US)
summary(SLR_odometer_US)

# Local Regression - Germany
myplot_loess_ger <- ggplot(Cars_Germany, aes(`Odometer (km)`, `Price ($)`))+
  geom_point(alpha=0.1, size=0.05,aes(color=Country))+
  geom_smooth(method="loess", se=FALSE)+
  geom_hline(yintercept=17985, linetype='dashed', color='red', size=0.5)+
  scale_color_manual(values = my_colors)+
  theme_bw()+
  coord_cartesian(xlim = c(0, 500000), ylim=c(0, 150000))+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(title="Scatterplot: Odometer ~ Price")+
  theme(
    legend.position = "right",
    plot.title = element_text(
      face = "bold", colour = "black"
    ))
myplot_loess_ger

# Price ($)
summary(Cars_final$`Price ($)`)
mean(Cars_final$`Price ($)`)  #No NAs, therefore na.rm not necessary
median(Cars_final$`Price ($)`)
quantile(Cars_final$`Price ($)`)
Mode(Cars_final$`Price ($)`)  # Look at the Density Plot

var(Cars_final$`Price ($)`)
sd(Cars_final$`Price ($)`)

ggplot(Cars_final, aes(`Price ($)`))+
  geomy_density()+
  theme_bw()+
  labs(title="Density: Price", y="")+
  coord_cartesian(xlim = c(0, 70000))+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  theme(
    legend.position = "right",
    plot.title = element_text(
      face = "bold", colour = "black"
    ))



ggplot(Cars_final, aes(`Price ($)`))+
  geom_bar()+
  theme_bw()
ggplot(Cars_final, aes(`Price ($)`))+
  geom_boxplot()+
  theme_bw()
plot(density(Cars_final$`Price ($)`))

### Integer: ----
# year
summary(Cars_final$year)
unique(Cars_final$year)
Mode(Cars_final$year)
quantile(Cars_final$year)

var(Cars_final$year)
sd(Cars_final$year)

ggplot(Cars_final, aes(year))+
  geom_bar()+
  theme_bw()
ggplot(Cars_final, aes(year))+
  geom_boxplot()+
  theme_bw()

# Data Explorer ----
introduce(Cars_final)
plot_intro(Cars_final)
plot_bar(Cars_final)
plot_histogram(data = Cars_final)
plot_density(data = Cars_final)
plot_correlation(Cars_final)
plot_boxplot(Cars_final, by="Gear")

glimpse(Cars_final)

# ----



# ToDos ----
# Descriptive statistics
# Explorative 
# Numeric Spalten: min(),max(),mean()
# Korrelationen zwischen allen features/assets/Variablen berechnen
# Variablenverteilung plotten


#----
### Korrelationsmatrix => Nur numerische Variablen ----
# Numerische Variablen extrahieren
odometer_value_num <- as.numeric(Cars$odometer_value)
year_produced_num <- as.numeric(Cars$year_produced)
engine_capacity_num <- as.numeric(Cars$engine_capacity)
price_usd_num <- as.numeric(Cars$price_usd)
number_of_photos_num <- as.numeric(Cars$number_of_photos)
up_counter_num <- as.numeric(Cars$up_counter)
duration_listed_num <- as.numeric(Cars$duration_listed)


Cars_num <- data.frame(Odometer_value=odometer_value_num, year_produced=year_produced_num,
                       engine_capacity=engine_capacity_num,price_usd=price_usd_num,
                       number_of_photos=number_of_photos_num,up_counter=up_counter_num,
                       duration_listed=duration_listed_num)

#F Cars_num <- select(Cars, odometer_value,year_produced,engine_capacity,price_usd,number_of_photos,up_counter,duration_listed)
#F sapply(Cars_num, as.numeric)
str(Cars_num)
head(Cars_num)
Cars_cor <- cor(Cars_num, method="pearson")
Cars_cor #Korrelationswerte
library('corrplot') #package corrplot
corrplot(Cars_cor, method = "number") #plot matrix



#Variablen
  #### Marke - manufacturer name ----
              manufacturer_name_factor<-factor(Cars$manufacturer_name)
              levels(manufacturer_name_factor)
unique(Cars$manufacturer_name) #
data.frame(table(Cars$manufacturer_name))  #Häufigkeit der characteristics

#--> Paar unbekannte Marken: 
  # Anzahl alter russ. Marken:
Russ_manufacturer <- sum(
  # "ВАЗ" = Lada Modell
  length(which(Cars=="ВАЗ")),
  # "ГАЗ" = Gorkowski Awtomobilny Sawod
  length(which(Cars=="ГАЗ")),
  # "ЗАЗ" = Saporisky Awtomobilebudiwny Sawod
  length(which(Cars=="ЗАЗ")),
  # "Москвич" = Moskwitsch
  length(which(Cars=="Москвич")),
  # "УАЗ" = Uljanowski Awtomobilny Sawod
  length(which(Cars=="УАЗ"))
  # => Anteil kalkulieren, wenn gering dann löschen 
)
Russ_manufacturer/nrow(Cars) # 2% Perecntage of the Dataset are effected => Delete

ggplot(Cars, aes(price_usd,manufacturer_name))+
  geom_boxplot()

ggplot(Cars, aes(manufacturer_name))+
  geom_bar()

ggplot(Cars, aes(price_usd,manufacturer_name))+
  geom_point(size=0.3,alpha=0.05, color="red")

LR_manufacturer_name <- lm(price_usd ~ manufacturer_name, Cars)
summary(LR_manufacturer_name)

#### PREIS in USD!! - price usd ----
price_usd_factor<-factor(Cars$price_usd)
levels(price_usd_factor)
unique(Cars$price_usd)
data.frame(table(Cars$price_usd))

min(Cars$price_usd)
max(Cars$price_usd)
mean(Cars$price_usd,na.rm=TRUE)
median(Cars$price_usd)

ggplot(Cars, aes(price_usd))+
  geom_histogram(binwidth = 1000) #Je Balken 1000

  #### Modellname - model name (Too huge) ----
              model_name_factor<-factor(Cars$model_name)
              levels(model_name_factor)
unique(Cars$model_name)
data.frame(table(Cars$model_name))

# Plots haben geringe Aussagekraft
ggplot(Cars, aes(model_name,price_usd))+
  geom_boxplot()
ggplot(Cars, aes(model_name,price_usd))+
  geom_point()
#Nicht plotten: 1118 Regressionskoeffizienten
LR_model_name <- lm(price_usd ~ model_name, Cars)
summary(LR_model_name)

  #### Getriebeart - transmission (Mechanisch, Automatisch) ----
          transmission_factor<-factor(Cars$transmission)
          levels(transmission_factor)
unique(Cars$transmission)
data.frame(table(Cars$transmission))

ggplot(Cars, aes(transmission,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(transmission))+
  geom_bar()

ggplot(Cars, aes(transmission,price_usd))+
  geom_point(alpha=0.01, color="blue")+
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color="red") #Regressionsgerade
  
LR_transmission <- lm(price_usd ~ transmission, Cars)
summary(LR_transmission)
# Plot funkt nicht
#plot(Cars$transmission, Cars$price_usd)
#abline(LR_transmission,col="red")

  #### Farbe - Color ----
          color_factor<-factor(Cars$color)
          levels(color_factor)
unique(Cars$color)
data.frame(table(Cars$color))

ggplot(Cars, aes(color))+
  geom_bar()
ggplot(Cars, aes(color,price_usd))+
  geom_boxplot()
ggplot(Cars, aes(color,price_usd))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color="red") #Regressionsgerade

LR_color <- lm(price_usd ~ color, Cars)
summary(LR_color)
abline(LR_color,col="red")

  #### Kilometerstand - Odometer value ----
unique(Cars$odometer_value)
data.frame(table(Cars$odometer_value))
min(Cars$odometer_value)
max(Cars$odometer_value)
mean(Cars$odometer_value)

ggplot(Cars, aes(odometer_value,price_usd))+
  geom_point(alpha=0.3,size=0.1)+
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color="red") #Regressionsgerade


LR_odometer_value <- lm(price_usd ~ odometer_value, Cars)
summary(LR_odometer_value)
plot(Cars$odometer_value,Cars$price_usd)
abline(LR_odometer_value,col="red")


  #### Produktionsjahr - year produced ----
            year_produced_factor<-factor(Cars$year_produced)
            levels(year_produced_factor)
unique(Cars$year_produced)
data.frame(table(Cars$year_produced))

ggplot(Cars, aes(year_produced,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(year_produced))+
  geom_bar()

ggplot(Cars, aes(year_produced,price_usd))+
  geom_point(alpha=0.1, size=0.5)+
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color="red") #Regressionsgerade

LR_year_produced <- lm(price_usd ~ year_produced, Cars)
summary(LR_year_produced)



  #### Motorkraftstoff - engine fuel ----
                engine_fuel_factor<-factor(Cars$engine_fuel)
                levels(engine_fuel_factor)
unique(Cars$engine_fuel)
data.frame(table(Cars$engine_fuel))

ggplot(Cars, aes(engine_fuel,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(engine_fuel))+
  geom_bar()

ggplot(Cars, aes(engine_fuel,price_usd))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color="red") #Regressionsgerade
    
LR_engine_fuel <- lm(price_usd ~ engine_fuel, Cars)
summary(LR_engine_fuel)
abline(LR_engine_fuel,col="red")


  #### Dummy: Gas - engine has gas ----
unique(Cars$engine_has_gas)
data.frame(table(Cars$engine_has_gas))

ggplot(Cars, aes(engine_has_gas))+
  geom_bar()

ggplot(Cars, aes(enginge_has_gas,price_usd))+
  geom_boxplot()


  #### Motortyp - engine type ----
                engine_type_factor<-factor(Cars$engine_type)
                levels(engine_type_factor)
unique(Cars$engine_type)
data.frame(table(Cars$engine_type))

ggplot(Cars, aes(engine_type,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(engine_type))+
  geom_bar()

ggplot(Cars, aes(engine_type,price_usd))+
  geom_point(alpha=0.1)

LR_engine_type <- lm(price_usd ~ engine_type, Cars)
summary(LR_engine_type)
abline(LR_engine_type,col="red")


  #### Hubraum - engine capacity ----
# = "fuel each piston can push when they move" - Je größer, desto mehr Leistung

                engine_capacity_factor<-factor(Cars$engine_capacity)
                levels(engine_capacity_factor)
unique(Cars$engine_capacity)
data.frame(table(Cars$engine_capacity))

ggplot(Cars, aes(engine_capacity,price_usd))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color="red") #Regressionsgerade

ggplot(Cars, aes(engine_capacity))+
  geom_bar()

ggplot(Cars, aes(engine_capacity,price_usd))+
  geom_boxplot(aes(group=engine_capacity))

LR_engine_capacity <- lm(price_usd ~ engine_capacity, Cars)
summary(LR_engine_capacity)
abline(LR_engine_capacity,col="red")


  #### Karosserietyp - body type ----
                  body_type_factor<-factor(Cars$body_type)
                  levels(body_type_factor)
unique(Cars$body_type)
data.frame(table(Cars$body_type))

ggplot(Cars, aes(body_type,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(body_type))+
  geom_bar()

ggplot(Cars, aes(body_type, price_usd))+
  geom_point(alpha=0.1, transparency=0.1)

LR_body_type <- lm(price_usd ~ body_type, Cars)
summary(LR_body_type)
abline(LR_body_type,col="red")


  #### Dummy: Garantie - has_warranty----
unique(Cars$has_warranty)
data.frame(table(Cars$has_warranty))

ggplot(Cars, aes(has_warranty,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(has_warranty))+
  geom_bar()

LR_has_warranty <- lm(price_usd ~ has_warranty, Cars)
summary(LR_has_warranty)
abline(LR_has_warranty,col="red")

  #### Zustand - State ----
unique(Cars$state)
data.frame(table(Cars$state))

ggplot(Cars, aes(state,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(state))+
  geom_bar()

  #### Antriebsart - drivetrain ----
unique(Cars$drivetrain)
data.frame(table(Cars$drivetrain))

ggplot(Cars, aes(drivetrain,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(drivetrain))+
  geom_bar()

ggplot(Cars, aes(drivetrain,price_usd))+
  geom_point(alpha=0.01)


  #### Umtausch (Rückgabegarantie) - is exchangeable ----
unique(Cars$is_exchangeable)
data.frame(table(Cars$is_exchangeable))

ggplot(Cars, aes(is_exchangeable,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(is_exchangeable))+
  geom_bar()

ggplot(Cars, aes(is_exchangeable,price_usd))+
  geom_point(alpha=0.01)

  #### Standort (Only RUSSIA) - location region ----
                  location_region_factor <- as.factor(Cars$location_region)
                  levels(location_region_factor)
unique(Cars$location_region)
data.frame(table(Cars$location_region))
#????? -> Translate!!!
  #Брестская обл.     = Region Brest
  #Витебская обл.     = Region Witebsk   
  #Гомельская обл.    = Region Gomel
  #Гродненская обл.   = Region Grodno
  #Минская обл.       = Gebiet Minsk
  #Могилевская обл.   = Region Mogilew
# => Necessary?

ggplot(Cars, aes(location_region,price_usd))+
  geom_boxplot()

ggplot(Cars, aes(location_region))+
  geom_bar()


  #### Anzahl Fotos - number of photos ----
unique(Cars$number_of_photos)
data.frame(table(Cars$number_of_photos))

ggplot(Cars, aes(number_of_photos,price_usd))+
  geom_point(alpha=0.01)

ggplot(Cars, aes(number_of_photos))+
  geom_bar()

ggplot(Cars, aes(number_of_photos,price_usd))+
  geom_boxplot(aes(group=number_of_photos))

LR_number_of_photos <- lm(price_usd ~ number_of_photos, Cars)
summary(LR_number_of_photos)
abline(LR_number_of_photos,col="red")


  #### Drehzahlmesser im Tacho - up counter ----
unique(Cars$up_counter)
data.frame(table(Cars$up_counter))

ggplot(Cars, aes(up_counter,price_usd))+
  geom_point(alpha=0.01)
max(Cars$up_counter)
mean(Cars$up_counter)
# => Does not make sense

ggplot(Cars, aes(up_counter))+
  geom_bar()

ggplot(Cars, aes(up_counter,price_usd))+
         geom_boxplot(aes(group=up_counter))
  
LR_up_counter <- lm(price_usd ~ up_counter, Cars)
summary(LR_up_counter)
abline(LR_up_counters,col="red")


  #### Anzeigenausspieldauer - duration listed ----
unique(Cars$duration_listed)
data.frame(table(Cars$duration_listed))
min(Cars$duration_listed)
max(Cars$duration_listed)
mean(Cars$duration_listed)
median(Cars$duration_listed)

ggplot(Cars, aes(duration_listed,price_usd))+
  geom_point(alpha=0.2,size=0.5)

ggplot(Cars, aes(duration_listed))+
  geom_bar()

Cars$duration_listed
LR_duration_listed <- lm(price_usd ~ duration_listed, Cars)
summary(LR_duration_listed)
abline(LR_duration_listed,col="red")



#----
  









# Multiple Lineare Regression 1: Price ~ odometer value + year produced ----
MLR1 <- lm(price_usd ~ odometer_value+year_produced, Cars)
summary(MLR1)
    # Multikollinearität


         # Funktioniert nicht: 3D Plot (erst MV Regression berechnen) ----
odometer_value <- Cars$odometer_value
year_produced <- Cars$year_produced

grid <- expand.grid(odometer_value, year_produced)
d <- setNames(data.frame(grid), c("odometer_value", "year_produced"))
vals <- predict(MLR1, newdata = d)

price_usd_MLR <- matrix(vals, nrow = length(d$odometer_value), ncol = length(d$year_produced))
plane <- price_usd_MLR

rm(d, grid, vals)

library(plotly)
p <- plot_ly(data = Cars, z = ~price_usd_MLR, 
             x = ~year_produced, y = ~odometer_value, opacity = 0.6) %>% add_markers()

p %>% add_surface(z = ~plane, x = ~year_produced, y = ~odometer_value, showscale = FALSE) %>% layout(showlegend = FALSE)


# Multiple Lineare Regression 2: Price ~  odometer value + engine type ----
MLR2 <- lm(price_usd ~ odometer_value+engine_type, Cars)
summary(MLR2)


# Multiple Lineare Regression 3: Price ~  year produced + engine type ----
MLR3 <- lm(price_usd ~ year_produced+engine_type, Cars)
summary(MLR3)


# Multiple Lineare Regression 4: Price ~  odometer value + engine type + year produced ----
MLR4 <- lm(price_usd ~ odometer_value+engine_type+year_produced, Cars)
summary(MLR4)


#----





# Appendix ----



  # Ggf Korrelationen plotten 
  # Ggf zusätzlich lineare Regressionen kalkulieren & plotten
  # Datensatz mit allen Korrelationen erzeugen (besser mit einer Matrix?)



#Pearson Korrelationen für numeric Variablen (auch für Dummies/logical --> Mit ifelse umcodieren)
#Spearman & Kendall Korrelation für ordinal/categorical
  #Probiere Korrelation erneut mit Spearman für character


# Kilometerstand 
odometer_value_num <- as.numeric(Cars$odometer_value)
odometer_value_num_pearson <- cor(odometer_value_num, Cars$price_usd, method = c("pearson"))
odometer_value_num_spearman <- cor(odometer_value_num, Cars$price_usd, method = c("spearman"))
odometer_value_num_kendall <- cor(odometer_value_num, Cars$price_usd, method = c("kendall"))

#Produktionsjahr
year_produced_num <- as.numeric(Cars$year_produced)
year_produced_num_pearson <- cor(year_produced_num, Cars$price_usd, method = c("pearson"))
year_produced_num_spearman <- cor(year_produced_num, Cars$price_usd, method = c("spearman"))
year_produced_num_kendall <- cor(year_produced_num, Cars$price_usd, method = c("kendall"))
#Code zum Plotten der Korrelationen ergänzen
Reg_year_produced <- lm(price_usd ~ year_produced, Cars)
summary(Reg_year_produced)
plot(Cars$year_produced,Cars$price_usd, type = "p")
abline(Reg_year_produced,col="red",lwd=2)
library(ggpubr)


#Motorkapazität
engine_capacity_pearson <- cor(Cars$engine_capacity, Cars$price_usd, method = c("pearson"))
engine_capacity_spearman <-cor(Cars$engine_capacity, Cars$price_usd, method = c("spearman"))
engine_capacity_kendall <-cor(Cars$engine_capacity, Cars$price_usd, method = c("kendall"))

#Anzahl Fotos 
number_of_photos_num <- as.numeric(Cars$number_of_photos)
number_of_photos_num_pearson <- cor(number_of_photos_num, Cars$price_usd, method = c("pearson"))
number_of_photos_num_spearman <- cor(number_of_photos_num, Cars$price_usd, method = c("spearman"))
number_of_photos_num_kendall <- cor(number_of_photos_num, Cars$price_usd, method = c("kendall"))

# Tacho Drehmoment
str(up_counter_num)
up_counter_num <- as.numeric(Cars$up_counter)
up_counter_num_pearson <- cor(up_counter_num, Cars$price_usd, method = c("pearson"))
up_counter_num_spearman <- cor(up_counter_num, Cars$price_usd, method = c("spearman"))
up_counter_num_kendall <- cor(up_counter_num, Cars$price_usd, method = c("kendall"))

# Anzeigendauer 
duration_listed_num <- as.numeric(Cars$duration_listed)
duration_listed_num_pearson <- cor(duration_listed_num, Cars$price_usd, method = c("pearson"))
duration_listed_num_spearman <- cor(duration_listed_num, Cars$price_usd, method = c("spearman"))
duration_listed_num_kendall <- cor(duration_listed_num, Cars$price_usd, method = c("kendall"))


#HIER FUNKTIONIERT ES
engine_fuel_factor <- as.factor(Cars$engine_fuel)
engine_fuel_factor
which(is.na(Cars$engine_fuel))
engine_fuel<- Cars$engine_fuel
which(is.na(engine_fuel_factor))
which(is.na(engine_fuel))
which(engine_fuel_factor=="electric")

levels(engine_fuel_factor)
diesel_index <- which(engine_fuel_factor=="diesel")
gasoline_index <- which(engine_fuel_factor=="gasoline")
electric_index <- which(engine_fuel_factor=="electric")
hybrid_petrol_index <- which(engine_fuel_factor=="hybrid-petrol")
hybrid_diesel_index <- which(engine_fuel_factor=="hybrid-diesel")
gas_index <- which(engine_fuel_factor=="gas")
#…
engine_fuel_dummy_2 <- numeric(length(Cars$engine_fuel))
engine_fuel_dummy_2[diesel_index] <- 1
engine_fuel_dummy_2[gasoline_index] <- 2
engine_fuel_dummy_2[electric_index] <- 3
engine_fuel_dummy_2[hybrid_petrol_index] <- 4
engine_fuel_dummy_2[hybrid_diesel_index] <- 5
engine_fuel_dummy_2[gas_index] <- 6
engine_fuel_dummy_2

engine_fuel_pearson <- cor(engine_fuel_dummy_2, Cars$price_usd, method = c("pearson"))
engine_fuel_spearman <- cor(engine_fuel_dummy_2, Cars$price_usd, method = c("spearman"))
engine_fuel_kendall <- cor(engine_fuel_dummy_2, Cars$price_usd, method = c("kendall"))

#Histogramm je Engine Fuel statt Korrelation
gasoline_index<-which(Cars$engine_fuel=="gasoline")
gasoline<-engine_fuel[gasoline_index]
ggplot(Cars, aes(engine_fuel,price_usd))+
  geom_boxplot(aes(group=engine_fuel))
ggplot(Cars, aes(engine_fuel,price_usd))+
  geom_boxplot(aes(group=engine_fuel))
hist(gasoline,Cars$price_usd)


# DIesen Datensatz fertig schreiben
Cars_dataframe_correlation <- data.frame(Correlation=c("Pearson","Spearman","Kendall"),
                             odometer_value=c(odometer_value_num_pearson, odometer_value_num_spearman, odometer_value_num_kendall),
                             year_produced=c(year_produced_num_pearson, year_produced_num_spearman, year_produced_num_kendall),
                             engine_capacity=c(engine_capacity_pearson, engine_capacity_spearman, engine_capacity_kendall),
                             number_of_photos_num=c(number_of_photos_num_pearson,number_of_photos_num_spearman,number_of_photos_num_kendall),
                             up_counter_num=c(up_counter_num_pearson, up_counter_num_spearman, up_counter_num_kendall),
                             duration_listed_num=c(duration_listed_num_pearson, duration_listed_num_spearman, duration_listed_num_kendall),
                             )
head(Cars_dataframe_correlation)


engine_fuel_factor <- factor(Cars$engine_fuel)
levels(engine_fuel_factor)
count(engine_fuel_factor)

engine_fuel_dummy <- numeric(length(Cars$engine_fuel))
if(engine_fuel_factor=="diesel") {
  engine_fuel_dummy <- 1
} else if(engine_fuel_factor=="gasoline"){
  engine_fuel_dummy <- 2
}




#…
engine_fuel_dummy_2

cor(engine_fuel_dummy_2, Cars$price_usd, method = c("pearson"))





Reg_manufacturer <- lm(price_usd ~ manufacturer_name, Cars)
summary(Reg_manufacturer)
plot(Cars$manufacturer_name,Cars$price_usd, type = "p")
abline(Reg_manufacturer)

Reg_year_produced <- lm(price_usd ~ year_produced, Cars)
summary(Reg_year_produced)
plot(Cars$year_produced,Cars$price_usd, type = "p")
abline(Reg_year_produced,col="red",lwd=2)
