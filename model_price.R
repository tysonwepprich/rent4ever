# Will I ever buy a house in Corvallis?

# Setup ----
library(tidyverse)
library(xlsx)
library(magrittr)
library(lubridate)
library(mgcv)
# library(realtR) # 
library(randomForest)
library(forestFloor)
library(ggmap)

# Import Data ----
# Annual data comes from https://www.co.benton.or.us/assessment/page/property-sales-data
# Used 2011-2018 (2001-2020 available)
fs <- list.files("data", full.names = TRUE)

# df <- xlsx::read.xlsx(fs[1], sheetIndex = 1, startRow = 2, header = TRUE)

# combine spreadsheets (takes a whike to run)
df <- fs %>% 
  map(.x = ., ~xlsx::read.xlsx(.x, sheetIndex = 1, startRow = 2, header = TRUE))
df2 <- df %>% 
  map(.x = ., ~mutate_all(.x, as.character)) %>% 
  bind_rows()

# Clean Data ----
# Consideration is the sold price I think, lots of weird zeros
# I think 0 for consideration could be refinances
hist(log(as.numeric(df2$Consideration) + 1))

# convert some columns back to numeric
names(df2)
cols <- names(df2)[c(4, 5, 19, 23, 24, 28:39)]
# df2[cols] <- lapply(df2[, cols], numeric)
df2 %<>% mutate_at(cols, funs(as.numeric(.)))

# excel date issue, only in 2018?

df2 <- df2 %>% 
  mutate(Date = lubridate::ymd(Inst_Date))
df2$Date[is.na(df2$Date)] <- as.Date(as.numeric(df2$Inst_Date[is.na(df2$Date)]), origin = "1899-12-30")

saveRDS(df2, "sales_data.rds")


# Add some lat/lon using geocoding
# Error: Google now requires an API key to get lat/lon from addresses. Not so in 2018

# test <- geocode("1665 SE Bethel, Corvallis, OR") # first result right on
# test[1, ] <- NA
# template <- test[1, ]
# 
# poss_geocode <- purrr::possibly(geocode, otherwise = template)
# 
# geofunc <- function(df){
#   address <- paste(df$Situs_Addr1, df$Situs_City, df$Situs_State, sep = ",")
#   res <- poss_geocode(address, limit = 10)
#   out <- res[1, ]
#   return(out)
# }
# 
# df3 <- df2 %>% 
#   filter(Improvement.Class %in% c("Dwelling")) %>%
#   filter(Grade != "Multiple") %>% 
#   filter(!is.na(Date)) %>% 
#   group_by(Inst_Number) %>% 
#   do(geofunc(.))
#   
# saveRDS(df3, "geocode_data.rds")


# check typeArea to see if address recognized (versus city or street)

# read in data from previous code sections and combine
df2 <- readRDS("sales_data.rds")

df3 <- readRDS("geocode_data.rds")

df <- df2 %>% 
  left_join(df3, by = "Inst_Number") %>% 
  filter(Improvement.Class == "Dwelling") %>% 
  filter(Situs_City %in% c("CORVALLIS", "PHILOMATH")) %>% 
  # filter(Improvement.Class %in% c("Dwelling", "Mobile Home")) %>% 
  filter(Grade != "Multiple") %>% 
  filter(Consideration > 100000, Consideration < 500000) %>%
  # filter(RMV >= 100000) %>% 
  # filter(Yr_Blt > 1800) %>% 
  mutate(Bath = FBath + .5 * HBath,
         Year = year(Date),
         Month = month(Date)) %>% 
  filter(!is.na(Date)) #%>% 
  # filter(State == "CA")

# Map
theme_set(theme_bw(base_size = 18)) 

# Error: Google now requires an API key.
# Boo Google!
corvmap <- get_map(location = "Corvallis, OR", zoom = 12, maptype = "roadmap", source = "google")

plt <- ggmap(corvmap) + 
  geom_point(data = df, aes(x = longitudeLocation, y = latitudeLocation, color = Consideration)) #+
  # facet_wrap(~Year)
plt

neighplot <- ggmap(corvmap) + 
  geom_point(data = df, aes(x = longitudeLocation, y = latitudeLocation, color = as.factor(Neigh))) 
neighplot
  
# find house info by address
test <- df[grepl(pattern = "HIGHLAND", x = df$Situs_Addr1, fixed = TRUE), ]
test <- test[grepl(pattern = "1665", x = test$Situs_Addr1, fixed = TRUE), ]

# 
# plot(df$RMV, df$Consideration, use = "complete.obs")
# cor(df$RMV, df$Consideration, use = "complete.obs")
# 
# mod <- lm(Consideration ~ RMV + Situs_City + Acres + Yr_Blt + Condition + Tot.SF + BR + Bath + Year + Month + I(Month^2), data = df)
# summary(mod)


# GAM of price or price - RMV
# filtered to houses in our range
# added some new variables about the neighborhood and scaled date
dfcor <- df %>% 
  filter(BR < 5, Bath < 4, Tot.SF < 2500 & Tot.SF > 500, Acres < 2, Condition != "P") %>% 
  mutate(overpaid = Consideration - RMV,
         percover = overpaid / Consideration) %>% 
  group_by(Neigh) %>% 
  mutate(medRMV = median(RMV, na.rm = TRUE), 
         medPrice = median(Consideration, na.rm = TRUE),
         neighpremium = medPrice - medRMV,
         zDate = as.numeric(scale(Date)),
         price_sqft = Consideration / TotFinSF,
         ac = as.factor(ifelse(Cooling == "None", 0, 1)),
         garage = as.factor(ifelse(AttGar.SF > 0, 1, 0))) %>% 
  filter(
    abs(percover) <= .5,
    Acres < .5,
    latitudeLocation <=  44.62 & latitudeLocation >= 44.52,
    longitudeLocation > -123.4)

test <- dfcor[grepl(pattern = "TILLICUM", x = dfcor$Situs_Addr1, fixed = TRUE), ]

plt <- ggmap(corvmap) + 
  geom_point(data = dfcor, aes(x = longitudeLocation, y = latitudeLocation, color = percover)) +
  scale_color_viridis() +
  facet_wrap(~Year)
plt

plot(dfcor$RMV[dfcor$Year > 2016], dfcor$Consideration[dfcor$Year > 2016], use = "complete.obs")
cor(dfcor$RMV, dfcor$Consideration, use = "complete.obs")

# dfcor$Neigh <- as.factor(as.character(dfcor$Neigh))

# 1st predictive model of price, I like Generalized Additive Models for curve fitting
# they're great for seasonal and spatial models and interactions
mod2 <- gam(Consideration ~ 
              s(RMV) + 
              s(Acres) +
              s(Yr_Blt) +
              s(TotFinSF) +
              s(BR, k = 4) + 
              s(Bath, k = 4) +
              s(Year, k = 4) +
              s(Month, bs = "cc") +
              # s(zDate) + # could use date here, but I split into year and month to model seasonality
              ti(RMV, Year) +
              te(longitudeLocation, latitudeLocation, k = 20), 
            data = dfcor)

summary(mod2)
plot(mod2, residuals = TRUE)


# Houses to predict price from GAM and other models

# newdata <- data.frame(RMV = 241500, Acres = 0.2, Yr_Blt = 1954, Neigh = "4203",
#                       Tot.SF = 1300, BR = 3, Bath = 2, Year = 2018, Month = 6,
#                       TotFinSF = 1300, AttGar.SF = 200, neighpremium = 12655, 
#                       longitudeLocation = -123.2629, latitudeLocation = 44.58104)

source('newdata.R')
newdata <- newdata %>% 
  mutate(Acres = LOT.SIZE / 43560,
         Yr_Blt = YEAR.BUILT,
         TotFinSF = SQUARE.FEET, 
         BR = BEDS,
         Bath = BATHS,
         longitudeLocation = LONGITUDE,
         latitudeLocation = LATITUDE,
         zDate = 2.51) # this zDate is just at the high end of the scaled variable range


price <- predict.gam(object = mod2,
                     newdata = newdata)
newdata$gampred <- price

# really simple linear model for comparison
mod <- lm(Consideration ~ (RMV + Acres + Yr_Blt + TotFinSF + BR + Bath + Year + Month + I(Month^2))^2, 
          data = dfcor)
mod1 <- step(mod)
summary(mod1)
price <- predict(object = mod1,
            newdata = newdata)
newdata$lmpred <- price

# Random forest model for comparison
# RF for price - RMV
x <- dfcor %>% 
  ungroup() %>% 
  dplyr::select(Consideration, RMV, Acres, Yr_Blt, TotFinSF, BR, Bath, Year, Month, zDate, latitudeLocation, longitudeLocation) %>%
  # dplyr::select(Consideration, RMV, latitudeLocation, longitudeLocation) %>%
  filter(complete.cases(.))
y <- x$Consideration
x$Consideration <- NULL
mod <- randomForest(x = x, y = y, importance = TRUE, keep.forest = T, keep.inbag = T)

# 
# # use geo only model prediction as covariate in full model
# x <- dfcor %>% 
#   ungroup() %>% 
#   dplyr::select(Consideration, RMV, Acres, Yr_Blt, TotFinSF, BR, Bath, Year, Month, zDate, latitudeLocation, longitudeLocation) %>%
#   filter(complete.cases(.)) %>% 
#   mutate(geopred = predict(object = mod, newdata = .))
# y <- x$Consideration
# x$Consideration <- NULL
# mod <- randomForest(x = x, y = y, importance = TRUE, keep.forest = T, keep.inbag = T)


price <- predict(object = mod,
                     newdata = newdata[, c("RMV", "Acres", "Yr_Blt", "TotFinSF", "BR", "Bath", "Year", "Month", "zDate", "latitudeLocation", "longitudeLocation")])
newdata$rfpred<- price

# These are cool plots from forestFloor package, but needs updated R
# #compute feature contributions
# ff = forestFloor(mod,x,binary_reg = F,calc_np=T)
# Col = fcol(ff,cols=1,outlier.lim = 2.5)
# 
# #the plot in this blog
# plot(ff,col=Col,plot_GOF = T, limitY = FALSE)
# 
# 


# PREDICTIONS OVER CORVALLIS ----
# Makes a map of model predictions (for an average 3/1 house)

library(ggmap)
library(viridis)
library(raster)

theme_set(theme_bw(base_size = 18))

x_range <- range(dfcor$longitudeLocation)  # min/max longitude of the interpolation area
y_range <- range(dfcor$latitudeLocation)  # min/max latitude of the interpolation area


corvmap <- ggmap(get_stamenmap(bbox = c(x_range[1], y_range[1], x_range[2], y_range[2]), maptype = "terrain", 
                               source = "stamen", color = "bw"))

# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(longitudeLocation = seq(from = x_range[1],
                                   to = x_range[2], 
                                   by = 0.001),
                   latitudeLocation = seq(from = y_range[1], to = y_range[2], 
                                  by = 0.001))  # expand points to grid
grd <- grd %>% 
  mutate(Yr_Blt = mean(dfcor$Yr_Blt, na.rm = TRUE),
         BR = 3, Bath = 1, Acres = mean(dfcor$Acres), RMV = mean(dfcor$RMV),
         TotFinSF = 1100, markettime = 5, zDate = 2.12, Year = 2018, Month = 6)
grd$rfpred <- predict(mod2, grd)
grd$lat <- grd$latitudeLocation
grd$long <- grd$longitudeLocation
grd <- grd %>% 
  filter(rfpred > 0 & rfpred < 300000)

plt <- corvmap +
  geom_raster(data = grd, aes(x = long, y = lat, fill = rfpred), alpha = .5) +
  scale_fill_viridis() +
  coord_cartesian() +
  geom_point(data = dfcor, aes(x = longitudeLocation, y = latitudeLocation), size = .5)
plt

# Plots of prices vs model predictions ----
# This was a useful graph
# The solid 1:1 line would be where the points should fall if the model predictions of price are accurate/unbiased
# The blue regression line shows the actual relationship between my model and price, obviously not aligning with 1:1 line
# Houses above the regression line are more expensive relative to model, below means a better deal
# Pioneer/bobwhite/green were in Philomath
# Starter homes are way overpriced in this market! And this isn't the sold price, just list


library(ggrepel)
newdata2 <- newdata %>% 
  ungroup() %>% 
  # dplyr::select(label, gampred:rfpred, listprice, soldprice) %>%
  rowwise() %>% 
  mutate(avgpred = mean(c(gampred, lmpred, rfpred)),
         bed3bath1 = ifelse(BR == 3 & Bath == 1, "YES", "NO")) %>% 
  arrange(avgpred)

plt <- ggplot(newdata2, aes(x = avgpred, y = listprice)) +
  geom_point() +
  geom_text_repel(aes(label = label, vjust = 1)) +
  geom_abline(slope = 1, intercept = 1) +
  geom_smooth(method = "lm") +
  theme_bw()
plt


# southtown <- dfcor %>% 
#   ungroup() %>% 
#   filter(longitudeLocation > -123.2687, latitudeLocation < 44.5541) %>% 
#   filter(Year >= 2016, BR == 3, Bath == 1) %>% 
#   mutate(Year == 2018, Month == 8) %>%  # standardize time of sale
#   mutate(gampred = predict.gam(mod2, newdata = .))
# plt <- ggplot(southtown, aes(x = Consideration, y = gampred, color = Acres)) +
#   geom_point() +
#   scale_color_viridis() +
#   geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
#   geom_text(aes(label = Situs_Addr1, vjust = 1.2)) +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_bw(base_size = 16)
# plt



# plt <- ggplot(newdata2, aes(x = soldprice, y = avgpred, group = bed3bath1, color = bed3bath1)) +
plt <- ggplot(newdata2, aes(x = soldprice, y = avgpred)) +
  geom_point() +
  geom_text_repel(aes(label = label, vjust = 1.2)) +
  geom_abline(slope = 1, intercept = 1) +
  theme_bw(base_size = 16) +
  geom_point(data = newdata2, aes(x = listprice, y = avgpred, shape = 3)) +
  geom_text_repel(data = newdata2, aes(x = listprice, y = avgpred, label = label, vjust = 1.2)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_identity() +
  xlab("Sold price = dots, list price = plus") +
  ylab("Predicted sale price from models")

plt


# get comps
library(geosphere)
target <- newdata %>% filter(label == "hemlock") %>% mutate(Situs_Addr1 = "HEMLOCK", Consideration = 310000)

comps <- dfcor %>% 
  ungroup() %>% 
  # mutate(zDate = 2.51, Year = 2018, Month = 8) %>% 
  mutate(gampred = predict(mod2, newdata = .)) %>% 
  mutate(distfrom = distGeo(c(target$longitudeLocation, target$latitudeLocation), 
                          cbind(.$longitudeLocation, .$latitudeLocation))) %>%
  mutate(saledate = as.Date(Inst_Date)) %>% 
  filter(distfrom <= 1500) %>% 
  filter(BR == target$BR & Bath <= target$Bath & abs(TotFinSF - target$TotFinSF) < 400)  %>% 
  filter(year(saledate) >= 2017) %>% 
  dplyr::select(saledate, Consideration, RMV, Situs_Addr1, TotFinSF, Acres, Yr_Blt, AttGar.SF, Heating, Cooling, price_sqft, gampred, distfrom) %>% 
  bind_rows(target)

plt <- ggplot(comps, aes(x = Consideration, y = gampred, color = TotFinSF)) +
  geom_point() +
  scale_color_viridis() +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
  geom_text_repel(aes(label = Situs_Addr1, vjust = 1.2)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw(base_size = 16)
plt


library(gridExtra)
png("test.png", height=800, width=1000)
p<-tableGrob(target)
grid.arrange(p)
dev.off()



dfpred <- dfcor %>% 
  mutate(zDate = 2.51, Year = 2018, Month = 8)
dfpred$pred <- predict(mod2)

plt <- corvmap +
  stat_summary_2d(data = dfpred, aes(x = longitudeLocation, y = latitudeLocation, z = pred),
                   fun = median, alpha = .5, bins = 50) +
  scale_fill_viridis() +
  coord_cartesian() +
  theme_bw()
plt

# TODO: 
# 1. Account for neighborhood, although dunno what the codes mean. Try mean $ for each as quantitative var
# 2. Geocode addresses for each neighborhood to get average distance from downtown

# devtools::install_github("abresler/realtR")
# this package looks fun!
library(realtR)

# geocoder
test <- geocode("1665 SE Bethel, Corvallis, OR") # first result right on

df_rates <- realtR::mortgage_rates(return_wide = F)

df_trends <-
  trends(locations = c("Raleigh, NC", "Corvallis, OR", 63301),
         return_message = F)

df_med <- 
  median_prices(locations = c("Raleigh, NC", "Corvallis, OR"),
                return_message = F)

df_vitality <-
  vitality(locations = c("Raleigh, NC", "Corvallis, OR"),
           return_message = F)

df_new_construction_listings <-
  listings(locations = c("Raleigh, NC", "Corvallis, OR"),
           is_new_construction = T)
df_new_construction_listings %>% skimr::skim()


df_detailed_listings <-
  df_new_construction_listings$urlListing %>%
  parse_listing_urls()

df_detailed_listings %>% skimr::skim()


df_listings <-
  listings(locations = c("Corvallis, OR"),
           price_max = 325000)
df_listings %>% skimr::skim()


# More stuff below, probably not useful or well commented.

# Southtown only
df <- read.csv("southtown_2015_2017.csv") %>%
  filter(Bed < 5) %>% # one outlier
  mutate(Date = mdy(Solddate),
         Year = year(Date),
         Month = month(Date),
         Zdate = as.numeric(scale(Date)),
         overpaid = Soldprice - Listprice)

# target house
#bethel
newdata = data.frame(TaxAssess2017 = 184,
                     Lotsqft = 10018, 
                     YearBlt = 1970,
                     Sqft = 1024,
                     Bed = 3, Bath = 1,
                     Zdate = 2.13,
                     Month = 5, 
                     Neigh = "N_alexander",
                     Listprice = 270,
                     Year = 2018)
#atwood
newdata = data.frame(TaxAssess2017 = 121,
                     Lotsqft = 12600, 
                     YearBlt = 1948,
                     Sqft = 850,
                     Bed = 2, Bath = 1,
                     Zdate = 2.13,
                     Month = 5, 
                     Neigh = "N_alexander",
                     Listprice = 200,
                     Year = 2018)

newdata = data.frame(TaxAssess2017 = 140,
                     Lotsqft = 10418, 
                     YearBlt = 1958,
                     Sqft = 1224,
                     Bed = 2, Bath = 1,
                     Zdate = 2.13,
                     Month = 6, 
                     # Neigh = "N_alexander",
                     Listprice = 305,
                     Year = 2018)

plot(df$TaxAssess2017, df$Soldprice, use = "complete.obs")
cor(df$TaxAssess2017, df$Soldprice, use = "complete.obs")

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(df[, c(2, 3, 4, 5, 6, 7, 8, 17)], lower.panel = panel.smooth,
      upper.panel = panel.cor)

mod <- lm(Soldprice ~ Bed + Bath + Sqft + YearBlt + Lotsqft + 
            Zdate + Month + I(Month^2) + Neigh, data = df)
summary(mod)

mod <- lm(Soldprice ~ (Sqft + YearBlt + Lotsqft + 
            Zdate + Month + I(Month^2))^2, data = df)
summary(mod1 <- step(mod))
predict(mod1, newdata)

# GAM of price or price - RMV

mod2 <- gam(Soldprice ~ 
              s(TaxAssess2017) + 
              s(Lotsqft) +
              s(YearBlt) +
              s(Sqft) +
              # s(Bed, k = 3) + 
              # s(Bath, k = 3) +
              s(Zdate) + 
              s(Month, bs = "cc"), data = df, select = TRUE)
summary(mod2)
plot(mod2, residuals = TRUE)
predict.gam(object = mod2,
                     newdata = newdata)

# Overpaid
mod2 <- gam(overpaid ~ 
              s(Listprice) + 
              s(Lotsqft) +
              s(YearBlt) +
              s(Sqft) +
              # s(Bed, k = 3) + 
              # s(Bath, k = 3) +
              s(Zdate) + 
              s(Month, bs = "cc") +
              s(Neigh, bs = "re"), data = df, select = TRUE)
summary(mod2)
plot(mod2, residuals = TRUE)
predict.gam(object = mod2,
            newdata = newdata)



mod <- lm(Consideration ~ (neighpremium + RMV + Acres + Yr_Blt + Tot.SF + BR + Bath + Year + Month + I(Month^2))^2, 
          data = dfcor)
mod1 <- step(mod)
summary(mod1)
price <- predict(object = mod,
                 newdata = data.frame(RMV = 210000, Acres = 0.23, Yr_Blt = 1970,  neighpremium = 18000,
                                      Tot.SF = 1024, BR = 3, Bath = 1, Year = 2018, Month = 4))

# RF for price - RMV


x <- df %>% 
  select(Soldprice, TaxAssess2017, Lotsqft, YearBlt, Sqft, Year, Month, Bed, Bath, Zdate) %>% 
  filter(complete.cases(.))
y <- x$Soldprice
x$Soldprice <- NULL


x <- df %>% 
  select(overpaid, Listprice, Lotsqft, YearBlt, Sqft, Zdate, Month) %>% 
  filter(complete.cases(.))
y <- x$overpaid
x$overpaid<- NULL

mod <- randomForest(x = x, y = y, importance = TRUE, keep.forest = T, keep.inbag = T)


# pairs(x, lower.panel = panel.smooth,
      # upper.panel = panel.cor)
predict(object = mod,
                 newdata = newdata)


#compute feature contributions
ff = forestFloor(mod,x,binary_reg = F,calc_np=T)
Col = fcol(ff,cols=1,outlier.lim = 2.5)

#the plot in this blog
plot(ff,col=Col,plot_GOF = T)

library(rpart)
library(rpart.plot)
tree.1 <- rpart(overpaid ~ ., data=x, method = "anova", 
                control=rpart.control(minsplit=12,cp=0))
# 
plot(tree.1)					# Will make a mess of the plot
text(tree.1)
# 
prp(tree.1)					# Will plot the tree
prp(tree.1,varlen=3)	
