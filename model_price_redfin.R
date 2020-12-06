# Scrape price history from Redfin

# import data sheets downloaded from searches (limited to ~360 records)

# Setup ----
library(tidyverse)
library(xlsx)
library(magrittr)
library(lubridate)
library(mgcv)
library(realtR)
library(randomForest)
library(forestFloor)
library(rvest)

# Import Data ----
fs <- list.files("redfin", full.names = TRUE)

# for update of newest
fs <- fs[9]

df <- fs %>% 
  map(.x = ., ~read.csv(.x, header = TRUE)) %>% 
  bind_rows() %>% 
  distinct()

# go through each url and extract price history table
names(df)[21] <- "URL"
df$URL <- as.character(df$URL)
# example
# history <- df[1, "URL"] %>%
#   read_html() %>% 
#   # html_nodes(css = "#property-history-transition-node td") %>%
#   html_nodes(xpath = "//*[@id='property-history-transition-node']/div/table") %>%
#   html_table(header = TRUE, fill = TRUE)

scraper <- function(url){
  out <- url %>%
    read_html() %>% 
    html_nodes(xpath = "//*[@id='property-history-transition-node']/div/table") %>%
    html_table(header = TRUE, fill = TRUE)
  return(out)
}

# connection reset error
history <- df %>% 
  # group_by(URL) %>% 
  # do(scraper(.$URL))
  split(.$URL) %>% 
    map(~scraper(.x$URL))

# try for loop instead
outlist <- vector("list", length = nrow(df))
safe_scrape <- safely(scraper)
for (i in seq_len(nrow(df))){
  # for (i in 1:20){ # testing
    print(i)
  tmp <- df[i, ]
  out <- safe_scrape(tmp$URL)
  if(is.null(out$error)){
    out <- out$result[[1]]
    price <- as.numeric(gsub("[^0-9]", "", out$Price))
    event <- substr(out$`Event & Source`, start = 1, stop = 4)
    source <- ifelse(grepl(pattern = "Public", x = out$`Event & Source`, fixed = TRUE),
                      "public", ifelse(grepl(pattern = "MLS", x = out$`Event & Source`, fixed = TRUE),
                                       "wvmls", NA))
    date <- mdy(out$Date)
    id <- i
    outdf <- data.frame(id, date, event, source, price)
  }else{
    outdf <- data.frame(id = i, date = NA, event = NA, source = NA, price = NA)
  }
  outlist[[i]] <- outdf
}
test <- bind_rows(outlist)


saveRDS(test, "redfin_history.rds")  

history <- readRDS("redfin_history.rds")

# extract useful stuff from histories
# generally want listing (date & price), contigent or pending (date),
# and sold (date & price), but often redundancies or missing 
# throw out other interesting stuff, like sold price over last few decades

simple_history <- function(house_df, type){
  # to prevent weird date conversion
  safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))
  
  resdf <- house_df %>% 
    arrange(desc(date))
  
  if(type == "PAST SALE"){
    resdf <- resdf %>% 
      filter(date <= max(resdf$date[resdf$event == "Sold"]))
  }
  
  lastsold <- resdf %>% 
    filter(event == "Sold") %>% 
    filter(max(date) - date <= 15) %>% 
    filter(row_number() == ifelse(length(source)>1, 
                                  ifelse("public" %in% source, which(source == "public"), 1),
                                  1))
  if(length(which(resdf$event == "Sold")) > 1){
    pastsold <- resdf %>% 
      anti_join(lastsold) %>% 
      filter(event == "Sold" & lastsold$date - date > 50) %>% 
      filter(date == max(date))
  }else{
    pastsold <- data.frame()
  }
  lastlist <- resdf %>% 
    filter(event == "List") %>% 
    filter(max(date) - date <= 15) %>% 
    filter(row_number() == ifelse(length(source)>1, 
                                  ifelse("public" %in% source, which(source == "public"), 1),
                                  1)) %>% 
    filter(ifelse(nrow(pastsold) > 0, 
                  date > pastsold$date, event == "List"))
  
  # Pending breaks stuff
  # lastpend <- resdf %>% 
  #   filter(event == "Pend") %>% 
  #   filter(max(date) - date <= 15) %>% 
  #   filter(row_number() == ifelse(length(source)>1, 
  #                                 ifelse("public" %in% source, which(source == "public"), 1),
  #                                 1))
  
  lastcont <- resdf %>% 
    filter(event == "Cont") %>% 
    filter(max(date) - date <= 15) %>% 
    filter(row_number() == ifelse(length(source)>1, 
                                  ifelse("public" %in% source, which(source == "public"), 1),
                                  1))
  
  lastpric <- resdf %>% 
    filter(event == "Pric") %>% 
    filter(max(date) - date <= 15) %>% 
    filter(row_number() == ifelse(length(source)>1, 
                                  ifelse("public" %in% source, which(source == "public"), 1),
                                  1))
  
  lastdeli <- resdf %>% 
    filter(event == "Deli") %>% 
    filter(max(date) - date <= 15) %>% 
    filter(row_number() == ifelse(length(source)>1, 
                                  ifelse("public" %in% source, which(source == "public"), 1),
                                  1))
  
  lastrec <- bind_rows(lastsold, lastlist, lastcont, lastpric, lastdeli) %>% 
    arrange(desc(date))
  
  # Past sales
  if(type == "PAST SALE"){
    if("List" %in% lastrec$event){
    tmp <- lastrec %>% 
      filter(date >= date[event == "List"]) %>% 
      mutate(days_after_list = date - date[event == "List"])
    listprice <- tmp$price[tmp$event == "List"]
    lastprice <- ifelse(length(which(tmp$event == "Pric")) > 0, 
                        tmp$price[tmp$event == "Pric"], NA)
    soldprice <- tmp$price[tmp$event == "Sold"]
    listdate <- tmp$date[tmp$event == "List"]
    solddate <- tmp$date[tmp$event == "Sold"]
    markettime <- as.numeric(min(tmp$days_after_list[which(tmp$event %in% c("Cont", "Deli", "Pend", "Sold"))], na.rm = TRUE))
    outdf <- data.frame(listprice, lastprice, soldprice, listdate, solddate, markettime)
    }else{
      tmp <- lastrec
      listprice <- NA
      lastprice <- ifelse(length(which(tmp$event == "Pric")) > 0, 
                          tmp$price[tmp$event == "Pric"], NA)
      soldprice <- tmp$price[tmp$event == "Sold"]
      listdate <- NA
      solddate <- tmp$date[tmp$event == "Sold"]
      markettime <- NA
      outdf <- data.frame(listprice, lastprice, soldprice, listdate, solddate, markettime)
    }
  }else{ # current listing
    tmp <- lastrec %>% 
      filter(date >= ifelse("Sold" %in% event, date[event == "Sold"], 
                            date[event == "List"]))
    listprice <- tmp$price[tmp$event == "List"]
    lastprice <- ifelse(length(which(tmp$event == "Pric")) > 0, 
                        min(tmp$price[tmp$event == "Pric"], na.rm = FALSE), NA)
    soldprice <- ifelse(length(which(tmp$event == "Sold")) > 0, 
                        tmp$price[tmp$event == "Sold"], NA)
    listdate <- tmp$date[tmp$event == "List"]
    solddate <- safe.ifelse(length(which(tmp$event == "Sold")) > 0, 
                       tmp$date[tmp$event == "Sold"], NA)
    markettime <- as.numeric(Sys.Date() - max(tmp$date))
    outdf <- data.frame(listprice, lastprice, soldprice, listdate, solddate, markettime)
  }
  return(outdf)
}

# 
# results <- history %>% 
#   group_by(id) %>% 
#   do(simple_history(.))

# # fixing weird records
# history <- history %>% 
#   filter(!(id == 152 & year(date) == 2016))
# history$price[which(history$id == 450 & history$event == "List")] <- 230000

# try for loop instead
outlist <- vector("list", length(unique(history$id)))
safe_history <- safely(simple_history)
for (i in seq_along(unique(history$id))){
  # for (i in 1:20){ # testing
  print(i)
  if(exists("out")) rm(out)
  if(exists("outdf")) rm(outdf)
  tmp <- history %>% filter(id == i)
  type <- df[i, "SALE.TYPE"]
  out <- safe_history(tmp, type)
  if(is.null(out$error)){
    outdf <- out$result %>% 
      mutate(id = i,
             error = "N")
  }else{
    outdf <- data.frame(id = i, error = "Y")
  }
  outlist[[i]] <- outdf
}

results <- bind_rows(outlist)
# look at results giving errors for exceptions to simple_history function
results <- results %>% filter(error == "N")
# history %>% filter(id %in% test$id)

all <- df %>% 
  mutate(id = row_number()) %>% 
  left_join(results, by = "id")
# saveRDS(all, "moddat.rds")



# df <- readRDS("moddat.rds") %>% 
#   bind_rows(all) # for updating most recent
# saveRDS(df, "moddat.rds")

df <- readRDS("moddat.rds")
# test <- df[grepl(pattern = "Ginseng", x = df$ADDRESS, fixed = TRUE), ]


# combine with sales data from benton county




# GAM of price or price - RMV
dfcor <- df %>% 
  filter(CITY == "Corvallis",
         SALE.TYPE == "PAST SALE") %>% 
  mutate(overpaid = soldprice - listprice,
         percover = overpaid / soldprice,
         Year = year(solddate),
         Month = month(solddate),
         zDate = as.numeric(scale(solddate)),
         price_sqft = soldprice / SQUARE.FEET) %>% 
  filter(
    abs(percover) <= .25,
    (LONGITUDE - mean(LONGITUDE, na.rm = TRUE)) < 1,
    (LATITUDE - mean(LATITUDE, na.rm = TRUE)) < 1,
    LOT.SIZE < 15000,
    SQUARE.FEET < 2000,
    markettime < 200)


test <- dfcor[grepl(pattern = "Ginseng", x = dfcor$ADDRESS, fixed = TRUE), ]

source('newdata.R')


# library(ggmap)
# library(viridis)
# theme_set(theme_bw(base_size = 18)) 
# 
# corvmap <- get_map(location = "Corvallis, OR", zoom = 13, maptype = "roadmap", source = "google")
# 
plt <- corvmap +
  geom_point(data = dfcor, aes(x = LONGITUDE, y = LATITUDE, color = PRICE)) +
  scale_color_viridis()
  # facet_wrap(~Year)
plt

mod2 <- gam(
  # overpaid ~
  soldprice ~
              # s(listprice) +
              s(markettime) +
              s(LOT.SIZE) +
              s(YEAR.BUILT) +
              s(SQUARE.FEET) +
              # s(BEDS, k = 3) +
              # s(BATHS, k = 3) +
              s(zDate, k = 10) + 
              s(Month, bs = "cc") +
              te(LONGITUDE, LATITUDE, k = 15), 
            data = dfcor)
gam.check(mod2)
summary(mod2)

# library('schoenberg')
# draw(mod2) # takes too long
vis.gam(mod2, view = c("LONGITUDE", "LATITUDE"), plot.type = "contour", color = "terrain")

plot(mod2, residuals = TRUE)


price <- predict(object = mod2,
                     newdata = newdata)
cbind(newdata$label, round(price))
newdata$gampred <- price


# Price per square foot and time to keep it simple
mod3 <- gam(
  price_sqft ~
    # s(listprice) +
    # s(markettime) +
    s(LOT.SIZE) +
    s(YEAR.BUILT) +
    s(SQUARE.FEET) +
    s(BEDS, k = 3) +
    s(BATHS, k = 3) +
    s(zDate, k = 10) + 
    s(Month, bs = "cc") +
    te(LONGITUDE, LATITUDE, k = 15), 
  select = TRUE,
  data = dfcor)
summary(mod3)
plot(mod3, residuals = TRUE)

price <- predict.gam(object = mod3,
                     newdata = newdata)
price

dfplt <- dfcor %>% 
  # filter(SQUARE.FEET < 1200 & SQUARE.FEET > 800) %>% 
  mutate(catsqft = cut(SQUARE.FEET, include.lowest = TRUE, breaks = quantile(SQUARE.FEET, probs = seq(0, 1, length.out = 5))))


plt <- ggplot(dfplt, aes(x = zDate, y = price_sqft, group = catsqft, color = SQUARE.FEET)) +
  geom_point() +
  geom_smooth() +
  viridis::scale_color_viridis() +
  facet_wrap(~catsqft) +
  theme_bw()
plt


x <- dfcor %>% 
  ungroup() %>% 
  select(price_sqft, LOT.SIZE, YEAR.BUILT, SQUARE.FEET, BEDS, BATHS, 
         LATITUDE, LONGITUDE,
         zDate, markettime) %>% 
  filter(complete.cases(.))
y <- x$price_sqft
x$price_sqft <- NULL
mod <- randomForest(x = x, y = y, importance = TRUE, keep.forest = T, keep.inbag = T, ntree = 750, mtry = 3)




# RF for price - RMV

x <- dfcor %>% 
  ungroup() %>% 
  dplyr::select(soldprice, LOT.SIZE, YEAR.BUILT, SQUARE.FEET, BEDS, BATHS, 
         Month, 
         LATITUDE, LONGITUDE,
         zDate, markettime) %>% 
  filter(complete.cases(.))
y <- x$soldprice
x$soldprice <- NULL
mod <- randomForest(x = x, y = y, importance = TRUE, keep.forest = T, keep.inbag = T, 
                    ntree = 750, mtry = 4, nodesize = 2)


price <- predict(object = mod,
                 newdata = newdata)
cbind(newdata$label, round(price))
newdata$rfpred <- price

#compute feature contributions
ff = forestFloor(mod,x,binary_reg = F,calc_np=T)
Col = fcol(ff,cols=1,outlier.lim = 2.5)
plot(ff,col=Col,plot_GOF = T, limitY = FALSE)

# model paying over listing
x <- dfcor %>% 
  ungroup() %>% 
  select(overpaid, listprice, LOT.SIZE, YEAR.BUILT, SQUARE.FEET, BEDS, BATHS, 
         Month, LATITUDE, LONGITUDE, zDate, markettime) %>% 
  filter(complete.cases(.))
y <- x$overpaid
x$overpaid <- NULL
mod <- randomForest(x = x, y = y, importance = TRUE, keep.forest = T, keep.inbag = T)


price <- predict(object = mod,
                 newdata = newdata)
price


#compute feature contributions
ff = forestFloor(mod,x,binary_reg = F,calc_np=T)
Col = fcol(ff,cols=1,outlier.lim = 2.5)

#the plot in this blog
plot(ff,col=Col,plot_GOF = T, limitY = FALSE)




# why are predictions so off?

x <- dfcor %>% 
  ungroup() %>% 
  dplyr::select(soldprice, LOT.SIZE, YEAR.BUILT, SQUARE.FEET, BEDS, BATHS, 
         Month, 
         LATITUDE, LONGITUDE,
         zDate, markettime) %>% 
  filter(complete.cases(.)) %>% 
  mutate(gampred = predict(object = mod2),
         rfpred = predict(object = mod),
         gamerror = soldprice - gampred,
         rferror = soldprice - rfpred) %>% 
  mutate(catsqft = cut(SQUARE.FEET, include.lowest = TRUE, breaks = quantile(SQUARE.FEET, probs = seq(0, 1, length.out = 4))),
         price_sqft = soldprice / SQUARE.FEET)



plt <- ggplot(x, aes(x = zDate, y = rferror, color = SQUARE.FEET)) +
  geom_point() +
  geom_smooth() +
  viridis::scale_color_viridis() +
  # facet_wrap(~catsqft) +
  theme_bw()
plt


preds <- newdata %>% 
  filter(label != "10th", label != "53th") %>% 
  mutate(price_sqft = listprice / SQUARE.FEET,
         solddate = as.Date("2018-09-01"),
         catsqft = cut(SQUARE.FEET, include.lowest = TRUE, breaks = quantile(dfplt$SQUARE.FEET, probs = seq(0, 1, length.out = 5))))

plt <- ggplot(dfplt, aes(x = solddate, y = price_sqft)) +
  geom_point() +
  geom_smooth() +
  # viridis::scale_color_viridis() +
  geom_point(data = preds, aes(x = solddate, y = price_sqft)) +
  geom_text(data = preds, aes(x = solddate, y = price_sqft, label = label, vjust = 1)) +
  facet_wrap(~catsqft, nrow = 2) +
  theme_bw()
plt


plt <- ggplot(newdata, aes(x = rfpred, y = gampred)) +
  geom_point() +
  geom_text(aes(label = label, vjust = 1)) +
  geom_abline(slope = 1, intercept = 1) +
  theme_bw()
plt


# PREDICTIONS OVER CORVALLIS


library(ggmap)
library(viridis)
library(raster)

theme_set(theme_bw(base_size = 18))

corvmap <- ggmap(get_map(location = "Corvallis, OR", zoom = 12, maptype = "toner", 
                         source = "stamen", color = "bw"))


x_range <- range(dfcor$LONGITUDE)  # min/max longitude of the interpolation area
y_range <- range(dfcor$LATITUDE)  # min/max latitude of the interpolation area

# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(LONGITUDE = seq(from = x_range[1],
                           to = x_range[2], 
                           by = 0.001),
                   LATITUDE = seq(from = y_range[1], to = y_range[2], 
                           by = 0.001))  # expand points to grid
grd <- grd %>% 
  mutate(YEAR.BUILT = mean(dfcor$YEAR.BUILT, na.rm = TRUE),
         BEDS = 3, BATHS = 1, LOT.SIZE = mean(dfcor$LOT.SIZE),
         SQUARE.FEET = 1100, markettime = 5, zDate = 1.92, Year = 2018, Month = 6)
grd$rfpred <- predict(mod2, grd)
grd$lat <- grd$LATITUDE
grd$long <- grd$LONGITUDE
grd <- grd %>% 
  filter(rfpred < 300000)

plt <- corvmap +
  geom_raster(data = grd, aes(x = long, y = lat, fill = rfpred), alpha = .5) +
  scale_fill_viridis() +
  coord_cartesian()
plt


