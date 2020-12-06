# new homes to predict


newdata <- list()
# bethel
newdata[[1]] <- data.frame(label = "bethel", soldprice = 300000, listprice = 270000, LOT.SIZE = 10020, YEAR.BUILT = 1970, 
                           SQUARE.FEET = 1024, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                           LONGITUDE = -123.2585, LATITUDE = 44.5495, zDate = 2.51,
                           markettime = 5, RMV = 193500)

# highland
newdata[[2]] <- data.frame(label = "highland", soldprice = 305000, listprice = 285000, LOT.SIZE = 8700, YEAR.BUILT = 1954, 
                           SQUARE.FEET = 1300, BEDS = 3, BATHS = 2, Year = 2018, Month = 9,
                           LONGITUDE = -123.2629, LATITUDE = 44.58104, zDate = 2.51,
                           markettime = 5, RMV = 241500)
# circle 
newdata[[3]] <- data.frame(label = "circle", soldprice = 288000, listprice = 295000, LOT.SIZE = 10400, YEAR.BUILT = 1958, 
                           SQUARE.FEET = 1220, BEDS = 2, BATHS = 1, Year = 2018, Month = 9,
                           LONGITUDE = -123.29, LATITUDE = 44.58488, zDate = 2.51,
                           markettime = 5, RMV = 269200)
# viewmont
newdata[[4]] <- data.frame(label = "viewmont", soldprice = 298000, listprice = 300000, LOT.SIZE = 7400, YEAR.BUILT = 1978, 
                           SQUARE.FEET = 1050, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                           LONGITUDE = -123.25733, LATITUDE = 44.54373, zDate = 2.51,
                           markettime = 5, RMV = 198300)
# R house
newdata[[5]] <- data.frame(label = "winterfield", soldprice = 289000, listprice = 292500, LOT.SIZE = 6530, YEAR.BUILT = 2005,
                      SQUARE.FEET = 1380, BEDS = 3, BATHS = 2, Year = 2018, Month = 9,
                      LONGITUDE = -123.25733, LATITUDE = 44.54373, zDate = 2.51,
                      markettime = 5, RMV = 277000)

# garyanna
newdata[[6]] <- data.frame(label = "garyanna", soldprice = 333000, listprice = 290000, LOT.SIZE = 7840, YEAR.BUILT = 1960, 
                           SQUARE.FEET = 960, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                           LONGITUDE = -123.269202, LATITUDE = 44.588207, zDate = 2.51,
                           markettime = 5, RMV = 238000)

# glenn
newdata[[7]] <- data.frame(label = "glenn", soldprice = 265000, listprice = 265000, LOT.SIZE = 5662, YEAR.BUILT = 1978, 
                           SQUARE.FEET = 1080, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                           LONGITUDE = -123.2647, LATITUDE = 44.53757, zDate = 2.51,
                           markettime = 5, RMV = 224500)

# 10th st
newdata[[8]] <- data.frame(label = "10th", soldprice = NA, listprice = 300000, LOT.SIZE = 3049, YEAR.BUILT = 1948, 
                           SQUARE.FEET = 720, BEDS = 1, BATHS = 1, Year = 2018, Month = 9,
                           LONGITUDE = -123.269342, LATITUDE = 44.557423, zDate = 2.51,
                           markettime = 5, RMV = 254500)

# 54th st
newdata[[9]] <- data.frame(label = "54th", soldprice = 300000, listprice = 285000, LOT.SIZE = 8276, YEAR.BUILT = 1966, 
                           SQUARE.FEET = 1028, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                           LONGITUDE = -123.31344, LATITUDE = 44.5601, zDate = 2.51,
                           markettime = 5, RMV = 229000)

# tillicum
newdata[[10]] <- data.frame(label = "tillicum", soldprice = 315000, listprice = 315000, LOT.SIZE = 6534, YEAR.BUILT = 1977, 
                           SQUARE.FEET = 1330, BEDS = 3, BATHS = 2, Year = 2018, Month = 9,
                           LONGITUDE = -123.29422, LATITUDE = 44.59542, zDate = 2.51,
                           markettime = 5, RMV = 277320)

# bobwhite
newdata[[11]] <- data.frame(label = "bobwhite", soldprice = 232500, listprice = 230000, LOT.SIZE = 2613, YEAR.BUILT = 1997, 
                            SQUARE.FEET = 1246, BEDS = 3, BATHS = 2.5, Year = 2018, Month = 9,
                            LONGITUDE = -123.3729, LATITUDE = 44.54635, zDate = 2.51,
                            markettime = 5, RMV = 184000)

# roosevelt
newdata[[12]] <- data.frame(label = "roosevelt", soldprice = 307000, listprice = 315000, LOT.SIZE = 8712, YEAR.BUILT = 1965, 
                            SQUARE.FEET = 1248, BEDS = 3, BATHS = 1.5, Year = 2018, Month = 9,
                            LONGITUDE = -123.283728, LATITUDE = 44.591126, zDate = 2.51,
                            markettime = 5, RMV = 276000)

# pioneer
newdata[[13]] <- data.frame(label = "pioneer", soldprice = 231500, listprice = 237500, LOT.SIZE = 9583, YEAR.BUILT = 1985, 
                            SQUARE.FEET = 1008, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                            LONGITUDE = -123.37921, LATITUDE = 44.542760, zDate = 2.51,
                            markettime = 5, RMV = 198000)

# green
newdata[[14]] <- data.frame(label = "green", soldprice = 275000, listprice = 245000, LOT.SIZE = 9583, YEAR.BUILT = 1971, 
                            SQUARE.FEET = 1525, BEDS = 4, BATHS = 1.5, Year = 2018, Month = 9,
                            LONGITUDE = -123.3512, LATITUDE = 44.5396, zDate = 2.51,
                            markettime = 5, RMV = 220000)

# sherwood
newdata[[15]] <- data.frame(label = "sherwood", soldprice = 264000, listprice = 279500, LOT.SIZE = 9583, YEAR.BUILT = 1968, 
                            SQUARE.FEET = 912, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                            LONGITUDE = -123.2403, LATITUDE = 44.5956, zDate = 2.51,
                            markettime = 5, RMV = 219000)

# arthur
newdata[[16]] <- data.frame(label = "arthur", listprice = 230000, LOT.SIZE = 3920, YEAR.BUILT = 1978,
                            SQUARE.FEET = 1088, BEDS = 2, BATHS = 1, Year = 2018, Month = 9,
                            LONGITUDE = -123.275, LATITUDE = 44.584, zDate = 2.51,
                            markettime = 5, RMV = 196000)

# aldrin
newdata[[17]] <- data.frame(label = "aldrin", listprice = 285000, LOT.SIZE = 5227, YEAR.BUILT = 1975,
                            SQUARE.FEET = 1118, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                            LONGITUDE = -123.2628, LATITUDE = 44.5371, zDate = 2.51,
                            markettime = 5, RMV = 211000)

# sycamore
newdata[[18]] <- data.frame(label = "sycamore", listprice = 260000, LOT.SIZE = 10454, YEAR.BUILT = 1958,
                            SQUARE.FEET = 1026, BEDS = 3, BATHS = 1, Year = 2018, Month = 9,
                            LONGITUDE = -123.25857, LATITUDE = 44.59, zDate = 2.51,
                            markettime = 5, RMV = 240000)

# 55th st
newdata[[19]] <- data.frame(label = "55th", soldprice = NA, listprice = 275000, LOT.SIZE = 8276, YEAR.BUILT = 1972, 
                           SQUARE.FEET = 932, BEDS = 3, BATHS = 1.5, Year = 2018, Month = 9,
                           LONGITUDE = -123.31344, LATITUDE = 44.5601, zDate = 2.51,
                           markettime = 5, RMV = 229000)

# cleveland
newdata[[20]] <- data.frame(label = "cleveland", soldprice = NA, listprice = 299900, LOT.SIZE = 5227, YEAR.BUILT = 1961, 
                            SQUARE.FEET = 960, BEDS = 3, BATHS = 1.5, Year = 2018, Month = 9,
                            LONGITUDE = -123.26191, LATITUDE = 44.58572, zDate = 2.51,
                            markettime = 5, RMV = 225000)

# hemlcok
newdata[[21]] <- data.frame(label = "hemlock", soldprice = NA, listprice = 310000, LOT.SIZE = 9147, YEAR.BUILT = 1964, 
                            SQUARE.FEET = 1304, BEDS = 3, BATHS = 1.5, Year = 2018, Month = 9,
                            LONGITUDE = -123.2544, LATITUDE = 44.5946, zDate = 2.51,
                            markettime = 5, RMV = 249000)
newdata <- bind_rows(newdata)