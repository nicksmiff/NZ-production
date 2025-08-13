################
# NZ price production analysis
# Author: Nick Smith
# Date: 13 Aug 2025
# Exploring production and price data for NZ ag products
################

library(tidyverse)

# import data
{
  # these two come from FAOSTAT>Production>Crops and livestock products / Value of Agricultural Production
  price_raw <- read_csv("NZ_pricing_2000-23.csv")
  prod_raw <- read_csv("NZ_production_2000-23.csv")
  
  # this comes from MfE, Agricultural and horticultural land use, clean, 2002 - 2019, https://data.mfe.govt.nz/table/105405-agricultural-and-horticultural-land-use-clean-2002-2019/
  animalArea_raw <- read_csv("NZ_animalArea_2002-19.csv")
}

# tidy data
{
  # remove unwanted columns, first from price, then from production
  price_tidy <- price_raw %>% 
    select(-`Domain Code`, -Domain, -`Area Code (M49)`, -Area, -`Element Code`,
           -`Year Code`, -Flag, -`Flag Description`)
  price_tidy$`Item Code (CPC)` <- sub("i$", "", price_tidy$`Item Code (CPC)`)
  
  prod_tidy <- prod_raw %>% 
    select(-`Domain Code`, -Domain, -`Area Code (M49)`, -Area, -`Element Code`,
           -`Year Code`, -Flag, -`Flag Description`, -Note) %>% 
    filter(Element == "Production") %>% 
    drop_na()

  # separating the production numbers from the area harvested numbers (which only apply to crops)
  area_tidy <- prod_raw %>% 
    select(-`Domain Code`, -Domain, -`Area Code (M49)`, -Area, -`Element Code`,
           -`Year Code`, -Flag, -`Flag Description`, -Note) %>% 
    filter(Element == "Area harvested") %>% 
    drop_na()
  
  # reducing MfE numbers to just what we want that is missing from FAOSTAT
  animalArea_tidy <- animalArea_raw %>% 
    group_by(Year = year, Item = landuse) %>% 
    summarise(Area = sum(area_ha)) %>% 
    filter(Item %in% c("Beef", "Dairy", "Sheep"))
}

# merge price and production data and tidy
{
  merged_data <- prod_tidy %>% 
    merge(price_tidy, by = c("Item Code (CPC)", "Year"), all = TRUE) %>% 
    merge(area_tidy, by = c("Item Code (CPC)", "Year"), all = TRUE)
  
  # add in MfE area values where appropriate
  mfe_years <- unique(animalArea_tidy$Year)
  for (i in mfe_years) {
    
    merged_data[merged_data$Item.x == "Raw milk of cattle" & merged_data$Year == i, "Value"] <- 
      animalArea_tidy[animalArea_tidy$Year == i & animalArea_tidy$Item == "Dairy", "Area"]
    merged_data[merged_data$Item.x == "Raw milk of cattle" & merged_data$Year == i, "Unit"] <- "ha"
    merged_data[merged_data$Item.x == "Raw milk of cattle" & merged_data$Year == i, "Element"] <- "Area"
    
    merged_data[merged_data$Item.x == "Meat of cattle with the bone, fresh or chilled" & merged_data$Year == i, "Value"] <- 
      animalArea_tidy[animalArea_tidy$Year == i & animalArea_tidy$Item == "Beef", "Area"]
    merged_data[merged_data$Item.x == "Meat of cattle with the bone, fresh or chilled" & merged_data$Year == i, "Unit"] <- "ha"
    merged_data[merged_data$Item.x == "Meat of cattle with the bone, fresh or chilled" & merged_data$Year == i, "Element"] <- "Area"
    
    merged_data[merged_data$Item.x == "Meat of sheep, fresh or chilled" & merged_data$Year == i, "Value"] <- 
      animalArea_tidy[animalArea_tidy$Year == i & animalArea_tidy$Item == "Sheep", "Area"]
    merged_data[merged_data$Item.x == "Meat of sheep, fresh or chilled" & merged_data$Year == i, "Unit"] <- "ha"
    merged_data[merged_data$Item.x == "Meat of sheep, fresh or chilled" & merged_data$Year == i, "Element"] <- "Area"
  }
  
  ## currently have up to 4 different prices per item. Identify which price element has the greatest item coverage
  # merged_data1 <- filter(merged_data, Element.y == "Gross Production Value (constant 2014-2016 thousand I$)")
  # merged_data2 <- filter(merged_data, Element.y == "Gross Production Value (constant 2014-2016 thousand US$)")
  # merged_data3 <- filter(merged_data, Element.y == "Gross Production Value (current thousand SLC)")
  # merged_data4 <- filter(merged_data, Element.y == "Gross Production Value (current thousand US$)")
  # merged_data5 <- filter(merged_data, Element.y == "Gross Production Value (constant 2014-2016 thousand SLC)")
  
  # turns out it is "Gross Production Value (constant 2014-2016 thousand I$)" that has the 
  # greatest coverage, but still only 77 items out of the original 98
  merged_data <- merged_data %>% 
    filter(Element.y == "Gross Production Value (constant 2014-2016 thousand I$)") %>% 
    mutate(UnitPrice = Value.y / Value.x, UnitPrice_unit = paste(Unit.y, Unit.x, sep = " per "),
           HaPrice = Value.y / Value, HaPrice_unit = paste(Unit.y, Unit, sep = " per "))
  
  # NOTE! Units are not the same for all production items, e.g. eggs are in number not mass
}

# plot
{
  # first some ugly ones with everything in
  # production
  ggplot(merged_data) +
    geom_line(aes(x = Year, y = Value.x, group = Item.x), stat = "identity")
  
  # price
  ggplot(merged_data) +
    geom_line(aes(x = Year, y = Value.y, group = Item.y), stat = "identity")
  
  # price per unit
  ggplot(merged_data) +
    geom_line(aes(x = Year, y = UnitPrice, group = Item.y), stat = "identity")
  
  # price per ha
  ggplot(merged_data) +
    geom_line(aes(x = Year, y = HaPrice, group = Item.y), stat = "identity")
}

# produce a tidy output sheet
output_data <- merged_data %>% 
  select(-Item, Item.y, -Element.x, -Item.y, -Element.y, -Element) %>% 
  rename(Item = Item.x, Production = Value.x, ProductionUnit = Unit.x, AreaUnit = Unit,
         Area = Value, Value = Value.y, ValueUnit = Unit.y)

write_csv(output_data, "output_data.csv")