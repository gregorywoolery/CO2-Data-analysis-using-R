install.packages("lookup")
install.packages("readxl")
install.packages("writexl")
install.packages("tidyverse")
install.packages("RColorBrewer")


library("readxl")
library("writexl")
library(dplyr)
library(gapminder)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(RColorBrewer)


#Clean initial table and add region_id to facts_table
fact_table <- read_excel("star_dimensions.xlsx", sheet = "fact_table")
dim_regions <- read_excel("star_dimensions.xlsx", sheet = "dim_regions")


fact_table_w_region_id <- fact_table  %>%
  select(-c(region_id))

glimpse(fact_table_w_region_id)
glimpse(dim_regions)

mergedFact_table <- merge(fact_table, dim_regions, by.x = "region", by.y = "region", all.x = TRUE)


clean_fact_table <- mergedFact_table  %>%
  select(-c(region_id.x)) %>%
  rename(region_id = region_id.y) %>%
  rename(id = ...1) %>%
  na.omit(clean_fact_table) %>%
  relocate(region_id, .before = id) %>%
  relocate(id, .before = region)

#Remove columns with text fields
clean_fact_table <- clean_fact_table %>%
  select(-c(id, region, Entity, Code, emission_type))

write_xlsx(clean_fact_table, "fact_table.xlsx")




fact_table <- read_excel("5 star model design.xlsx", sheet = "fact_table")
dim_organization <- read_excel("5 star model design.xlsx", sheet = "dim_organization")
dim_regions <- read_excel("5 star model design.xlsx", sheet = "dim_region")
dim_country <- read_excel("5 star model design.xlsx", sheet = "dim_country")
dim_emission_type <- read_excel("5 star model design.xlsx", sheet = "dim_emission_type")
dim_sector <- read_excel("5 star model design.xlsx", sheet = "dim_sector")

sector_join <- merge(fact_table, dim_sector, by.x = "emission_sector_id", by.y = "sector_id", all.x = TRUE)

#Slice
oil_sector_join_table <- sector_join %>%
  filter(emission_type_id == 1) %>%
  select(-c(region_id, country_id, organization_id, emission_type_id)) %>%
  relocate(sector, .before = '2007')

agg = aggregate(cbind(oil_sector_join_table$`2010`, 
                      oil_sector_join_table$`2011`,
                      oil_sector_join_table$`2012`,
                      oil_sector_join_table$`2013`,
                      oil_sector_join_table$`2014`,
                      oil_sector_join_table$`2015`)
                ~emission_sector_id, oil_sector_join_table, FUN = sum )

View(agg)


#Dice
country_emission_join <- merge(
                    merge(fact_table, dim_country, all.x = TRUE), 
                    dim_emission_type, by.x = "emission_type_id", by.y = "emission_id", all.x = TRUE) %>%
                    filter(emission_type_id == 1 | emission_type_id == 2)
                    filter(country == "Spain" | country == "Canada")
                  

View(country_emission_join)

agg <- aggregate(cbind(country_emission_join$`2011`, 
                      country_emission_join$`2012`)
                ~emission+country, country_emission_join, FUN = sum )
      
agg <- aggregate(cbind(country_emission_join$`2011`, 
                       country_emission_join$`2012`)
                 ~emission, country_emission_join, FUN = sum )

agg <- aggregate(cbind(country_emission_join$`2011`, 
                       country_emission_join$`2012`)
                 ~country, country_emission_join, FUN = sum )

agg <- aggregate(country_emission_join$emission
                 ~country, country_emission_join, FUN = sum )

View(agg)




#Roll Up
rollup_standard_join <-merge(
  merge(
    merge(fact_table, dim_sector, by.x = "emission_sector_id", by.y = "sector_id", all.x = TRUE), 
          dim_country, all.x = TRUE),
    dim_regions) %>%
  filter(region == "Latin America & Caribbean" | region == "South Asia" | region == "Europe & Central Asia" | region == "North America" )
agg = aggregate(cbind(rollup_standard_join$`2010`, 
                      rollup_standard_join$`2011`,
                      rollup_standard_join$`2012`,
                      rollup_standard_join$`2013`)
                ~region, rollup_standard_join, FUN = sum )
View(agg)



#Visualization
#Line Chart
emission_type_country_join <- merge(
  merge(fact_table, dim_emission_type, by.x = "emission_type_id", by.y = "emission_id", all.x = TRUE),
  dim_country, all.x = TRUE) %>%
  select(-c(organization_id, emission_sector_id, region_id, country_id)) %>%
  filter(country == "Spain")

View(emission_type_country_join)

spain_emission_table =  melt(emission_type_country_join, id.vars=c("country", "emission", "emission_type_id"))%>%
  rename(year = variable) %>%
  rename(emission_amt = value) %>%
  filter(emission_type_id == 1) %>%
  select(-c(emission_type_id))

spain_emission_table$year <- as.Date(as.character(spain_emission_table$year), format = "%Y")



View(spain_emission_table)

ggplot(data = spain_emission_table, mapping = aes(x=year)) +
  geom_line(mapping = aes(y = emission_amt),linetype=1, color = "blue", size =1) +
  geom_point(mapping =  aes(y = emission_amt), color = "blue", size =2) +
  scale_y_continuous(expand = c(0,0))+
  expand_limits(y = c(0,1.05 * max(spain_emission_table$emission_amt)))+
  labs(
    x = "Year",
    y = "Emission Amount",
    title = "Graph showing Oil CO2 Emmisions in Spain.",
    color = "Cylinders",
  ) +
  theme(
    plot.title = element_text(
      face  = "bold", 
      color = "red",
      size  = "15"
    ),
    panel.background = element_rect(
      fill = "grey"
    )
  )


#Pie
region_emissiontype_country_join <- merge(
  merge(fact_table, dim_regions, all.x = TRUE),
  dim_emission_type, by.x = "emission_type_id", by.y = "emission_id", all.x = TRUE) %>%
  select(-c(organization_id, emission_sector_id,country_id))

View(region_emissiontype_country_join)


region_emission_agg = aggregate(cbind(region_emissiontype_country_join$`2010`, 
                                      region_emissiontype_country_join$`2011`,
                                      region_emissiontype_country_join$`2012`,
                                      region_emissiontype_country_join$`2013`,
                                      region_emissiontype_country_join$`2014`,
                                      region_emissiontype_country_join$`2015`,
                                      region_emissiontype_country_join$`2016`,
                                      region_emissiontype_country_join$`2017`)
                ~emission+region, region_emissiontype_country_join, FUN = sum )
View(region_emission_agg)

colnames(region_emission_agg) <- c("emission", "region", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")


region_emission_table =  melt(region_emission_agg, id.vars=c("region", "emission")) %>%
  rename(year = variable) %>%
  rename(emission_amt = value)

View(region_emission_table)


oil_emissions <- region_emission_table %>%
  filter(emission == "oil" & year == "2017")
View(oil_emissions)

a = table(oil_emissions$region) %>%
  as.matrix(a)

vals <- oil_emissions$emission_amt
val_names <- sprintf("%s (%s)", rownames(a), scales::percent(round(vals/sum(vals), 2)))



View(val_names)


par(mar=c(0,4,2,10))
pie(oil_emissions[,4], labels = val_names , col=brewer.pal(length(oil_emissions[,4]), 'Spectral'), 
    main = "Oil Emissions for all Regions in 2017")
legend("topleft", legend = rownames(a), fill = brewer.pal(length(oil_emissions[,4]), 'Spectral'), cex = 0.6)





#Histogram
country_emission_join <- merge(
  merge(fact_table, dim_country, all.x = TRUE),
  dim_emission_type, by.x = "emission_type_id", by.y = "emission_id", all.x = TRUE) %>%
  select(-c(organization_id, emission_sector_id,country_id,region_id, emission_type_id)) %>%
  filter(country == "United States")
View(country_emission_join)

country_emission_table =  melt(country_emission_join, id.vars=c("country", "emission")) %>%
  rename(year = variable) %>%
  rename(emission_amt = value)

View(country_emission_table)


  ggplot(data = country_emission_table, aes(x=emission_amt, fill=emission)) +
    geom_histogram(
      color="#e9ecef",
      alpha=0.6,
      position = 'identity'
    )+
    scale_fill_manual(values=c("#69b3a2", "#404080", "#90db40")) +
    theme_light() +
    labs(
      fill = "",
      x="CO2 Emission amount",
      y="CO2 Emission frequecy"
      ) +
    ggtitle('Histogram showing 2008-2017 frequency of CO2 emmisions for United States')

