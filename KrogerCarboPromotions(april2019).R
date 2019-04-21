#Kroger Promotions
#Beth Hilbert ~ April 2019
#Data available from https://8451.com/area51


# Load Required Packages ----------------------------------------------------------

library(DT)           ## create functional tables in HTML
library(gridExtra)    ## plot more than one graph
library(haven)        ## read statistical software data (SAS files)
library(scales)       ## format labels with dollars and commas
library(readr)        ## write to csv file
library(randomForest) ## random forest
library(vip)          ## variable importance for random forest
library(rpart)        ## single decision tree
library(rpart.plot)   ## plotting single decision tree
library(arules)       ## association rules
library(arulesViz)    ## plot association rules
library(glmnet)       ## lasso
library(factoextra)   ## plot for number clusters (elbow and silhouette)
library(dendextend)   ## plotting cluster dendrogram
library(tidyverse)    ## collection of R data science packages 


# Load Data ------------------------------------------------------------------

files <- c('transactions', 'product_lookup', 'causal_lookup', 'store_lookup')
df_names <- c('transactions', 'product', 'promo', 'store')

for (i in seq_along(files)) {
  # Create file path
  full_path <- paste0('Data/',files[i], '.sas7bdat')
  
  # Import data
  if (file.exists(full_path)) {
    df <- read_sas(full_path)
    df <- zap_formats(df) 
    assign(df_names[i],df)
    rm(df)
  } else {
    response <- paste('There is no file with the name', full_path)
    print(response)
  }
}


# Initial Exploration Before Cleaning -----------------------------------------

for (i in seq_along(df_names)) {
  print(df_names[i])
  glimpse(get(df_names[i]))
}

### Distributions of households, baskets, items

print(paste('Unique households', comma(n_distinct(transactions$household))))
print(paste('Unique baskets', comma(n_distinct(transactions$basket))))
print(paste('Average of', round((n_distinct(transactions$basket) / n_distinct(transactions$household)),2),'baskets per household over 2 years'))

# Number items per basket (basketgrain)
(basketgrain <- transactions %>% 
  group_by(basket) %>% 
  summarize(n_items = n()) %>% 
  arrange(desc(n_items)))
hist(basketgrain$n_items)

# Number baskets for each number of items (basketgrain)
distributionbasket <- basketgrain %>% 
  group_by(n_items) %>% 
  summarize(nbaskets = n())
(distributionbasket %>% 
  ggplot(aes(n_items, nbaskets)) + 
  geom_point() )
(avgbasket <- basketgrain %>% 
  summarize(numbaskets = n(), 
            avgsize = mean(n_items), 
            maxsize = max(n_items)) )
print(paste('Average of',round((n_distinct(transactions$basket) / 
              n_distinct(transactions$household)),2),
            'baskets per household over 2 years'))

# Number baskets per household (housegrain)
(housegrain <- transactions %>% 
    group_by(household) %>% 
    summarize(n_baskets = n_distinct(basket)) %>% 
    arrange(n_baskets))
hist(housegrain$n_baskets)

# Number of households for each number of baskets (housegrain)
distributionhouse <- housegrain %>% 
  group_by(n_baskets) %>% 
  summarize(n_households = n())
(distributionhouse %>% 
  ggplot(aes(n_baskets, n_households)) + 
  geom_point() )

### Dollar sales and coupons

print(paste('Coupons were used on products in',round(mean(transactions$coupon)*100,2),'percent of the transactions'))
print(paste('Sales ranged from $', range(transactions$dollar_sales)[1], 'to $', range(transactions$dollar_sales)[2], ' with median sale of $', median(transactions$dollar_sales)))

# Negative (could be returns)
negative_dollars <- transactions[transactions$dollar_sales < 0,]
count_negative_dollars <- count(negative_dollars)
percent_negative_dollars <- round((count(negative_dollars) / count(transactions))*100, 4)
transactions %>% 
  filter(dollar_sales < 0) %>% 
  group_by(coupon) %>% 
  summarize(n_coupons = n())

# Zero (could be bogo)
zero_dollars <- transactions[transactions$dollar_sales == 0,]
count_zero_dollars <- count(zero_dollars)
percent_zero_dollars <- round((count(zero_dollars) / count(transactions))*100, 4)
transactions %>% 
  filter(dollar_sales == 0) %>% 
  group_by(coupon) %>% 
  summarize(n_coupons = n())

print(paste('transactions with negative dollars', count_negative_dollars, percent_negative_dollars))
print(paste('transactions with zero dollars', count_zero_dollars, percent_zero_dollars))
print(paste0('max dollars in a transaction $', max(transactions$dollar_sales)))

# Evaluate how prevalent negative sales are
negative_dollars %>%
  group_by(upc) %>%
  tally() %>%
  arrange(desc(n))

# Plot potentially unusual transactions
boxplot(transactions$dollar_sales ~ transactions$units, 
        main = 'Potentially Unusual Transactions', 
        xlab = 'Quantity of UPC Per Basket', 
        ylab = 'Dollars of UPC Per Basket')


# Data Cleaning and Engineering  --------------------------------------------------

### Changes to Transaction Data

# Coerce time to numeric
transactions$time_of_transaction <- as.numeric(transactions$time_of_transaction)

### Changes to Promotion Data

# Factor features and display descriptions, change hyphen and slashes to underscore 
promo$display_desc <- str_replace_all(promo$display_desc,'-','_')
promo$display_desc <- str_replace_all(promo$display_desc,
  'Promo/Seasonal Aisle','Promo_Seasonal Aisle')

# Factor feature and display descriptions
feature_levels = c(
  'Wrap Front Feature', 
  'Wrap Back Feature', 
  'Wrap Interior Feature',
  'Front Page Feature', 
  'Back Page Feature',
  'Interior Page Feature', 
  'Interior Page Line Item',
  'Not on Feature')

display_levels = c(
  'Store Front',
  'Front End Cap',
  'Side_Aisle End Cap',
  'Promo_Seasonal Aisle',
  'Secondary Location Display',
  'Rear End Cap',
  'Store Rear',
  'Mid_Aisle End Cap',
  'In_Aisle',
  'In_Shelf',
  'Not on Display')

promo <- mutate(promo, 
  feature_desc = factor(feature_desc, ordered = TRUE, levels = feature_levels),
  display_desc = factor(display_desc, ordered = TRUE, levels = display_levels))

### Changes to Product Data

# Rename and factor commodities
product <- product %>%
  mutate(commodity = case_when(
  str_detect(commodity, 'pasta sauce') ~ 'SAUCE',
  str_detect(commodity, 'pasta') ~ 'PASTA',
  str_detect(commodity, 'pancake mixes') ~ 'PANCAKE',
  str_detect(commodity, 'syrups') ~ 'SYRUP',
  TRUE ~ NA_character_))

commodity_level = c(
  'PASTA', 
  'SAUCE', 
  'PANCAKE',
  'SYRUP')

product <- product %>%
  mutate(commodity = factor(commodity, levels = commodity_level))

# Create variable 'product_attribute' for grouping and renaming descriptions consistently
product <- product %>%
  mutate(product_attribute = case_when(
  str_detect(product_description, 'BUTTERM') ~ 'BUTTERMILK',
  str_detect(product_description, 'BTMK') ~ 'BUTTERMILK',
  str_detect(product_description, 'BTRMK') ~ 'BUTTERMILK',
  str_detect(product_description, 'BTRMLK') ~ 'BUTTERMILK',
  str_detect(product_description, 'MILK') ~ 'BUTTERMILK',
  str_detect(product_description, 'MLK') ~ 'BUTTERMILK',
  str_detect(product_description, 'LITE') ~ 'LITE',
  str_detect(product_description, 'BELG') ~ 'BELGIAN',
  str_detect(product_description, 'BUCKWHEAT') ~ 'BUCKWHEAT',
  str_detect(product_description, 'BLUEB') ~ 'BERRY',
  str_detect(product_description, 'BERRY') ~ 'BERRY',
  str_detect(product_description, 'BLUBRY') ~ 'BERRY',
  str_detect(product_description, 'MAPLE') ~ 'MAPLE',
  str_detect(product_description, 'MPL') ~ 'MAPLE',
  str_detect(product_description, 'CORN') ~ 'CORNSYRUP',
  str_detect(product_description, 'MOLAS') ~ 'MOLASSES',
  str_detect(product_description, 'BLACKSTRA') ~ 'MOLASSES',
  str_detect(product_description, 'SORGHUM') ~ 'SORGHUM',
  str_detect(product_description, 'SUGAR') ~ 'SUGARFREE',
  str_detect(product_description, 'SUGR') ~ 'SUGARFREE',
  str_detect(product_description, 'CANE') ~ 'CANE', 
  str_detect(product_description, 'MARIN') ~ 'MARINARA',
  str_detect(product_description, 'VODKA ') ~ 'VODKA',
  str_detect(product_description, 'MEATLESS') ~ 'MEATLESS',
  str_detect(product_description, 'MEAT') ~ 'MEAT',
  str_detect(product_description, 'SAUSAGE') ~ 'SAUSAGE',
  str_detect(product_description, 'SAUS ') ~ 'SAUSAGE',
  str_detect(product_description, 'SAUS/') ~ 'SAUSAGE',
  str_detect(product_description, 'SAUSG') ~ 'SAUSAGE',
  str_detect(product_description, 'ALFREDO') ~ 'ALFREDO',
  str_detect(product_description, 'ALFEDO') ~ 'ALFREDO',
  str_detect(product_description, 'ALFRDO') ~ 'ALFREDO',
  str_detect(product_description, 'PUTTAN') ~ 'PUTTANESCA',
  str_detect(product_description, 'PESTO') ~ 'PESTO',
  str_detect(product_description, 'MSHRM') ~ 'MUSHROOM',
  str_detect(product_description, 'MUSH') ~ 'MUSHROOM',
  str_detect(product_description, 'MUSRM') ~ 'MUSHROOM',
  str_detect(product_description, 'MSHR') ~ 'MUSHROOM',
  str_detect(product_description, 'CLAM') ~ 'CLAM', 
  str_detect(product_description, 'VEG') ~ 'VEG',
  str_detect(product_description, 'GARDEN') ~ 'VEG',
  str_detect(product_description, 'GRDN') ~ 'VEG',
  str_detect(product_description, 'ONION') ~ 'VEG',
  str_detect(product_description, 'ARTICHOKE') ~ 'VEG',
  str_detect(product_description, 'EGGPLANT') ~ 'VEG',
  str_detect(product_description, 'OLIVE') ~ 'VEG',
  str_detect(product_description, 'TOM') ~ 'VEG',
  str_detect(product_description, 'CHEESE') ~ 'CHEESE',
  str_detect(product_description, 'PARM') ~ 'CHEESE',
  str_detect(product_description, 'CHEDDAR') ~ 'CHEESE',
  str_detect(product_description, 'CHDR') ~ 'CHEESE',
  str_detect(product_description, 'FET') ~ 'FETTUCINI',
  str_detect(product_description, 'EGG ') ~ 'EGG',
  str_detect(product_description, 'ANGEL') ~ 'ANGEL',
  str_detect(product_description, 'ROTI') ~ 'ROTINI',
  str_detect(product_description, 'LASA') ~ 'LASAGNA',
  str_detect(product_description, 'ELBO') ~ 'ELBOW',
  str_detect(product_description, 'MACARON') ~ 'ELBOW',
  str_detect(product_description, 'MANI') ~ 'MANICOTTI',
  str_detect(product_description, 'ORZO') ~ 'ORZO',
  str_detect(product_description, 'CAPE') ~ 'CAPELLINI',
  str_detect(product_description, 'NACH') ~ 'SPINACH',
  str_detect(product_description, 'BOW') ~ 'BOWTIE',
  str_detect(product_description, 'VERMI') ~ 'VERMICELLI',
  str_detect(product_description, 'GNOC') ~ 'GNOCCHI',
  str_detect(product_description, 'ZIT') ~ 'ZITI',
  str_detect(product_description, 'FAR') ~ 'FARFALLE',
  str_detect(product_description, 'TORT') ~ 'TORTELLINI',
  str_detect(product_description, 'ROTE') ~ 'ROTELLE',
  str_detect(product_description, 'MOSTA') ~ 'MOSTACCIOLI',
  str_detect(product_description, 'ALPHA') ~ 'ALPHABET',
  str_detect(product_description, 'RIGAT') ~ 'RIGATONI',
  str_detect(product_description, 'SHELL') ~ 'SHELLS',
  str_detect(product_description, 'PENNE') ~ 'PENNE',
  str_detect(product_description, 'LIN')
  & commodity == 'PASTA' ~ 'LINGUINI',
  str_detect(product_description, 'THIN') ~ 'THIN',
  str_detect(product_description, 'SPAG') 
  & commodity=='PASTA'  ~ 'SPAGHETTI',
  TRUE ~ 'OTHER' ))

product <- product %>%
  mutate(product_attribute = factor(product_attribute)) %>%
  select('product_description', 'product_attribute', 'product_size', everything())

#number of unique product sizes
length(unique(sort(product$product_size)))

# Tidying product$product_size
product <- mutate(product, 

  # Remove spaces
  product_size = str_replace_all(product_size, ' ', ''),
  
  # Convert pounds to ounces           
  product_size = str_replace(product_size, '1LB', '16'),
  product_size = str_replace(product_size, '2LB', '36'),
  product_size = str_replace(product_size, '3LB', '48'),
  product_size = str_replace(product_size, '4LB', '64'),
  product_size = str_replace(product_size, '6LB11OZ', '26'), #Ragu Trad Spaghetti
  product_size = str_replace(product_size, 'GAL', '16'),
  product_size = str_replace(product_size, 'P 1 LB ', '16'),
  product_size = str_replace(product_size, 'N 1 LB', '16'),
  product_size = str_replace(product_size, '1.5LB', '24'),
  
  # Clean these sizes
  product_size = str_replace(product_size,'KH#13384', '16'),
  product_size = str_replace(product_size,'KH#18277', '8'),
  product_size = str_replace(product_size,'KH#18280', '8'),
  product_size = str_replace(product_size,'KH#18283', '8'),
  product_size = str_replace(product_size,'KH#20749', '16'),
  product_size = str_replace(product_size,'KH#2793', '12'),
  product_size = str_replace(product_size,'KH#39724', '8'),
  product_size = str_replace(product_size,'KH#58442', '24'),
  product_size = str_replace(product_size,'KH#61779', '24'),
  product_size = str_replace(product_size,'KH#61780', '24'),
  product_size = str_replace(product_size,'KH#68255', '3'),
  product_size = str_replace(product_size,'KH#6862', '16'),
  product_size = str_replace(product_size,'KH#69333', '12'),
  product_size = str_replace(product_size,'KH#71916', '26'),
  product_size = str_replace(product_size,'KH#8525', '16'),
  product_size = str_replace(product_size,'KH#8623', '16'),
  product_size = str_replace(product_size,'KH#8627', '16'),
  product_size = str_replace(product_size,'KH#8651', '16'),
  product_size = str_replace(product_size,'KH#8652', '16'),
  product_size = str_replace(product_size,'%KH#29483', '26'),
  product_size = str_replace(product_size,'%KH#9390', '7'),
  product_size = str_replace(product_size,'NOTAG', '16'),
  product_size = str_replace(product_size,'##########', ''),
  product_size = str_replace(product_size,'CUSTREQST', '12'),
  product_size = str_replace(product_size, '61/2OZ', '6.5'),
  product_size = str_replace(product_size, '311/2OZ', '31.5'),
  
  # Remove units codes
  product_size = str_replace(product_size, 'OUNCE', ''),
  product_size = str_replace(product_size, 'OZ', ''),
  product_size = str_replace(product_size, '0Z', ''),
  product_size = str_replace(product_size, 'Z', ''),
  product_size = str_replace(product_size, 'FL', ''),
  product_size = str_replace(product_size, 'FMLY', ''),
  product_size = str_replace(product_size, 'PET', ''),
  product_size = str_replace(product_size, 'CR', ''),
  product_size = str_replace(product_size, 'NOTAG', ''),     
  product_size = str_replace(product_size, 'N', ''),
  product_size = str_replace(product_size, 'P', ''),
  product_size = str_replace(product_size, 'SO', ''))

# SAS encodes nulls as blanks, fill in blanks with 16 (most common)
product$product_size[product$product_size == ''] <- '16'

# Coerce to numeric
product <- mutate(product, 
                  product_size = as.numeric(product$product_size))

# Determine appropriate splits for product_size groups
commodity_split <- split(product, f = product$commodity)
pasta_df <- data.frame(commodity_split[1])
sauce_df <- data.frame(commodity_split[2])
pancake_df <- data.frame(commodity_split[3])
syrup_df <- data.frame(commodity_split[4])

# Summary statistics for product sizes by commodity
size_stats <- map(c(pasta_df, sauce_df, pancake_df, syrup_df), summary, na.rm = TRUE)
size_stats[c(3, 9, 15, 21)] #print only product_size stats

# Create variable 'product_size_factor'
PASTA_SMALL <- 9
PASTA_REGULAR <- 17
SAUCE_SMALL <- 12
SAUCE_REGULAR <- 26
PANCAKE_SMALL <- 10
PANCAKE_REGULAR <- 20
SYRUP_SMALL <- 16
SYRUP_REGULAR <- 24

product <- product %>%
  mutate(product_size_factor = case_when(
    commodity == 'PASTA' & product_size <= PASTA_SMALL ~ 'SMALL',
    commodity == 'PASTA' & product_size <= PASTA_REGULAR~ 'REGULAR',
    commodity == 'PASTA' ~ 'JUMBO',
    commodity == 'SAUCE' & product_size <= SAUCE_SMALL ~ 'SMALL',
    commodity == 'SAUCE' & product_size <= SAUCE_REGULAR ~ 'REGULAR',
    commodity == 'SAUCE' ~ 'JUMBO',
    commodity == 'PANCAKE' & product_size <= PANCAKE_SMALL ~ 'SMALL',
    commodity == 'PANCAKE' & product_size <= PANCAKE_REGULAR ~ 'REGULAR',
    commodity == 'PANCAKE' ~ 'JUMBO',
    commodity == 'SYRUP' & product_size <= SYRUP_SMALL ~ 'SMALL',
    commodity == 'SYRUP' & product_size <= SYRUP_REGULAR ~ 'REGULAR',
    commodity == 'SYRUP' ~ 'JUMBO',
    TRUE ~ 'OTHER' ))

size_level = c('SMALL', 
               'REGULAR', 
               'JUMBO')
product <- product %>% 
        mutate(product_size_factor = factor(product_size_factor, levels = size_level))

# Convert brand to uppercase and factor
product$brand = factor(toupper(product$brand))

# Cleaned product data
datatable(product %>% 
  select(upc, commodity, product_attribute, brand, product_description, 
  product_size, product_size_factor, everything()) %>%
  arrange(desc(commodity), upc))

# Clean environment
remove_objects <- c('negative_dollars', 'count_negative_dollars', 
                    'percent_negative_dollars', 'zero_dollars', 
                    'count_zero_dollars', 'percent_zero_dollars', 'avgbasket', 
                    'basketgrain', 'distributionbasket', 'distributionhouse',
                    'housegrain','commodity_split', 'pasta_df', 'sauce_df', 'pancake_df', 
                     'syrup_df', 'size_stats', 'PASTA_SMALL', 'PASTA_REGULAR', 
                     'SAUCE_SMALL', 'SAUCE_REGULAR', 'PANCAKE_SMALL', 
                     'PANCAKE_REGULAR', 'SYRUP_SMALL', 'SYRUP_REGULAR')
rm(list = ls()[ls() %in% remove_objects])


# Join Tables -----------------------------------------------------------------

paste('Transactions without product',transactions %>% 
        anti_join(product) %>% tally() )
paste('Products without transaction',product %>% 
        anti_join(transactions) %>% 
        tally() )

paste('Transactions without promotions', transactions %>% 
        anti_join(promo) %>% 
        tally())
paste('Promotions without transactions', promo %>% 
        anti_join(transactions) %>% 
        tally())

# Join tables
carbo <- transactions %>%
  inner_join(product, by = 'upc')  %>%
  inner_join(promo, by = c('upc', 'week', 'store', 'geography')) %>%
  left_join(store, by = 'store') %>%
  arrange(household, day, upc)

# Rename columns to be clear and consistent
carbo <- rename(carbo, quantity = units, 
                time = time_of_transaction, 
                product_desc = product_description,
                ounces = product_size, 
                store_zip = store_zip_code)

# Reorder columns
carbo <- carbo %>%
  select(household, week, feature_desc, display_desc, commodity, brand, 
    product_attribute, ounces, product_size_factor, upc, product_desc, 
    coupon, quantity, dollar_sales, day, time, basket, everything())

## Key variables I will be looking at are:
print('Commodities')
print(unique(carbo$commodity))
print(paste('Products:',length(unique(carbo$product_desc)) ))
print(paste('Product attributes:',length(unique(carbo$product_attribute))))
print(paste('Mailer levels:', levels(carbo$feature_desc)))
print(paste('Display levels:', levels(carbo$display_desc)))

# Export the data to file
write_excel_csv(carbo, path = 'data/CarboData.csv')

#backup to restore so don't have to keep recreating dataset
carbo_backup <- carbo
carbo <- carbo_backup


# Aggregate to Product-Grain -------------------------------------------

# Create new dataframe at product grain with each row a different upc
carbo_product <- carbo %>%
  group_by(upc, 
           commodity, 
           brand, 
           product_attribute, 
           product_size_factor, 
           product_desc) %>%
  summarize(num_households = n_distinct(household), 
            percent_coupons = mean(coupon),
            total_quantity = sum(quantity), 
            avg_quantity = mean(quantity),
            max_quantity = max(quantity),
            num_baskets = n_distinct(basket),
            num_weeks = n_distinct(week),
            first_week = min(week),
            last_week = max(week),
            avg_dollars = round(mean(dollar_sales),2),
            total_dollars = round(sum(dollar_sales),2),
            num_stores = n_distinct(store),
            num_regions = n_distinct(geography)
  )

# Add column to product grain which counts the mailer promotions
level_names <- carbo %>%
  pull(feature_desc) %>%
  levels() 

level_names_underscore <- str_replace_all(level_names,' ','_') #replace spaces
level_names_underscore <- str_remove(level_names_underscore, '_Feature') #remove duplicate word 

for (i in seq_along(level_names_underscore)) {
  variable_name_new <- paste0('num_feature_', level_names_underscore[i])
  carbo_product1 <- carbo %>% 
    group_by(upc) %>%
    summarize(!!variable_name_new := sum(feature_desc == level_names[i]))
  
  carbo_product <- carbo_product %>%
    left_join(carbo_product1, by = 'upc')
}

# Add column to product grain which counts in-store displays
level_names <- carbo %>%
  pull(display_desc) %>%
  levels() 

level_names_underscore <- str_replace_all(level_names,' ','_') #replace spaces
level_names_underscore <- str_remove(level_names_underscore, '_Display') #remove duplicate word

for (i in seq_along(level_names_underscore)) {
  variable_name_new <- paste0('num_display_', level_names_underscore[i])
  carbo_product1 <- carbo %>% 
    group_by(upc) %>%
    summarize(!!variable_name_new := sum(display_desc == level_names[i]))
  
  carbo_product <- carbo_product %>%
    left_join(carbo_product1, by = 'upc')
}

# Ungroup and clean environment
carbo_product <- carbo_product %>%
  ungroup()
rm(carbo_product1)

carbo_product_backup <- carbo_product #backup of product

# Analysis: Promotion Types and Sales (KPI: num baskets) -----------------------

### Promotion type pairings (both display and mailer)
carbo %>%
  ggplot(aes(display_desc, fct_rev(feature_desc), color = commodity)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_discrete(name = 'In-Store Display Promotion') +
  scale_y_discrete(name = 'Mailer Promotion') +
  ggtitle('Pasta and Sauces Dominate Promotion Types') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ commodity)

### Promotion sales frequency (measured by number of baskets)

# Function: promotion frequency
plot_promotion_frequency <- function(column, axis_label) {
  col_name <- as.name(column)
  carbo %>%
    ggplot(aes(fct_rev(fct_infreq(!! col_name)), fill = commodity)) +
    geom_bar() +
    theme_bw() +
    scale_fill_brewer(palette = 'Set1') +
    scale_x_discrete(name = axis_label) +
    scale_y_continuous(name = 'Number of Baskets', labels = comma) +
    ggtitle('Distribution of Shopping Baskets', subtitle = axis_label) +
    theme(legend.position = 'none') +
    facet_wrap(~ commodity) +
    coord_flip()
}

# Call function for display desc
plot_promotion_frequency(column = 'display_desc', 
                         axis_label = 'In-Store Display Promotion')
# Call function for feature frequency
plot_promotion_frequency(column = 'feature_desc', axis_label = 'Mailer Promotion')

### Promotion Attributes 

# Most frequently sold products identified by  attributes
carbo %>%
    mutate(new_attribute = fct_lump(product_attribute, 
                                    n = 15, 
                                    other_level = 'all other attributes')) %>%
    ggplot(aes(fct_rev(fct_infreq(new_attribute)), fill = commodity)) +
    geom_bar() +
    theme_bw() +
    scale_fill_brewer(palette = 'Set1') +
    scale_x_discrete(name = 'Product Attribute') +
    scale_y_continuous(name = 'Number of Baskets', labels = comma) +
    ggtitle('Most Frequently Sold Product Attributes') +
    theme(legend.position = 'none') +
    facet_wrap(~ commodity) +
    coord_flip()

# function: attribute frequency
plot_attribute_frequency <- function(column, legend_text) {
    col_name <- as.name(column)
    carbo %>%
      filter(commodity == 'SAUCE') %>%
      ggplot(aes(fct_rev(fct_infreq(product_attribute)), fill = !! col_name)) +
      geom_bar() +
      theme_bw() +
      labs(fill = legend_text) +
      scale_fill_brewer(palette = 'Set3') +
      scale_x_discrete(name = 'Product Attribute') +
      scale_y_continuous(name = 'Number of Baskets', labels = comma) +
      ggtitle('Product Attribute Distribution for Sauces', subtitle = legend_text) +
      coord_flip() 
}

# Call function to plot attributes for display_desc
plot_attribute_frequency(column = 'display_desc', 
                     legend_text = 'In-Store Display Promotion')

# Call function to plot attribute for feature_desc
plot_attribute_frequency(column = 'feature_desc', 
                     legend_text = 'Mailer Promotion')

### Frequent Brands 

# Most frequently sold brands
carbo %>%
  mutate(new_brand = fct_lump(brand, 
                              n = 15, 
                              other_level = 'all other brands')) %>%
  ggplot(aes(fct_rev(fct_infreq(new_brand)), fill = commodity)) +
  geom_bar() +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  scale_x_discrete(name = 'Brand') +
  scale_y_continuous(name = 'Number of Baskets', labels = comma) +
  ggtitle('Most Frequently Sold Brands') +
  theme(legend.position = 'none') +
  facet_wrap(~ commodity) +
  coord_flip()

# function: filter for sauce, brand freq
plot_brand_frequency <- function(column, legend_text) {
  col_name <- as.name(column)
  carbo %>%
    filter(commodity == 'SAUCE') %>%
    ggplot(aes(fct_rev(fct_infreq(brand)), fill = !! col_name)) +
    geom_bar() +
    theme_bw() +
    labs(fill = legend_text) +
    scale_fill_brewer(palette = 'Set3') +
    scale_x_discrete(name = 'Brand') +
    scale_y_continuous(name = 'Number of Baskets', labels = comma) +
    ggtitle('Promotion for Sauce by Brand', subtitle = legend_text) +
    coord_flip() 
}

# Call function to plot brands for display_desc
plot_brand_frequency(column = 'display_desc', 
                           legend_text = 'In-Store Display Promotion')

# Call function to plot brands for feature_desc
plot_brand_frequency(column = 'feature_desc', 
                           legend_text = 'Mailer Promotion')

# function: filter for sauce, attribute frequency
plot_attribute_sauce_frequency <- function(column, legend_text) {
  col_name <- as.name(column)
  carbo %>%
    filter(commodity == 'SAUCE') %>%
    ggplot(aes(fct_rev(fct_infreq(product_attribute)), fill = !! col_name)) +
    geom_bar() +
    theme_bw() +
    labs(fill = legend_text) +
    scale_fill_brewer(palette = 'Set3') +
    scale_x_discrete(name = 'Attribute') +
    scale_y_continuous(name = 'Number of Baskets', labels = comma) +
    ggtitle('Promotion for Sauce by Attribute', subtitle = legend_text) +
    coord_flip() 
}

# Call function to plot brands for display_desc
plot_attribute_sauce_frequency(column = 'display_desc', 
                     legend_text = 'In-Store Display Promotion')

# Call function to plot brands for feature_desc
plot_attribute_sauce_frequency(column = 'feature_desc', 
                     legend_text = 'Mailer Promotion')


# Model: Random Forest (on product-grain) ----------------------------------

# Uses libraries: randomForest and vip

carbo_product <- carbo_product_backup 

### num_baskets (distribution skewed)
# num_baskets before log (because of market response model)
carbo_product %>% 
  ggplot(aes(num_baskets)) + 
  geom_density()
summary(carbo_product$num_baskets)
# log of number baskets
carbo_product$log_num_baskets <- log(carbo_product$num_baskets)
carbo_product$num_baskets <- NULL
hist(carbo_product$log_num_baskets, 
     main = 'Number Baskets After Log', 
     xlab = 'log(num_baskets)',
     col = 'dodgerblue3')

### total_dollars (distribution skewed)
# total_dollars before log
carbo_product %>%
  ggplot(aes(total_dollars)) +
  geom_density()
summary(carbo_product$total_dollars)
# log of total_dollars
carbo_product$log_total_dollars <- log(carbo_product$total_dollars)
carbo_product$total_dollars <- NULL
hist(carbo_product$log_total_dollars, 
     main = 'Total Dollars After Log', 
     xlab = 'log(total_dollars)',
     col = 'dodgerblue3')

# Categorical columns to remove
carbo_product$upc <- NULL
carbo_product$product_desc <- NULL
carbo_product$brand <- NULL

# Split into Train, Validation, Test
set.seed(765432)
fractionTrain <- 0.60
fractionValid <- 0.20
fractionTest <- 0.20
sampleSizeTrain <- floor(fractionTrain * nrow(carbo_product))
sampleSizeValid <- floor(fractionValid * nrow(carbo_product))
sampleSizeTest <- floor(fractionTest * nrow(carbo_product))
# Avoid overlapping subsets of indices
indicesTrain <- sort(sample(seq_len(nrow(carbo_product)), size = sampleSizeTrain))
indicesNotTrain <- setdiff(seq_len(nrow(carbo_product)), indicesTrain)
indicesValid  <- sort(sample(indicesNotTrain, size = sampleSizeValid))
indicesTest <- setdiff(indicesNotTrain, indicesValid)
# Output dataframes
carbo_product_train <- carbo_product[indicesTrain, ]
carbo_product_valid <- carbo_product[indicesValid, ]
carbo_product_test <- carbo_product[indicesTest, ]

# Tuning Parameters
mtry_num <- c(seq(1, ncol(carbo_product_train), 1))
MSE.valid <- rep(0, length(mtry_num))
for (i in 1:length(mtry_num)) {
  fit <- randomForest(log_num_baskets ~ .,
                            data = carbo_product_train,
                            mtry = mtry_num[i], #floor(p/3) for regression)
                            ntree = 1000)
  fit.pred <- predict(fit, newdata = carbo_product_valid)
  MSE.valid[i] <- mean((carbo_product_valid$log_num_baskets - fit.pred)^2)
}
plot(mtry_num, MSE.valid, type = 'l', col = 4, lwd = 2, xaxt = "n", 
     main = "Optimum Mtry (")
axis(1, at = mtry_num, las = 1)

ntree_num <- c(seq(500, 3000, 50))
MSE.valid <- rep(0, length(ntree_num))
for (i in 1:length(ntree_num)) {
fit <- randomForest(log_num_baskets ~ .,
                            data = carbo_product_train,
                            mtry = 5,
                            ntree = ntree_num[i])
  fit.pred <- predict(fit, newdata = carbo_product_valid)
  MSE.valid[i] <- mean((carbo_product_valid$log_num_baskets - fit.pred)^2)
}
plot(ntree_num, MSE.valid, type = 'l', col = 4, lwd = 2, xaxt = "n", 
     main = "Optimum Ntree")
axis(1, las = 1)

# Fit random forest model
fit.rf <- randomForest(
  log_num_baskets ~ ., 
  data = carbo_product_train, 
  mtry = 5, #rule is floor(p/3) for regression, but search showed 5
  ntree = 1000, #enough to stabilize error
  importance = TRUE)
print(fit.rf)

fit.pred.train <- predict(fit.rf)
MSE.train <- sum((carbo_product_train$log_num_baskets - fit.pred.train)^2)
fit.pred.test <- predict(fit.rf, carbo_product_test)
MSE.test <- mean((carbo_product_test$log_num_baskets - fit.pred.test)^2)
print(paste('MSE train', MSE.train))
print(paste('MSE test', MSE.test))

fit.pred.valid <- predict(fit.rf, carbo_product_valid)
MSE.valid <- mean((carbo_product_valid$log_num_baskets - fit.pred.valid)^2)
print(paste('MSE valid', MSE.valid))

# Plotting random forest variable importance
options(scipen = 999, digits = 2)
fit_imp <- fit.rf$importance
fit_imp[order(fit_imp[,1], decreasing = TRUE),]
vip(fit.rf, type = 1, fill = 'dodgerblue3') #MSE

# Explaining single tree (versus random forest). Uses libraries rpart and rpart.plot
singletree.fit <- rpart(log_num_baskets~ ., 
                        data = carbo_product_train)
plotcp(singletree.fit)
prp(singletree.fit, digits = 4, extra = 1)
singletree.fit <- rpart(log_num_baskets~ .-num_households -total_quantity -log_total_dollars, 
               data = carbo_product_train)
prp(singletree.fit, digits = 4, extra = 1) 


# Model: Association Rules (on transactions) --------------------------------

# Uses libraries: arules and arulesViz

# Convert basket/product to transaction format for arules
basketgrain1 <- carbo %>% 
  #filter(commodity == c('PANCAKE', 'SYRUP')) %>%
  group_by(basket, product_desc) %>% 
  summarize(n_items = n()) %>% 
  arrange(desc(n_items))
basketgrain1

TransFoodTemp <- basketgrain1 %>% 
  select(basket, product_desc) %>%
  arrange(basket)
# Export the data to file
write_excel_csv(TransFoodTemp, path = 'data/CarboData1.csv')

TransFood <- read.transactions(file = 'data/CarboData1.csv',
                               header = T,
                               format = 'single',
                               cols = c(1,2),
                               sep = ",",
                               rm.duplicates = T)

summary(TransFood) #transactions and most frequent items
itemFrequencyPlot(TransFood, support = 0.02) #individual item freq

# High min confidence results in fewer rules
basket_rules <- apriori(TransFood, parameter = list(sup = 0.0001, conf = 0.05, target = 'rules'))
summary(basket_rules) 
rules_lift <- sort(basket_rules, by = "lift", decreasing = TRUE)
inspect(rules_lift)

rules_subset <- subset(basket_rules, subset = lhs %in% "PRIVATE LABEL SPAGHETTI REGULAR")
rules_lift <- sort(rules_subset, by = "lift", decreasing = TRUE)
inspect(rules_lift)

# Visualize rules
plot(rules_subset)
plot(rules_subset, by = 'lift',method = 'graph')
plot(rules_subset, method = 'grouped')


# Model: Hierarchical Clustering (on product-grain) -------------------------

# Uses libraries: cluster (num clusters) and dendextend (plot)

# Convert to numbers for modeling (clustering)
carbo_product_numbers <- carbo_product_backup %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

# Lasso to select best variables (uses library glmnet)
lasso_fit = glmnet(x = as.matrix(carbo_product_numbers[, -c(which(colnames(carbo_product_numbers) == 'num_baskets'))]), 
                   y = carbo_product_numbers$num_baskets, 
                   alpha = 1)
#use cross validation to pick lambda
cv_lasso_fit = cv.glmnet(x = as.matrix(carbo_product_numbers[, -c(which(colnames(carbo_product_numbers) == 'num_baskets'))]), 
                         y = carbo_product_numbers$num_baskets, 
                         alpha = 1, 
                         nfolds = 5)
plot(cv_lasso_fit)

cv_lasso_fit$lambda.1se #lambda for the minimal cross validation measure. This is the number we put in for s
coef(lasso_fit, s = cv_lasso_fit$lambda.1se) #automates picking s based on cross validation
#lambda big is simpler model. lambda small (negative then model complex)

carbo_product_numbers_subset <- carbo_product_numbers %>% 
                                select(c('num_households', 'total_quantity', 'total_dollars',
                                       'num_display_Rear_End_Cap', 'num_display_Mid_Aisle_End_Cap'))

# Calculate distance
dist_carbo <- dist(carbo_product_numbers_subset, method = 'binary') #jiccard distance
hc_carbo <- hclust(dist_carbo, method = 'complete') #maximum linkage

### Estimate K

# K plot (uses library factoextra)
fviz_nbclust(carbo_product_numbers_subset, kmeans, method = "wss") #compactness cluster
fviz_nbclust(carbo_product_numbers_subset, kmeans, method = "silhouette") #quality fit

# Use dendrogram to estimate k
dend_clustered <- as.dendrogram(hc_carbo) #converts hclust to dendrogram
dend_colored <- color_branches(dend_clustered, k = 2)
p1 <- ggplot(dend_colored) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_classic() +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(name = 'Height') +
  ggtitle('k=2')
dend_colored <- color_branches(dend_clustered, k = 3)
p2 <- ggplot(dend_colored) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_classic() +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(name = 'Height') +
  ggtitle('k=3')
dend_colored <- color_branches(dend_clustered, k = 5)
p3 <- ggplot(dend_colored) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_classic() +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(name = 'Height') +
  ggtitle('k=5')
grid.arrange(p1, p2, p3, ncol = 3)

# Fit model and append cluster number
cluster_assignment <- cutree(hc_carbo, k = 2) #extract cluster assignment
carbo_product_clustered <- carbo_product_backup
carbo_product_clustered$cluster = cluster_assignment #append assignment to original dataframe

# Statistics on clusters (all variables)
clustered_stats <- carbo_product_clustered %>% 
  select(-(1:6)) %>%
  group_by(cluster) %>% 
  summarise_all(funs(round(mean(.),2)))
datatable(clustered_stats)

# Statistics on clusters (only variables used in models)
clustered_stats_focused <- clustered_stats %>%
      select(c('num_households', 'total_quantity', 'total_dollars',
           'num_display_Rear_End_Cap', 'num_display_Mid_Aisle_End_Cap'))
datatable(clustered_stats_focused)

# Table of counts
carbo_product_clustered %>% 
  group_by(cluster) %>% 
  summarise(num_prod_cluster = n())

# Products sorted by cluster
clustered_products_details <- carbo_product_clustered %>% 
  group_by(cluster, commodity, product_attribute) %>% 
  select(cluster, commodity, product_attribute, product_desc) %>%
  arrange(cluster, commodity, product_attribute)
View(clustered_products_details)

# Products sorted by cluster with all variables displayed
datatable(carbo_product_clustered %>%
            select(cluster, everything()) %>%
            arrange(cluster) )

# Compare brands (Kroger versus other) in clusters
carbo_product_clustered %>% 
  mutate(kroger_brand = fct_other(brand, 
                                  keep = c('PRIVATE LABEL VALUE', 'PRIVATE LABEL', 'PRIVATE LABEL PREMIUM'))) %>% 
  group_by(cluster, commodity) %>% 
  count(kroger_brand) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(kroger_brand, n, color = commodity)) + 
  geom_point() +
  facet_grid(commodity ~ as.factor(cluster)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = 'Set1') +
  ggtitle('Cluster Distribution of Kroger Brands') +
  labs(x = 'Brand', y = 'Number Products') +
  coord_flip()

# Clean environment
remove_objects <- c('cluster_assignment', 'carbo_product_numbered', 'hc_carbo', 
                    'dend_colored', 'dend_clustered', 'clustered_stats', 'sil_df')
rm(list = ls()[ls() %in% remove_objects])


