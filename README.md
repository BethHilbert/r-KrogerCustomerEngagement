# Kroger Customer Engagement

I analyzed how weekly mailers and in-store displays correlate with sales. I used R models for random forest, association rules, LASSO regression, and hierarchical clustering with a dataset provided by 84.51. December 2018 and April 2019. 



Dataset
--------------------
I used the Carbo_Loading dataset provided by 84.51. The [data](http://8451.com/area51) is a subset of 4 potentially related product commodities (pasta/sauce, pancake mix/syrup) including their sales and weekly promotion. Promotions are identified by location in weekly mailer, in-store display, and coupons redeemed. 



Process
--------------------
The original dataset includes 5,197,681 transactions, the grain is one row per household per UPC per day.  There are 927 products with their associated promotion campaigns (ads and in store displays) over a 2 year period. I looked at promotion types and resulting sales by commodity and attribute.

In Fall 2018, I aggregated the product information, segmented these products into groups with a hierarchical clustering model, and analyzed what differentiates the resulting clusters. 

In Spring 2019 I expanded my analysis, adding models for random forest, association rules, and LASSO regression. I also created a slide presentation and a Tableau dashboard. 


Links
--------------------
Code (April 2019): https://github.com/BethHilbert/r-KrogerCustomerEngagement/blob/master/KrogerCarboPromotions(april2019).R

Slide Presentation (April 2019): 

Tableau dashboard (April 2019): https://public.tableau.com/profile/beth.hilbert#!/vizhome/KrogerCarboPromotions/StoryNewProduct

RPubs (knit version, Dec 2018): http://rpubs.com/Beth_Hilbert/446290

Code (Dec 2018): https://github.com/BethHilbert/r-KrogerCustomerEngagement/blob/master/KrogerCustomerEngagement(dec2018).Rmd

Note: This is a different project than Kroger Case with which my team won the 84.51 Competition in September 2018. I cannot upload that project to Github as it was live data and proprietary information. 
