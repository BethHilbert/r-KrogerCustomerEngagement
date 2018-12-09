# Kroger Customer Engagement

I analyzed how weekly mailers and in-store displays correlate with sales. I used R and hierarchical clustering with a dataset provided by 84.51. December 2018. 



Dataset
--------------------
I used the Carbo_Loading dataset provided by 84.51. The [data](http://uc-r.github.io/data_wrangling/final-project) is a subset of 4 potentially related product commodities (pasta/sauce, pancake mix/syrup) including their sales and weekly promotion. Promotions are identified by location in weekly mailer, in-store display, and coupons redeemed. 



Process
--------------------
The original dataset includes 5,197,681 transactions, the grain is one row per household per UPC per day.  There are 927 products with their associated promotion campaigns (ads and in store displays) over a 2 year period. First looked at promotion types and resulting sales by commodity and attribute. Then I aggregate the product information, segment these products into groups with a hierarchical clustering model, and analyze what differentiates the resulting clusters. 


Links
--------------------
RPubs (knit version): http://rpubs.com/Beth_Hilbert/446290

Code: https://github.com/BethHilbert/r-KrogerCustomerEngagement/blob/master/KrogerCustomerEngagement.Rmd

Note: This is a different project than Kroger Case with which my team won the 84.51 Competition in September 2018. I cannot upload that project to Github as it was live data and proprietary information. 
