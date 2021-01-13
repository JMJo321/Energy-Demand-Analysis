## Treatment Effects by Term(s) between Periods: for RSGH

### Forward Case

![Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSGH_Forward](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSGH_Forward.png)


![Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_With-Interaction_RSGH_Forward](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_With-Interaction_RSGH_Forward.png)


_Notes_: While the upper plot is for a linear model without interaction term, the lower plot is for a linear model with interaction term. Only for five bandwidths (i.e., 10%, 20%, 30%, 40%, and 50%), treatment effects are estimated. The term(s) between periods has the value of 1 (2, 3, and so on) when period 0 and period 1 (period 0 and period 2, period 0 and period3, and so on) data are used to estimate the treatment effects. The horizontal axis indicates the month for which the treatment effects are estimated. For example, the 1-term (2-term, 3-term, and so on) point estimate for April indicates the effect of treatment on March (February, January, and so on). Standard errors are selected between heteroskedasticity-robust and cluster (by Accounting-Premise IDs and Billing Year-Month) standard errors. Error bars indicate the 95% confidence intervals. Both point estimates and error bars are jittered for readability. When estimating the treatment effects, observations between 2005 and 2011 are exploited only. The base usage quantity during summer season (May through October) is larger than that during winter season (November through April).

+ When bandwidth is narrow, the estimated treatment effects are, in general, statistically insignificant.
+ As bandwidth become wider, positive summer-season treatment effects for households that were treated during winter season increase. This phenomenon seems to be due to responses of heavy consumers.
+ When bandwidth is wide, households that were treated on May or October have statistically significant negative treatment effects.



























### Backward Case

![Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSGH_Backward](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSGH_Backward.png)



![](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_With-Interaction_RSGH_Backward.png)



_Notes_: While the upper plot is for a linear model without interaction term, the lower plot is for a linear model with interaction term. Only for five bandwidths (i.e., 10%, 20%, 30%, 40%, and 50%), treatment effects are estimated. The term(s) between periods has the value of 1 (2, 3, and so on) when period 0 and period 1 (period 0 and period 2, period 0 and period3, and so on) data are used to estimate the treatment effects. The horizontal axis indicates the month for which the treatment effects are estimated. For example, the 1-term (2-term, 3-term, and so on) point estimate for January indicates the effect of treatment on February (March, April, and so on). Standard errors are selected between heteroskedasticity-robust and cluster (by Accounting-Premise IDs and Billing Year-Month) standard errors. Error bars indicate the 95% confidence intervals. Both point estimates and error bars are jittered for readability. When estimating the treatment effects, observations between 2005 and 2011 are exploited only. RSCH and RSEH rate codes have the same base usage quantity. The base usage quantity during winter season (November through April) is larger than that during summer season (May through October).

+ Up to 30% bandwidth, the estimated treatment effects are insignificant generally.
+ As bandwidth become wider, households that were treated during winter season have significant positive summer-season treatment effects. 

























## Treatment Effects by Term(s) between Periods: for RSCH & RSEH

### Forward Case

![Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSCH-and-RSEH_Forward](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSCH-and-RSEH_Forward.png)



![Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_With-Interaction_RSCH-and-RSEH_Forward](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_With-Interaction_RSCH-and-RSEH_Forward.png)



### Backward Case

![Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSCH-and-RSEH_Backward](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_Without-Interaction_RSCH-and-RSEH_Backward.png)



![Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_With-Interaction_RSCH-and-RSEH_Backward](/Users/jmjo/Dropbox/00_JMJo/Projects/Energy-Demand-Analysis/03_Note/01_Descriptive-Analysis/02_RD-Design/02_Plots/Regression-Results_RD-Design_Treatment-Effect-by-Term-between-Periods_With-Interaction_RSCH-and-RSEH_Backward.png)