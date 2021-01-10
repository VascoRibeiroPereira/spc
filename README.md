# Statistical Process Control

This is a R code to check Out of Trend (OOT) results with Shewhart Charts.

**This is under development, if you wish to contribute you're welcome**

The rules applied in this code are:   
- Rule 1 - Beyond Limits: any point +/- 3sigma   
- Rule 2 - Zone A: 2 out of 3 points above 2sigma   
- Rule 3 - Zone B: 4 out of 5 points above sigma   
- Rule 4 - Zone C: 8 consecutive points on the same side of center   
- Rule 5 - Trend: 6 points in a row trending up or down   
- Rule 6 - Overcontrol: 14 consecutive points alternating up and down    
- Rule 7 - Stratification: 15 consecutive points within sigma   
- Rule 8 - Mixture: 8 consecutive points out of +/- sigma   


This rules may change in the number of points accepted to comply with the rules.   

### To do:

- Tranform the functions in order for them to accept the number of points needed to comply   
- Study ways to use the functions: shiny app? Or R Packages? Generate Reports?

- What must be the format of input data? Check if the batches must be numeric or if it works with alpha-numeric values

