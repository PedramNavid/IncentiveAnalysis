# IncentiveAnalysis

(Growing) collection of tools for incentive analysis and reward modeling in R. 

It is based on a simple result -> level -> payout reward model for incentive design. 
A result is a measureable event tied to an employee, that is to be paid on.
A level is determined by comparing the result against a set of thresholds, t1 .. tn, resulting in levels l1...ln+1 (if the result exceeds the maximum threshold). 
A payout is a grid p1...pn where a level n provides a payment of pn. 

Current functions include:

* annualize -> takes any result and perform a naive annualization based on the number of months used to observe the results.
* findLevel -> determine the levels for a set of results based on a provided threshold
* findPayout -> determine the payout for a given set of levels, based on a provided payout grid

Upcoming features:

* a reward model class that summarizes participation rates, and calculates cost of compensation (efficiency ratios) based on provided thresholds and payout grids. 
* a wrapper for manipulate/ggplot2 to interactively change thresholds and payout thresholds to view effects on participation rates
