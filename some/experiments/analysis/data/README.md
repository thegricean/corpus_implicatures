## ISSUES:     
each experiment should have 25 unique turkers

- experiment 15 --> 24 unique turkers  
- experiment 17 --> 24 unique turkers  
- experiment 27 --> 41 unique turkers???   
- experiment 28 --> 23 unique turkers  

**TOTAL**   
We should have 725 participants (29 experiments x 25 participants)  
We have 737 participants (x unique)


## HOW TO FIX: 
Workers who did the same experiment twice are included twice in the example-trial.csv file with the same workerid, they can be removed from results_merged. 

We can test more people for experiment 14,17,28 and randomly remove 16 participants from 



## ANALYSIS   
- 122 workers did the experiment in less than 6 minutes  
- 82 people got more than 2 controls wrong (30 of them are already excluded with time exclusion)  
- Nobody got excluded with variance exclusion  
- 65 people more than 2 SD away in at least 2 items (6 of them already excluded. with time and control exclusion)