Dan Meeting Notes:  
  
Two things we don't want to do when using LCA with Auxiliary Variables:  
- Assume people have perfect class measurement  
- 1 step approach: incorporate aux variables into the model directly  

Vermunt approach less general / easy -- latent classes might still try to move to replicate aux variables (not as robust as BCH), also need to re-estimate the model with a lot going on.  
Vermunt apporach is not feasible for our project.  

More feasible: 
- BCH
- 2-step approach, fix all parameters of LCA then estimate the aux variables.... but this isn't great for us either

All packages to fit LCA in R will generate posterior probabilities for output. For most models, the mean of the posterior probabilities is the mixture probability. We can use this info to create a package where BCH is used for any output, not just MX.

In BCH, the data set is N x K where K is the number of latent classes.  
Dan used a  Custom defined log-likelihood -- weighted ML estimation, anything that does sampling weights will include robust standard errors as an option   
Potential path of least resistance:  
Identify a generalized linear model package that allows for weighting and provides cluster robust standard errors --> crack it open, remove the error trap for negative weights  

Start with distal outcomes, and can add class predictors if we want to later.
- Class predictors tend to have less of an influence on the classes "growing legs and wandering away"

Important LCA / LPA Packages :
- Used in Dan's workshop: polca, mclust, rccm, emmix
- Middle ground: make the package agnostic, but include instructions for common packages

Output:
- Means, variances, standard errors
- Test of equality of means (Wald Test; Chi-Square or F) (Basically an ANOVA)
- Probably don't need to estimate residual variance for all 3 classes (assuming homoscedasticity)
- Correction for multiple comparsions
  
Shiny:
- Upload a dataset that includes posterior probabilities, designate where they are, designate distal outcome of interest (is it binary, count, continuous)
- Visualizations of differences

Class 4/8:
- Using glmmTMB()
- Step 1: function to create the matrix that is needed, then pass this to glmmTMB(), translate to GLM output
- Long format data
- Plan:
    - Hanna & James finish the function
    - Need to test the function (looking ONLY at the estimates; what if I fit it using factor rather than numeric, has an interaction, missing data, etc)
    - Jen will help write the error traps

- For testing: we can use the data and output that Dan already sent

  
Can this be used for other types of models?

