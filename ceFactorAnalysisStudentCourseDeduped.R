#Load psych package
library(psych)
library(dplyr)
library(readr)

#read in data - to dedupe by student
f18 <- read_csv("CleansedFall2018Responses(1).csv", col_types = cols(Q10 = col_double(), Q11 = col_double(), Q12 = col_double(), Q13 = col_double(), Q9 = col_double()))

#dedupe by student
dedupStudent <- f18 %>% 
  distinct(StuUserID, .keep_all = TRUE)

#dedupe by course
dedupStudentCourse <- dedupStudent %>% 
  distinct(SubjectID, .keep_all = TRUE)

#only retain the numerical items
efa_data <- select(dedupStudentCourse, 4:18)

#conduct single-factor EFA
EFA_model <- fa(efa_data)

#inspect EFA_model
EFA_model

#view the factor loadings
EFA_model$loadings

#create a path diagram of the items' factor loadings
fa.diagram(EFA_model)

#basic descriptive statistics
describe(efa_data)

#graphical respresentation of error
error.dots(as.matrix(efa_data))
error.bars(efa_data)

#take a look at some correlation data
lowerCor(efa_data, use = "pairwise.complete.obs")

#check out the p-values
corr.test(efa_data, use = "pairwise.complete.obs")

#view confidence intervals created when calculating correlations
corr.test(efa_data, use = "pairwise.complete.obs")$ci

#estimate coefficient alpha - the overall internal reliability
alpha(efa_data)

#Calculate split-half reliability.  All observations
splitHalf(efa_data)

#Calculate eigenvalues, first by creating a correlation matrix, then getting the eigenvalues
efa_cor <- cor(efa_data, use = "pairwise.complete.obs")
eigenvals <- eigen(efa_cor)

#checkout eigenvalues returned
eigenvals$values

#make a screeplot
scree(efa_cor, factors = FALSE)

#Multidimenional EFA ----
#Run the EFA with two factors (effort, course, instructor)
efa_model <- fa(efa_data, nfactors = 2)
efa_model

#Run each theorized EFA on the dataset (as luck would have it I capitalized EFA_model for unidim model)
EFA_model$BIC
efa_model$BIC

library(sem)

# Use the wrapper function to create syntax for use with the sem() function
efa_syn <- structure.sem(efa_model)

# Set up syntax specifying which items load onto each factor
efa_model_syn_eq <- "
COURSE: Q1, Q2, Q3, Q4,
INSTR: Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14
"

# Feed the syntax in to have variances and covariances automatically added
efa_model_syn <- cfa(text = efa_model_syn_eq,
                     reference.indicators = FALSE)

# Use the sem() function to run a CFA
efa_model_CFA <- sem(efa_model_syn, data = dedupe_stu_quant)

# Use the summary function to view fit information and parameter estimates
summary(efa_model_CFA)

options(fit.indices = c("CFI", "GFI", "RMSEA", "BIC"))

# Run a CFA using the EFA syntax created earlier
#first, use dedupestudents 4:18
dedupe_stu_quant <- select(dedupStudentCourse, 4:18)
efa_cfa <- sem(efa_model, data = dedupe_stu_quant)
efa_syn <- sem(efa_syn, data = dedupe_stu_quant)

# Locate the BIC in the fit statistics of the summary output
summary(efa_cfa)$BIC

# Compare EFA_CFA BIC to the BIC from the CFA based on theory
summary(efa_syn)$BIC

