# reference: http://r-sig-ecology.471788.n2.nabble.com/repeated-measures-NMDS-td5717091.html
# experiment:  24 sites, sampled thrice, 5 spp

## Load packages
require(vegan)
require(permute)

## Data
set.seed(123)
sp <- matrix(runif(24*3*5, 0, 100), nrow = 24 * 3, ncol = 5)  # spp matrix with count values between 0 and 100
env <- rnorm(24*3, 10, 2) # env matrix with mean of 10, sd of 2
rep.mes <- gl(24, 3) # factors

### NMDS:
sol <- metaMDS(sp, trymax = 5)
fit <- envfit(sol~env, permutations = 0) ## perms now won't work!
sol; fit
plot(sol); plot(fit)

B <- 999 ## number of perms

### setting up frame for population of r2 values:
pop <- rep(NA, B + 1)
pop[1] <- fit$vectors$r # populations 1st slot with actual r2 from envfit

## set-up a Control object:
ctrl <- how(plot = Plots(strata = rep.mes),nperm = 999,
                    within = Within(type = "series", mirror = FALSE))
ctrl
## we turn off mirroring as time should only flow in one direction

## Number of observations
nobs <- nrow(sp)

## check it works
matrix(shuffle(nobs, control = ctrl), ncol = 3, byrow = TRUE) #permuted.index
## Yep - Phew!!!

### loop:
set.seed(1)
for(i in 2:(B+1)){
  idx <- shuffle(nobs, control = ctrl)
  fit.rand <- envfit(sol ~ env[idx], permutations = 0)
  pop[i] <- fit.rand$vectors$r
}

### p-value:
pval <- sum(pop >= pop[1]) / (B + 1)
pval

#I get:
#pval
#[1] 0.286

#Now to compare with the actual permutation you'd have gotten from env.fit, you first need:

detach(package:permute)

#Then run:
set.seed(1)
fit2 <- envfit(sol~env, permutations = 999, strata = rep.mes)
fit2

# ***VECTORS
# 
#       NMDS1   NMDS2     r2 Pr(>r)
# env 0.28727 0.95785 0.0315  0.321
# P values based on 999 permutations, stratified within strata.




# ------------------------
# https://github.com/gavinsimpson/permute
# permute example
set.seed(1234)

## unrestricted permutations
shuffle(20)

## observations represent a time series of line transect
CTRL <- how(within = Within(type = "series"))
shuffle(20, control = CTRL)

## observations represent a time series of line transect
## but with mirroring allowed
CTRL <- how(within = Within(type = "series", mirror = TRUE))
shuffle(20, control = CTRL)

## observations represent a spatial grid, 5rx4c
nr <- 5
nc <- 4
CTRL <- how(within = Within(type = "grid", ncol = nc, nrow = nr))
perms <- shuffle(20, control = CTRL)
## view the permutation as a grid
matrix(matrix(1:20, nrow = nr, ncol = nc)[perms],
       ncol = nc, nrow = nr)

## random permutations in presence of strata
cbind(gl(4, 5), 1:20)
plots <- Plots(strata = gl(4, 5))
CTRL <- how(plots = plots, within = Within(type = "free"))
shuffle(20, CTRL)
## as above but same random permutation within strata
CTRL <- how(plots = plots, within = Within(type = "free",
                                           constant = TRUE))
shuffle(20, CTRL)

## time series within each level of block
CTRL <- how(plots = plots, within = Within(type = "series"))
shuffle(20, CTRL)
## as above, but  with same permutation for each level
CTRL <- how(plots = plots, within = Within(type = "series",
                                           constant = TRUE))
shuffle(20, CTRL)

## spatial grids within each level of block, 4 x (5r x 5c)
nr <- 5
nc <- 5
nb <- 4 ## number of blocks
plots <- Plots(gl(nb, 25))
CTRL <- how(plots = plots,
            within = Within(type = "grid", ncol = nc, nrow = nr))
shuffle(100, CTRL)
## as above, but with same permutation for each level
CTRL <- how(plots = plots,
            within = Within(type = "grid", ncol = nc, nrow = nr,
                            constant = TRUE))
shuffle(100, CTRL)

## permuting levels of plots instead of observations
CTRL <- how(plots = Plots(gl(4, 5), type = "free"),
            within = Within(type = "none"))
shuffle(20, CTRL)
## permuting levels of plots instead of observations
## but plots represent a time series
CTRL <- how(plots = Plots(gl(4, 5), type = "series"),
            within = Within(type = "none"))
shuffle(20, CTRL)

## permuting levels of plots but plots represent a time series
## free permutation within plots
CTRL <- how(plots = Plots(gl(4, 5), type = "series"),
            within = Within(type = "free"))
shuffle(20, CTRL)

## permuting within blocks
grp <- gl(2, 10) # 2 groups of 10 samples each
CTRL <- how(blocks = grp)
shuffle(length(grp), control = CTRL)

## Simple function using permute() to assess significance
## of a t.test  
pt.test <- function(x, group, control) {
  ## function to calculate t
  t.statistic <- function(x, y) {
    m <- length(x)
    n <- length(y)
    ## means and variances, but for speed
    xbar <- mean(x)
    ybar <- mean(y)
    xvar <- var(x)
    yvar <- var(y)
    pooled <- sqrt(((m-1)*xvar + (n-1)*yvar) / (m+n-2))
    (xbar - ybar) / (pooled * sqrt(1/m + 1/n))
  }
  ## check the control object
  #control <- check(x, control)$control ## FIXME
  ## number of observations
  Nobs <- nobs(x)
  ## group names
  lev <- names(table(group))
  ## vector to hold results, +1 because of observed t
  t.permu <- numeric(length = control$nperm) + 1
  ## calculate observed t
  t.permu[1] <- t.statistic(x[group == lev[1]], x[group == lev[2]])
  ## generate randomisation distribution of t
  for(i in seq_along(t.permu)) {
    ## return a permutation
    want <- permute(i, Nobs, control)
    ## calculate permuted t
    t.permu[i+1] <- t.statistic(x[want][group == lev[1]],
                                x[want][group == lev[2]])
  }
  ## pval from permutation test
  pval <- sum(abs(t.permu) >= abs(t.permu[1])) / (control$nperm + 1)
  ## return value
  return(list(t.stat = t.permu[1], pval = pval))
}

## generate some data with slightly different means
set.seed(1234)
gr1 <- rnorm(20, mean = 9)
gr2 <- rnorm(20, mean = 10)
dat <- c(gr1, gr2)
## grouping variable
grp <- gl(2, 20, labels = paste("Group", 1:2))
## create the permutation design
control <- how(nperm = 999, within = Within(type = "free"))
## perform permutation t test
perm.val <- pt.test(dat, grp, control)
perm.val

## compare perm.val with the p-value from t.test()
t.test(dat ~ grp, var.equal = TRUE)


#The design:
#We have multivariate species data, sampled at different sites (n = 6) at 3 points in time (N = 24).
# https://thebiobucket.blogspot.com/2011/04/repeat-measure-adonis-lately-i-had-to.html#more
### species matrix with 20 species abundances (mean = 50, sd = 10)
### one time variable, with 3 timepoints, which should be tested
### and a factor denoting sites that were repeatedly sampled (site)

## Load packages
require(vegan)

### Data:
sp <- matrix(rnorm(3 * 6 * 20, 50, 10), nrow = 3 * 6, ncol = 20,
             dimnames = list(1:18, paste("Sp", 1:20, sep = "")))

time <- as.ordered(rep(1:3, 6))
site <- gl(6, 3)
cbind(site, time, sp)

### add time effect at timepoint 3,
### this effect will be tested by adonis():
sp_1 <- sp
sp_1[time==3,] <-  sp[time==3,] + rnorm(20, 10, 1)
cbind(site, time, sp_1)

### choose which species set to test:
test_sp <- sp_1

### computing the true R2-value

### (btw, using dist() defaults to euclidean distance):
print(fit <- adonis(dist(test_sp) ~ time, permutations=1))

### number of perms
B <- 1999

### setting up frame which will be populated by
### random r2 values:
pop <- rep(NA, B + 1)

### the first entry will be the true r2:
pop[1] <- fit$aov.tab[1, 5]

### set up a "permControl" object:
### we turn off mirroring as time should only flow in one direction
#ctrl <- permControl(strata = site, within = Within(type = "series", mirror = FALSE))
ctrl <- how(plots = Plots(strata = site), within = Within(type = "series", mirror = FALSE))
### Number of observations:
nobs <- nrow(test_sp)

### check permutation (...rows represent the sample id):
### ..they are ok!
### within in each repeated sample (= sites) timepoints are shuffled,
### with keeping the sequence intact (e.g., for  site 1: 1,2,3 - 2,3,1 - 3,2,1)
shuffle(nobs, control = ctrl)

### loop:
### in adonis(...) you need to put permutations = 1, otherwise 
### adonis will not run
set.seed(123)
for(i in 2:(B+1)){
  idx <- shuffle(nobs, control = ctrl)
  fit.rand <- adonis(dist(test_sp) ~ time[idx], permutations = 1)
  pop[i] <- fit.rand$aov.tab[1, 5]
}

### get the p-value:
print(pval <- sum(pop >= pop[1]) / (B + 1))
### [1] 0.0035

### the sign. p-value supports the H1 (->there is a time effect).
### ..and the fact that samples are not iid is allowed by
### the customized perms - so this p-value is trustworthy as opposed
### to tests not acknowledging dependency of data points..

### test sp set without an effect:
### replace test_sp with sp set without effect:
test_sp <- sp

### now re-run the script and see the result:
### it is insign. - as expected:

### setting up frame which will be populated by
### random r2 values:
pop <- rep(NA, B + 1)

### computing the true R2-value:
print(fit <- adonis(dist(test_sp) ~ time, permutations = 1))

### the first entry will be the true r2:
pop[1] <- fit$aov.tab[1, 5]

### run the loop:
set.seed(123)
for(i in 2:(B+1)){
  idx <- shuffle(nobs, control = ctrl)
  fit.rand <- adonis(dist(test_sp) ~ time[idx], permutations = 1)
  pop[i] <- fit.rand$aov.tab[1, 5]
}
print(pval <- sum(pop >= pop[1]) / (B + 1))
### [1] 0.701

## make a histogram to see random R2-values and the true one:
hist(pop, xlab = "Population R2")
abline(v = pop[1], col = 2, lty = 3)
text(0.08, 300, paste("true R2,\np = ", pval, sep = ""))
