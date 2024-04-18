library(PlayerRatings)
library(reshape2)
library(MASS) # required for ordinal logistic regression

##### Data Wrangling
mls.original <- read.csv('data/mls2001-2021.csv')
mls.big <- mls.original[,c('home', 'away', 'date', 'year', 'venue', 'league', 
                           'game_status', 'shootout', 'home_score', 
                           'away_score', 'part_of_competition')]
mls.regular.season <- 
  mls.big['Regular Season' %in% mls.big$part_of_competition, ]

mls.regular.season$score_difference <- mls.regular.season$home_score - 
                                        mls.regular.season$away_score

mls.regular.season[mls.regular.season['home'] == 'Chicago', 
                       'home'] = 'Chicago Fire FC'
mls.regular.season[mls.regular.season['home'] == 'Colorado', 
                       'home'] = 'Colorado Rapids'
mls.regular.season[mls.regular.season['home'] == 'Columbus', 
                       'home'] = 'Columbus Crew SC'
mls.regular.season[mls.regular.season['home'] == 'Columbus Crew', 
                       'home'] = 'Columbus Crew SC'
mls.regular.season[mls.regular.season['home'] == 'DC United', 
                       'home'] = 'D.C. United'
mls.regular.season[mls.regular.season['home'] == 'Dallas', 
                       'home'] = 'FC Dallas'
mls.regular.season[mls.regular.season['home'] == 'Houston Dynamo', 
                       'home'] = 'Houston Dynamo FC'
mls.regular.season[mls.regular.season['home'] == 'Miami', 
                       'home'] = 'Inter Miami CF'
mls.regular.season[mls.regular.season['home'] == 'KC Wiz', 
                       'home'] = 'KC Wizards'
mls.regular.season[mls.regular.season['home'] == 'Montreal Impact', 
                       'home'] = 'CF Montréal'
mls.regular.season[mls.regular.season['home'] == 'New England', 
                       'home'] = 'New England Revolution'
mls.regular.season[mls.regular.season['home'] == 'San Jose', 
                       'home'] = 'San Jose Earthquakes'
mls.regular.season[mls.regular.season['home'] == 'MetroStars', 
                       'home'] = 'New York Red Bulls'
mls.regular.season[mls.regular.season['home'] == 'Tampa Bay', 
                       'home'] = 'Tampa Bay Mutiny'
mls.regular.season[mls.regular.season['home'] == 'LAFC', 
                   'home'] = 'Los Angeles FC'

mls.regular.season[mls.regular.season['away'] == 'Chicago', 
                       'away'] = 'Chicago Fire FC'
mls.regular.season[mls.regular.season['away'] == 'Colorado', 
                   'away'] = 'Colorado Rapids'
mls.regular.season[mls.regular.season['away'] == 'Columbus', 
                   'away'] = 'Columbus Crew SC'
mls.regular.season[mls.regular.season['away'] == 'Columbus Crew', 
                   'away'] = 'Columbus Crew SC'
mls.regular.season[mls.regular.season['away'] == 'DC United', 
                   'away'] = 'D.C. United'
mls.regular.season[mls.regular.season['away'] == 'Dallas', 
                   'away'] = 'FC Dallas'
mls.regular.season[mls.regular.season['away'] == 'Houston Dynamo', 
                   'away'] = 'Houston Dynamo FC'
mls.regular.season[mls.regular.season['away'] == 'Miami', 
                   'away'] = 'Inter Miami CF'
mls.regular.season[mls.regular.season['away'] == 'KC Wiz', 
                   'away'] = 'KC Wizards'
mls.regular.season[mls.regular.season['away'] == 'Montreal Impact', 
                   'away'] = 'CF Montréal'
mls.regular.season[mls.regular.season['away'] == 'New England', 
                   'away'] = 'New England Revolution'
mls.regular.season[mls.regular.season['away'] == 'San Jose', 
                   'away'] = 'San Jose Earthquakes'
mls.regular.season[mls.regular.season['away'] == 'MetroStars', 
                   'away'] = 'New York Red Bulls'
mls.regular.season[mls.regular.season['away'] == 'Tampa Bay', 
                   'away'] = 'Tampa Bay Mutiny'
mls.regular.season[mls.regular.season['away'] == 'LAFC', 
                   'away'] = 'Los Angeles FC'


mls.regular.season$result <- 0.5
mls.regular.season[mls.regular.season$score_difference > 0, 'result'] <- 1
mls.regular.season[mls.regular.season$score_difference < 0, 'result'] <- 0

mls.regular.season <- mls.regular.season[!(mls.regular.season$home %in% 
                                             c('East All-Stars', 
                                               'West All-Stars')),]

mls.regular.season[mls.regular.season$year <= 2000, 'date_corrected'] <-
  as.Date(mls.regular.season[mls.regular.season$year <= 2000, 'date'], 
          format="%m/%d/%Y")
mls.regular.season[mls.regular.season$year > 2000,'date'] <- 
  paste(mls.regular.season[mls.regular.season$year > 2000,'date'], 
        mls.regular.season[mls.regular.season$year > 2000,'year'],
        sep=", ")
mls.regular.season[mls.regular.season$year > 2000,'date_corrected'] <- 
  as.Date(mls.regular.season[mls.regular.season$year > 2000, 'date'],
          format="%A, %B %d, %Y")


##### Fit basic elo model to compare our own elo function

mls <- mls.regular.season[mls.regular.season$year %in% 2002:2022, 
                          c('year', 'home', 'away', 'result')]

elo(mls, init=1500, gamma=0, kfac=30, history=FALSE, sort=TRUE)

##### Write elo.g function for goal difference 

mls.g <- mls.regular.season[mls.regular.season$year %in% 2002:2022,
                            c('year', 'date_corrected', 'home', 'away', 
                              'result', 'score_difference')]
colnames(mls.g) <- c('year', 'date', 'home', 'away', 'result', 'gd')


We <- function(r1, r2, gamma=0) {
  return ((1 + 10**(-(r1-r2+gamma)/400))**(-1))
}

elo.g <- function(df, init=1500, status=NULL, k0=30, lambda=0, gamma=0) {
  # Set up matrix of ratings, with one for each date
  teams <- unique(mls.regular.season$home)
  years <- as.character(unique(df$year))
  history <- matrix(data=NA, nrow=length(teams), ncol=length(years)+1)
  dimnames(history) <- list(teams, c('init', years))
  
  # Set initial ratings
  ratings <- numeric(0)
  if (!is.null(status)) {
    for (i in 1:length(teams)) {
      history[teams[i],1] <- ifelse(!is.na(status[teams[i]]),status[teams[i]],
                                    NA)
      ratings[teams[i]] <- ifelse(!is.na(status[teams[i]]), status[teams[i]],
                                  init)
    }
  } else {
    history[,1] <- init
    ratings <- rep(init, length(teams))
    names(ratings) <- teams
  }
  
  # Loop through games, and update ratings
  for (y in min(years):max(years)) {
    temp_ratings = ratings
    year_df = df[df$year == as.integer(y),]
    for (i in 1:dim(year_df)[1]) {
      temp_ratings[year_df[i, 'home']] <- temp_ratings[year_df[i, 'home']] + 
        k0*(1 + abs(year_df[i, 'gd']))**lambda * 
        (year_df[i, 'result'] - 
        We(ratings[year_df[i, 'home']], ratings[year_df[i, 'away']], 
                              gamma=gamma))
      
      temp_ratings[year_df[i, 'away']] <- temp_ratings[year_df[i, 'away']] + 
        k0*(1 + abs(year_df[i, 'gd']))**lambda * 
        ((1 - year_df[i, 'result']) - We(ratings[year_df[i, 'away']], 
                                    ratings[year_df[i, 'home']], gamma=gamma))
    }
    # print(temp_ratings)
    # print(ratings)
    ratings = temp_ratings
    # history[df$Player, as.character(df[i, 'year'])] <- ratings
    # history[df$Player, as.character(df[i, 'year'])] <- ratings
    history[,as.character(y)] <- ratings
  }
  
  return(list('ratings'=ratings, 'history'=history))
}

##### Function to predict outcomes based on elo ratings

predict.elo.g <- function(ratings, df, gamma=0) {
  row.We <- function(row) {
    return (We(ratings[[row['home']]], ratings[[row['away']]], gamma=gamma))
  }
  return (apply(df, 1, row.We))
}

##### Compare our function to elo in PlayerRatings package
mls.mini <- mls[1:50,]
mls.g.mini <- mls.g[1:50,]
elo.mini <- elo(mls.mini, init=1500, gamma=0, kfac=30)
elo.g.mini <- elo.g(mls.g.mini)

##### Check correspondence between ratings from PlayerRatings package 
##### and our own implementation; checks now show ours is working properly
elo.g.full <- elo.g(mls.g, status=NULL, k0=30, lambda=0, gamma=0)
elo.full <- elo(mls, init=1500, gamma=0, kfac=30, history=FALSE, sort=FALSE)
plot(elo.g.full$ratings[elo.full$ratings$Player] ~ elo.full$ratings$Rating)
cor(elo.g.full$ratings[elo.full$ratings$Player], elo.full$ratings$Rating)

##### Cross validate k0 and lambda
ls.lambda <- 0:31 * 0.16
ls.k <- 0:31 * 1.3
log.likelihood.hfa <- expand.grid(ls.k, ls.lambda)
colnames(log.likelihood.hfa) <- c("k0", "lambda")
log.likelihood.hfa$ll <- rep(-1e6, dim(log.likelihood.hfa)[1])
kfac.init <- 30

for (j in 1:dim(log.likelihood.hfa)[1]) {
  ratings.elo <- elo.g(mls.g[mls.g$year==2002,], init=1500, 
                       k0=kfac.init, 
                       gamma=0)
  ratings.elo <- elo.g(mls.g[mls.g$year>2002 & mls.g$year <= 2017,], 
                       status=ratings.elo$ratings, 
                       k0=log.likelihood.hfa[j, 'k0'], 
                       lambda=log.likelihood.hfa[j, 'lambda'])
  
  # Validation on val set
  ll.tmp = 0
  for (val.year in 2018:2022){
    pred <- predict.elo.g(ratings.elo$ratings, mls.g[mls.g$year==val.year,], 
                          gamma=0)
    # Log-likelihood
    ll.tmp <- ll.tmp +
      sum(mls.g[mls.g$year==val.year,]$result * log(pred) +
            (1 - mls.g[mls.g$year==val.year,]$result) * 
            log(1-pred))
    # Update model 
    ratings.elo <- elo.g(mls.g[mls.g$year==val.year,], 
                         k0 = log.likelihood.hfa[j, 'k0'], 
                         status = ratings.elo$ratings, 
                         lambda=log.likelihood.hfa[j, 'lambda'])
  }
  log.likelihood.hfa[j, 'll'] = ll.tmp
}

best.ind = which.max(log.likelihood.hfa$ll)
best.k = log.likelihood.hfa[best.ind,]["k0"][[1]]
best.lambda = log.likelihood.hfa[best.ind,]["lambda"][[1]]
best.k = 3.9 # so you don't have to re-run cv
best.lambda = 0.16 # so you don't have to re-run cv

##### Cross validate for BASE ELO
ls.k.base.elo <- 0:31 * 1.3
log.likelihood.k <- rep(-1e6, length(ls.k.base.elo))
kfac.init <- 30

for (j in 1:length(ls.k.base.elo)) {
  ratings.elo.base <- elo(mls[mls$year==2002,], init=1500, 
                       kfac=kfac.init, 
                       gamma=0)
  ratings.elo.base <- elo(mls[mls$year>2002 & mls$year <= 2017,], 
                       status=ratings.elo.base$ratings, 
                       kfac=ls.k.base.elo[j])
  # Validation on val set
  ll.tmp = 0
  for (val.year in 2018:2022){
    pred <- predict(ratings.elo.base, mls[mls$year==val.year,], 
                          gamma=0)
    
    # Account for games with teams that have no ratings yet
    home_NA = (mls[mls$year  == val.year, "home"])[is.na(pred)]
    away_NA = (mls[mls$year  == val.year, "away"])[is.na(pred)]
    
    replace_NA_rating <- function(x)
      ifelse(x %in% unique(ratings.elo.base$ratings$Player), 
             ratings.elo.base$ratings$Rating[which(ratings.elo.base$ratings$Player == x)], 1500)
    
    home_NA_ratings = sapply(home_NA, replace_NA_rating)
    away_NA_ratings = sapply(away_NA, replace_NA_rating)
    
    pred[is.na(pred)] = We(home_NA_ratings, away_NA_ratings)
    
    ll.tmp <- ll.tmp +
      sum(mls[mls$year==val.year,]$result * log(pred) +
            (1 - mls[mls$year==val.year,]$result) * 
            log(1-pred))
    # Update model 
    ratings.elo.base <- elo(mls[mls$year==val.year,], 
                         init = 1500,
                         kfac = ls.k.base.elo[j], 
                         status = ratings.elo.base$ratings)
  }
  log.likelihood.k[j] = ll.tmp
}

best.ind = which.max(log.likelihood.k)
best.k.base.elo = ls.k[best.ind]
best.k.base.elo

# Create heatmaps (using two different methods)

acast(log.likelihood.hfa, k0~lambda, value.var="ll")
m = as.matrix(acast(log.likelihood.hfa, k0~lambda, value.var="ll"))

image(m, xaxt="n", yaxt="n")
axis(1, at=seq(0,1,length.out=ncol(m)), 
     labels= paste("lambda", colnames(m)), las= 2 )
axis(2, at=seq(0,1,length.out=nrow(m)), 
     labels= paste("k", rownames(m)), las= 2)

im.matrix <- matrix(log.likelihood.hfa$ll, nrow=length(ls.k), 
                    ncol=length(ls.lambda), byrow=FALSE)
image(x=unique(log.likelihood.hfa$k0), y=unique(log.likelihood.hfa$lambda),
      z=im.matrix, col=hcl.colors(300, "Spectral", rev = TRUE), 
      ylab=expression(lambda), xlab=expression(k[0]),
      main="Elo with Goal Diff Heatmap")

ll.hfa.truncated <- log.likelihood.hfa[log.likelihood.hfa$k0 < 10 & 
                                         log.likelihood.hfa$lambda < 1, ]
im.matrix <- matrix(ll.hfa.truncated$ll, nrow=length(ls.k[ls.k < 10]), 
                    ncol=length(ls.lambda[ls.lambda < 1]), byrow=FALSE)
image(x=unique(ll.hfa.truncated$k0), y=unique(ll.hfa.truncated$lambda),
      z=im.matrix, col=hcl.colors(300, "Spectral", rev = TRUE), 
      ylab=expression(lambda), xlab=expression(k[0]),
      main="Elo with Goal Diff Parameter Heatmap")


ratings.elo = elo.g(mls.g[mls.g$year==2002,], init=1500, k0=best.k, gamma=0)
ratings.elo = elo.g(mls.g[mls.g$year>2002 & mls.g$year <= 2017,], 
                    status=ratings.elo$ratings, k0=best.k, lambda=best.lambda)

##### Write the H&A four-step procedure

### Step 1: Split time periods
time.A <- 2002:2007
time.B <- 2008:2013
time.C <- 2014:2017

### Step 2: Fit initial elo ratings over time period A 

# First use k0 = 30, lambda = 0 
ratings.elo.init <- elo(mls[mls.g$year == 2002,], init=1500, kfac=30)
ratings.elo.g.init <- elo.g(mls.g[mls.g$year == 2002,], init=1500, k0=30, lambda=0,
                         gamma=0)

# Then use optimal parameters to fit ratings
ratings.elo.A <- elo(mls[mls$year %in% time.A & mls$year > 2002,], init=1500, 
                        status=ratings.elo.init$ratings, kfac=best.k.base.elo)
ratings.elo.g.A <- elo.g(mls.g[mls.g$year %in% time.A & mls.g$year > 2002,],
                       status=ratings.elo.g.init$ratings, k0=best.k, 
                       lambda=best.lambda, gamma=0)


### Step 3: Fit ordered logit model on time period B, continue to update elo
###         ratings

# Create df for this time period
time.B.df <- mls[mls$year %in% time.B,]
time.B.df.g <- mls.g[mls.g$year %in% time.B,]

# Generate ratings for this time period
ratings.elo.B <- elo(time.B.df, init=1500,
                         status=ratings.elo.A$ratings, kfac=best.k.base.elo)
ratings.elo.g.B <- elo.g(time.B.df.g,
                       status=ratings.elo.g.A$ratings, k0=best.k, 
                       lambda=best.lambda, gamma=0)

# Add ratings at the time to time period df
time.B.df$home.rating <- -1e6
time.B.df$away.rating <- -1e6
for (i in 1:dim(time.B.df.g)[1]) {
  time.B.df[i, 'home.rating'] <- 
    ratings.elo.B$ratings[which(ratings.elo.B$ratings$Player == time.B.df[i, 'home']),"Rating"]
  time.B.df[i, 'away.rating'] <- 
    ratings.elo.B$ratings[which(ratings.elo.B$ratings$Player == time.B.df[i, 'away']),"Rating"]
}

time.B.df.g$home.rating <- -1e6
time.B.df.g$away.rating <- -1e6
for (i in 1:dim(time.B.df.g)[1]) {
  time.B.df.g[i, 'home.rating'] <- 
    ratings.elo.g.B$history[time.B.df.g[i, 'home'], 
                          as.character(time.B.df.g[i, 'year'])]
  time.B.df.g[i, 'away.rating'] <- 
    ratings.elo.g.B$history[time.B.df.g[i, 'away'], 
                          as.character(time.B.df.g[i, 'year'])]
}

# Set gamma, our HFA parameter
gamma <- 0

# Create rating.diff column 
time.B.df$rating.diff <- time.B.df$home.rating - time.B.df$away.rating 
time.B.df.g$rating.diff <- time.B.df.g$home.rating + gamma - time.B.df.g$away.rating 

# Fit ordered logit model
elo.base.result.fit <- polr(as.factor(result) ~ rating.diff, data=time.B.df)
summary(elo.base.result.fit)

elo.g.result.fit <- polr(as.factor(result) ~ rating.diff, data=time.B.df.g)
summary(elo.g.result.fit)

### Step 4: Predict match results for time period C, calculate loss

# Create df for this time period
time.C.df <- mls[mls$year %in% time.C,]
time.C.df.g <- mls.g[mls.g$year %in% time.C,]

# Generate ratings for this time period
ratings.elo.C <- elo(time.C.df, init=1500,
                         status=ratings.elo.B$ratings, kfac=best.k.base.elo)

ratings.elo.g.C <- elo.g(time.C.df.g ,
                       status=ratings.elo.g.B$ratings, k0=best.k, 
                       lambda=best.lambda, gamma=0)

# Add ratings at the time to time period df
time.C.df$home.rating <- -1e6
time.C.df$away.rating <- -1e6
for (i in 1:dim(time.C.df.g)[1]) {
  time.C.df[i, 'home.rating'] <- 
    ratings.elo.C$ratings[which(ratings.elo.C$ratings$Player == time.C.df[i, 'home']),"Rating"]
  time.C.df[i, 'away.rating'] <- 
    ratings.elo.C$ratings[which(ratings.elo.C$ratings$Player == time.C.df[i, 'away']),"Rating"]
}

time.C.df.g$home.rating <- -1e6
time.C.df.g$away.rating <- -1e6
for (i in 1:dim(time.C.df.g)[1]) {
  time.C.df.g[i, 'home.rating'] <- 
    ratings.elo.g.C$history[time.C.df.g[i, 'home'], 
                          as.character(time.C.df.g[i, 'year'])]
  time.C.df.g[i, 'away.rating'] <- 
    ratings.elo.g.C$history[time.C.df.g[i, 'away'], 
                          as.character(time.C.df.g[i, 'year'])]
}

# Set gamma, our HFA parameter
gamma <- 0

# Create rating.diff column 
time.C.df$rating.diff <- time.C.df$home.rating - time.C.df$away.rating 
time.C.df.g$rating.diff <- time.C.df.g$home.rating + gamma - time.C.df.g$away.rating 

# Predict match results
time.C.pred <- predict(elo.base.result.fit, newdata=time.C.df)
time.C.g.pred <- predict(elo.g.result.fit, newdata=time.C.df.g)

# Define function to get model's predicted probability for the true outcome
pred.prob <- function(df, coef, zeta) {
  mini.fn <- function(row) {
    # print(typeof(row[['rating.diff']]))
    # print(row[['rating.diff']])
    p.loss <- (1 + exp((-1)*(zeta[[1]] - as.double(row[['rating.diff']]) * 
                         coef[[1]])))**(-1)
    p.tie <- (1 + exp((-1)*(zeta[[2]] - as.double(row[['rating.diff']]) * 
                              coef[[1]])))**(-1) - p.loss
    p.win <- 1 - (1 + exp((-1)*(zeta[[2]] - as.double(row[['rating.diff']]) * 
                            coef[[1]])))**(-1)
    if (row['result'] <= 0 ) {
      return(p.loss)
    } 
    if (row['result'] <= 0.5) {
      return(p.tie)
    }
    return(p.win)
  }
  return(apply(df, 1, mini.fn))
}

# MIGHT STILL BE A PROBLEM WITH NA VALUES — SAM
# I AM NOT HAVING THIS SAME PROBLEM — DANIEL 
pred.prob(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)
pred.prob(time.C.df.g, elo.g.result.fit$coefficients, elo.g.result.fit$zeta)

# Define loss functions
quad.loss <- function(y, y.pred) {
  return (mean((y - y.pred)**2))
}
info.loss <- function(df, coef, zeta) {
   return(mean(-1 * log2(pred.prob(df, coef, zeta))))
}

# Calculate loss
quad.loss(time.C.df$result, as.double(time.C.pred) / 2 - 0.5)
quad.loss(time.C.df.g$result, as.double(time.C.g.pred) / 2 - 0.5)
info.loss(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)
info.loss(time.C.df.g, elo.g.result.fit$coefficients, elo.g.result.fit$zeta)


##### Recreate Fig. 3 for our ordered logit model
dummy.diff <- -400:400
dummy.loss.df <- data.frame('result'=as.factor(rep(0, length(dummy.diff))), 
                            'rating.diff'=dummy.diff)
dummy.loss.df$prob <- pred.prob(dummy.loss.df, elo.g.result.fit$coefficients, 
                                elo.g.result.fit$zeta)
dummy.draw.df <- data.frame('result'=as.factor(rep(0.5, length(dummy.diff))), 
                            'rating.diff'=dummy.diff)
dummy.draw.df$prob <- pred.prob(dummy.draw.df, elo.g.result.fit$coefficients, 
                                elo.g.result.fit$zeta)
dummy.win.df <- data.frame('result'=as.factor(rep(1, length(dummy.diff))), 
                           'rating.diff'=dummy.diff)
dummy.win.df$prob <- pred.prob(dummy.win.df, elo.g.result.fit$coefficients, 
                               elo.g.result.fit$zeta)

# png('pred-prob_vs_rating-diff.png', width=600, height=500)
plot(dummy.loss.df$prob ~ dummy.loss.df$rating.diff, col='red', type='l', 
     main="Ordered Logit Predicted Probabilities vs. Rating Difference", 
     ylab="Predicted Probability",
     xlab="Rating Difference")
lines(dummy.draw.df$prob ~ dummy.draw.df$rating.diff, col='purple')
lines(dummy.win.df$prob ~ dummy.win.df$rating.diff, col='blue')
legend(200,0.5, c('Home Loss', 'Draw', 'Home Win'), 
       col=c('red', 'purple', 'blue'), lty=1)
# dev.off()

##### Find our estimated 'HFA' effect as H&A did
approx.effect <- dummy.loss.df[which.min(abs(dummy.loss.df$prob - 
                                               dummy.win.df$prob)), 
                               'rating.diff']
range <- ((approx.effect-5)*100):((approx.effect+5)*100) / 100
prob.diffs <- rep(-1e6, length(range))
for (i in 1:length(range)) {
  df.l <- data.frame('rating.diff'=range[i], 'result'=0)
  df.l$p <- pred.prob(df.l, elo.g.result.fit$coefficients, elo.g.result.fit$zeta)
  df.w <- data.frame('rating.diff'=range[i], 'result'=1)
  df.w$p <- pred.prob(df.w, elo.g.result.fit$coefficients, elo.g.result.fit$zeta)
  
  prob.diffs[i] <- abs(df.l$p - df.w$p)
}
hfa.est <- range[which.min(prob.diffs)]







