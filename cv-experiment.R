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

##### Write elo.g function for goal difference 

mls.g <- mls.regular.season[mls.regular.season$year %in% 1996:2022,
                            c('year', 'date_corrected', 'home', 'away', 
                              'result', 'score_difference')]
colnames(mls.g) <- c('year', 'date', 'home', 'away', 'result', 'gd')


We <- function(r1, r2, gamma=0) {
  return ((1 + 10**(-(r1-r2+gamma)/400))**(-1))
}

elo.g <- function(df, init=1500, status=NULL, k0=30, lambda=0, gamma=0) {
  # Set up matrix of ratings, with one for each date
  teams <- unique(mls.regular.season$home)
  dates <- unique(df$date)
  history <- matrix(data=NA, nrow=length(teams), ncol=length(dates)+1)
  dimnames(history) <- list(teams, c('init', as.character(dates)))
  
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
  
  for (row in 1:dim(df)[1]) {
    temp_ratings <- ratings
    temp_ratings[df[row,'home']] <- temp_ratings[df[row,'home']] + 
      k0*(1 + abs(df[i, 'gd']))**lambda * 
      (df[row,'result'] - We(ratings[df[row,'home']], ratings[df[row,'away']],
                             gamma=gamma))
    temp_ratings[df[row,'away']] <- temp_ratings[df[row,'away']] + 
      k0*(1 + abs(df[i, 'gd']))**lambda * 
      ((1 - df[row,'result']) - We(ratings[df[row,'away']], ratings[df[row,'home']],
                                   gamma=gamma))
    
    ratings <- temp_ratings
    history[df[row,'home'], as.character(df[row, 'date'])] <- ratings[df[row,'home']]
    history[df[row,'away'], as.character(df[row, 'date'])] <- ratings[df[row,'away']]
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

##### Cross validate k0 for base elo model
ls.lambda <- 0
# ls.lambda <- 1:15 * 0.16
ls.k <- 1:15 * 40/15
log.likelihood.hfa <- expand.grid(ls.k, ls.lambda)
colnames(log.likelihood.hfa) <- c("k0", "lambda")
log.likelihood.hfa$quad.loss <- rep(-1e6, dim(log.likelihood.hfa)[1])
log.likelihood.hfa$info.loss <- rep(-1e6, dim(log.likelihood.hfa)[1])
kfac.init <- 30

for (j in 1:dim(log.likelihood.hfa)[1]) {
  k0 <- log.likelihood.hfa[j, 'k0']
  lambda <- log.likelihood.hfa[j, 'lambda']
  
  ### Step 1: Split time periods
  time.A <- 2001:2005
  time.B <- 2006:2010
  time.C <- 2011:2015
  
  ### Step 2: Fit initial elo ratings over time period A 
  
  # First use k0 = 30, lambda = 0 
  ratings.elo.init <- elo.g(mls.g[mls.g$year == 2001,], init=1500, k0=30)
  
  # Then use optimal parameters to fit ratings
  ratings.elo.A <- elo.g(mls.g[mls.g$year %in% time.A & mls$year > 2001,], init=1500, 
                         status=ratings.elo.init$ratings, k0=k0,
                         lambda=lambda)
  
  ### Step 3: Fit ordered logit model on time period B, continue to update elo
  ###         ratings
  
  # Create df for this time period
  time.B.df <- mls.g[mls.g$year %in% time.B,]
  
  # Generate ratings for this time period
  ratings.elo.B <- elo.g(time.B.df.g, init=1500, status=ratings.elo.A$ratings, 
                         k0=k0, lambda=lambda)
  
  # Add ratings at the time to time period df
  time.B.df$home.rating <- -1e6
  time.B.df$away.rating <- -1e6
  for (i in 1:dim(time.B.df)[1]) {
    if (sum(as.Date(colnames(ratings.elo.B$history)[2:dim(ratings.elo.B$history)[2]], 
                    format="%Y-%m-%d") < time.B.df[i, 'date']) < 1) {
      time.B.df[i, 'home.rating'] <- ratings.elo.B$history[time.B.df[i,'home'],1]
      time.B.df[i, 'away.rating'] <- ratings.elo.B$history[time.B.df[i,'away'],1]
    } else {
      before.df <- ratings.elo.B$history[,as.Date(colnames(ratings.elo.B$history)[2:dim(ratings.elo.B$history)[2]], 
                                                  format="%Y-%m-%d") < time.B.df[i, 'date']]
      time.B.df[i, 'home.rating'] <- tail(na.omit(before.df[time.B.df[i,'home'],]), n=1)
      time.B.df[i, 'away.rating'] <- tail(na.omit(before.df[time.B.df[i,'away'],]), n=1)
    }
  }

  # Create rating.diff column 
  time.B.df$rating.diff <- time.B.df$home.rating + gamma - time.B.df$away.rating 
  
  # Fit ordered logit model
  elo.base.result.fit <- polr(as.factor(result) ~ rating.diff, data=time.B.df)

  ### Step 4: Predict match results for time period C, calculate loss
  
  # Create df for this time period
  time.C.df <- mls.g[mls.g$year %in% time.C,]
  
  # Generate ratings for this time period
  ratings.elo.C <- elo.g(time.C.df, init=1500, status=ratings.elo.B$ratings, 
                         k0=k0, lambda=lambda)
  
  # Add ratings at the time to time period df
  time.C.df$home.rating <- -1e6
  time.C.df$away.rating <- -1e6
  for (i in 1:dim(time.C.df)[1]) {
    if (sum(as.Date(colnames(ratings.elo.C$history)[2:dim(ratings.elo.C$history)[2]], 
                    format="%Y-%m-%d") < time.C.df[i, 'date']) < 1) {
      time.C.df[i, 'home.rating'] <- ratings.elo.B$history[time.C.df[i,'home'],1]
      time.C.df[i, 'away.rating'] <- ratings.elo.B$history[time.C.df[i,'away'],1]
    } else {
      before.df <- ratings.elo.C$history[,as.Date(colnames(ratings.elo.C$history)[2:dim(ratings.elo.C$history)[2]], 
                                                  format="%Y-%m-%d") < time.C.df[i, 'date']]
      time.C.df[i, 'home.rating'] <- tail(na.omit(before.df[time.C.df[i,'home'],]), n=1)
      time.C.df[i, 'away.rating'] <- tail(na.omit(before.df[time.C.df[i,'away'],]), n=1)
    }
  }
  
  # Create rating.diff column 
  time.C.df$rating.diff <- time.C.df$home.rating - time.C.df$away.rating 
  
  # Predict match results
  time.C.pred <- predict(elo.base.result.fit, newdata=time.C.df)
  
  # Define function to get model's predicted probability for the true outcome
  pred.prob <- function(df, coef, zeta, column = "rating.diff") {
    mini.fn <- function(row) {
      # print(typeof(row[['rating.diff']]))
      # print(row[['rating.diff']])
      p.loss <- (1 + exp((-1)*(zeta[[1]] - as.double(row[[column]]) * 
                                 coef[[1]])))**(-1)
      p.tie <- (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                coef[[1]])))**(-1) - p.loss
      p.win <- 1 - (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
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
  
  pred.prob.all <- function(df, coef, zeta, column = 'rating.diff') {
    res = data.frame(p.loss=numeric(0),p.tie=numeric(0),p.win=numeric(0))
    for (i in 1:dim(df)[1]) {
      row = df[i,]
      res[i, "p.loss"] <- (1 + exp((-1)*(zeta[[1]] - as.double(row[[column]]) * 
                                           coef[[1]])))**(-1)
      res[i, "p.tie"] <- (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                          coef[[1]])))**(-1) - res[i, "p.loss"]
      res[i, "p.win"] <- 1 - (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                              coef[[1]])))**(-1)
    }
    return(res)
  }
  
  # Define loss functions
  quad.loss <- function(y, y.pred) {
    N = length(y)
    true_outcomes = data.frame(loss=rep(0, N),tie=rep(0, N),win=rep(0, N))
    for (i in 1:N) {
      if (y[i]==0) {
        true_outcomes[i, "loss"] = 1
      }
      else if (y[i]==0.5) {
        true_outcomes[i, "tie"] = 1
      }
      else if (y[i]==1) {
        true_outcomes[i, "win"] = 1
      }
    }
    loss_l = (true_outcomes[,"loss"] - y.pred[, "p.loss"])**2
    tie_l = (true_outcomes[, "tie"] - y.pred[, "p.tie"])**2
    win_l = (true_outcomes[, "win"] - y.pred[, "p.win"])**2
    return(sum(loss_l, tie_l, win_l, na.rm = TRUE)/N)
  }

  info.loss <- function(df, coef, zeta, column = "rating.diff") {
    return(mean(-1 * log2(pred.prob(df, coef, zeta, column = column)), na.rm= TRUE))
  }
  
  elo.b.probs = pred.prob.all(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)

  log.likelihood.hfa[j, 'quad.loss'] <- quad.loss(time.C.df$result, elo.b.probs)
  log.likelihood.hfa[j, 'info.loss'] <- info.loss(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)
  
  print(log.likelihood.hfa[j,])
}

best.k0.base <- log.likelihood.hfa[which.min(log.likelihood.hfa$info.loss), 
                                   'k0']
best.k0.base <- 11/15 * 40


##### Cross validate k0 and lambda for goal difference elo model
ls.lambda <- 1:8 * 2.5/8
ls.k <- 1:8 * 5
log.likelihood.hfa <- expand.grid(ls.k, ls.lambda)
colnames(log.likelihood.hfa) <- c("k0", "lambda")
log.likelihood.hfa$quad.loss <- rep(-1e6, dim(log.likelihood.hfa)[1])
log.likelihood.hfa$info.loss <- rep(-1e6, dim(log.likelihood.hfa)[1])
kfac.init <- 30

for (j in 1:dim(log.likelihood.hfa)[1]) {
  k0 <- log.likelihood.hfa[j, 'k0']
  lambda <- log.likelihood.hfa[j, 'lambda']
  
  ### Step 1: Split time periods
  time.A <- 2001:2005
  time.B <- 2006:2010
  time.C <- 2011:2015
  
  ### Step 2: Fit initial elo ratings over time period A 
  
  # First use k0 = 30, lambda = 0 
  ratings.elo.init <- elo.g(mls.g[mls.g$year == 2001,], init=1500, k0=30, 
                            lambda=lambda)
  
  # Then use optimal parameters to fit ratings
  ratings.elo.A <- elo.g(mls.g[mls.g$year %in% time.A & mls$year > 2001,], init=1500, 
                         status=ratings.elo.init$ratings, k0=k0,
                         lambda=lambda)
  
  ### Step 3: Fit ordered logit model on time period B, continue to update elo
  ###         ratings
  
  # Create df for this time period
  time.B.df <- mls.g[mls.g$year %in% time.B,]
  
  # Generate ratings for this time period
  ratings.elo.B <- elo.g(time.B.df.g, init=1500, status=ratings.elo.A$ratings, 
                         k0=k0, lambda=lambda)
  
  # Add ratings at the time to time period df
  time.B.df$home.rating <- -1e6
  time.B.df$away.rating <- -1e6
  for (i in 1:dim(time.B.df)[1]) {
    if (sum(as.Date(colnames(ratings.elo.B$history)[2:dim(ratings.elo.B$history)[2]], 
                    format="%Y-%m-%d") < time.B.df[i, 'date']) < 1) {
      time.B.df[i, 'home.rating'] <- ratings.elo.B$history[time.B.df[i,'home'],1]
      time.B.df[i, 'away.rating'] <- ratings.elo.B$history[time.B.df[i,'away'],1]
    } else {
      before.df <- ratings.elo.B$history[,as.Date(colnames(ratings.elo.B$history)[2:dim(ratings.elo.B$history)[2]], 
                                                  format="%Y-%m-%d") < time.B.df[i, 'date']]
      time.B.df[i, 'home.rating'] <- tail(na.omit(before.df[time.B.df[i,'home'],]), n=1)
      time.B.df[i, 'away.rating'] <- tail(na.omit(before.df[time.B.df[i,'away'],]), n=1)
    }
  }
  
  # Create rating.diff column 
  time.B.df$rating.diff <- time.B.df$home.rating + gamma - time.B.df$away.rating 
  
  # Fit ordered logit model
  elo.base.result.fit <- polr(as.factor(result) ~ rating.diff, data=time.B.df)
  
  ### Step 4: Predict match results for time period C, calculate loss
  
  # Create df for this time period
  time.C.df <- mls.g[mls.g$year %in% time.C,]
  
  # Generate ratings for this time period
  ratings.elo.C <- elo.g(time.C.df, init=1500, status=ratings.elo.B$ratings, 
                         k0=k0, lambda=lambda)
  
  # Add ratings at the time to time period df
  time.C.df$home.rating <- -1e6
  time.C.df$away.rating <- -1e6
  for (i in 1:dim(time.C.df)[1]) {
    if (sum(as.Date(colnames(ratings.elo.C$history)[2:dim(ratings.elo.C$history)[2]], 
                    format="%Y-%m-%d") < time.C.df[i, 'date']) < 1) {
      time.C.df[i, 'home.rating'] <- ratings.elo.B$history[time.C.df[i,'home'],1]
      time.C.df[i, 'away.rating'] <- ratings.elo.B$history[time.C.df[i,'away'],1]
    } else {
      before.df <- ratings.elo.C$history[,as.Date(colnames(ratings.elo.C$history)[2:dim(ratings.elo.C$history)[2]], 
                                                  format="%Y-%m-%d") < time.C.df[i, 'date']]
      time.C.df[i, 'home.rating'] <- tail(na.omit(before.df[time.C.df[i,'home'],]), n=1)
      time.C.df[i, 'away.rating'] <- tail(na.omit(before.df[time.C.df[i,'away'],]), n=1)
    }
  }
  
  # Create rating.diff column 
  time.C.df$rating.diff <- time.C.df$home.rating - time.C.df$away.rating 
  
  # Predict match results
  time.C.pred <- predict(elo.base.result.fit, newdata=time.C.df)
  
  # Define function to get model's predicted probability for the true outcome
  pred.prob <- function(df, coef, zeta, column = "rating.diff") {
    mini.fn <- function(row) {
      # print(typeof(row[['rating.diff']]))
      # print(row[['rating.diff']])
      p.loss <- (1 + exp((-1)*(zeta[[1]] - as.double(row[[column]]) * 
                                 coef[[1]])))**(-1)
      p.tie <- (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                coef[[1]])))**(-1) - p.loss
      p.win <- 1 - (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
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
  
  pred.prob.all <- function(df, coef, zeta, column = 'rating.diff') {
    res = data.frame(p.loss=numeric(0),p.tie=numeric(0),p.win=numeric(0))
    for (i in 1:dim(df)[1]) {
      row = df[i,]
      res[i, "p.loss"] <- (1 + exp((-1)*(zeta[[1]] - as.double(row[[column]]) * 
                                           coef[[1]])))**(-1)
      res[i, "p.tie"] <- (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                          coef[[1]])))**(-1) - res[i, "p.loss"]
      res[i, "p.win"] <- 1 - (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                              coef[[1]])))**(-1)
    }
    return(res)
  }
  
  # Define loss functions
  quad.loss <- function(y, y.pred) {
    N = length(y)
    true_outcomes = data.frame(loss=rep(0, N),tie=rep(0, N),win=rep(0, N))
    for (i in 1:N) {
      if (y[i]==0) {
        true_outcomes[i, "loss"] = 1
      }
      else if (y[i]==0.5) {
        true_outcomes[i, "tie"] = 1
      }
      else if (y[i]==1) {
        true_outcomes[i, "win"] = 1
      }
    }
    loss_l = (true_outcomes[,"loss"] - y.pred[, "p.loss"])**2
    tie_l = (true_outcomes[, "tie"] - y.pred[, "p.tie"])**2
    win_l = (true_outcomes[, "win"] - y.pred[, "p.win"])**2
    return(sum(loss_l, tie_l, win_l, na.rm = TRUE)/N)
  }
  
  info.loss <- function(df, coef, zeta, column = "rating.diff") {
    return(mean(-1 * log2(pred.prob(df, coef, zeta, column = column)), na.rm= TRUE))
  }
  
  elo.b.probs = pred.prob.all(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)
  
  log.likelihood.hfa[j, 'quad.loss'] <- quad.loss(time.C.df$result, elo.b.probs)
  log.likelihood.hfa[j, 'info.loss'] <- info.loss(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)
  
  print(log.likelihood.hfa[j,])
}

best.ind <- which.min(log.likelihood.hfa$info.loss)
best.k0.gd <- log.likelihood.hfa[best.ind,'k0']
best.lambda.gd <- log.likelihood.hfa[best.ind,'lambda']
best.k0.gd <- 10 
best.lambda.gd <- 1.875



##### Cross validate for glicko
predict.fn.glicko <- function(t1, t2, ratings, gamma = 0){
  rd = ratings[t1, "stderr"]^2+ratings[t2,"stderr"]^2
  g = 1/sqrt(1+0.00001007252*rd)
  E = 1/(1+10^(-g * (ratings[t1,"est"]-ratings[t2,"est"] + gamma)/400))
  return (E)
}

ls.omega <- 1:100
log.likelihood.hfa <- expand.grid(ls.omega)
colnames(log.likelihood.hfa) <- c("omega")
log.likelihood.hfa$quad.loss <- rep(-1e6, dim(log.likelihood.hfa)[1])
log.likelihood.hfa$info.loss <- rep(-1e6, dim(log.likelihood.hfa)[1])

elo.columns <- c('year', 'home', 'away', 'result')

for (j in 1:dim(log.likelihood.hfa)[1]) {
  omega <- log.likelihood.hfa[j, 'omega']
  
  ### Step 1: Split time periods
  time.A <- 2001:2005
  time.B <- 2006:2010
  time.C <- 2011:2015
  
  ### Step 2: Fit initial elo ratings over time period A 
  
  # Fit ratings
  ratings.glicko.A <- glicko(mls.g[mls.g$year %in% time.A, elo.columns],
                             init=c(1500,300), cval=omega)
  
  ### Step 3: Fit ordered logit model on time period B, continue to update elo
  ###         ratings
  
  # Create df for this time period
  time.B.df <- mls.g[mls.g$year %in% time.B,]
  
  # Generate ratings for this time period
  ratings.glicko.B <- glicko(time.B.df[, elo.columns],
                             status=ratings.glicko.A$ratings, cval=omega, 
                             history = TRUE)
  
  # Add ratings at the time to time period df
  ratings.glicko.B.history <- ratings.glicko.B$history[,,'Rating']
  colnames(ratings.glicko.B.history) <- time.B
  
  time.B.df$glicko.home.rating <- -1e6
  time.B.df$glicko.away.rating <- -1e6
  for (i in 1:dim(time.B.df)[1]) {
    time.B.df[i, 'glicko.home.rating'] <- 
      ratings.glicko.B.history[time.B.df[i, 'home'], 
                               as.character(time.B.df[i, 'year'])]
    time.B.df[i, 'glicko.away.rating'] <- 
      ratings.glicko.B.history[time.B.df[i, 'away'], 
                               as.character(time.B.df[i, 'year'])]
  }
  
  # Create rating.diff column 
  time.B.df$glicko.rating.diff <- time.B.df$glicko.home.rating + best.hfa - time.B.df$glicko.away.rating 
  
  # Fit ordered logit model
  glicko.base.result.fit <- polr(as.factor(result) ~ glicko.rating.diff, data=time.B.df)
  
  ### Step 4: Predict match results for time period C, calculate loss
  
  # Create df for this time period
  time.C.df <- mls.g[mls.g$year %in% time.C,]
  
  # Generate ratings for this time period
  ratings.glicko.C <- glicko(time.C.df[, elo.columns],
                             status=ratings.glicko.B$ratings, cval=omega, 
                             history = TRUE)
  
  # Add ratings at the time to time period df
  ratings.glicko.C.history <- ratings.glicko.C$history[,,'Rating']
  colnames(ratings.glicko.C.history) <- time.C
  
  time.C.df$glicko.home.rating <- -1e6
  time.C.df$glicko.away.rating <- -1e6
  for (i in 1:dim(time.C.df)[1]) {
    time.C.df[i, 'glicko.home.rating'] <- 
      ratings.glicko.C.history[time.C.df[i, 'home'], 
                               as.character(time.C.df[i, 'year'])]
    time.C.df[i, 'glicko.away.rating'] <- 
      ratings.glicko.C.history[time.C.df[i, 'away'], 
                               as.character(time.C.df[i, 'year'])]
  }
  
  # Create rating.diff column 
  time.C.df$glicko.rating.diff <- time.C.df$glicko.home.rating - time.C.df$glicko.away.rating 
  
  # Predict match results
  time.C.glicko.pred <- predict(glicko.base.result.fit, newdata=time.C.df)
  
  # Define function to get model's predicted probability for the true outcome
  pred.prob <- function(df, coef, zeta, column = "rating.diff") {
    mini.fn <- function(row) {
      # print(typeof(row[['rating.diff']]))
      # print(row[['rating.diff']])
      p.loss <- (1 + exp((-1)*(zeta[[1]] - as.double(row[[column]]) * 
                                 coef[[1]])))**(-1)
      p.tie <- (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                coef[[1]])))**(-1) - p.loss
      p.win <- 1 - (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
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
  
  pred.prob.all <- function(df, coef, zeta, column = 'rating.diff') {
    res = data.frame(p.loss=numeric(0),p.tie=numeric(0),p.win=numeric(0))
    for (i in 1:dim(df)[1]) {
      row = df[i,]
      res[i, "p.loss"] <- (1 + exp((-1)*(zeta[[1]] - as.double(row[[column]]) * 
                                           coef[[1]])))**(-1)
      res[i, "p.tie"] <- (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                          coef[[1]])))**(-1) - res[i, "p.loss"]
      res[i, "p.win"] <- 1 - (1 + exp((-1)*(zeta[[2]] - as.double(row[[column]]) * 
                                              coef[[1]])))**(-1)
    }
    return(res)
  }
  
  # Define loss functions
  quad.loss <- function(y, y.pred) {
    N = length(y)
    true_outcomes = data.frame(loss=rep(0, N),tie=rep(0, N),win=rep(0, N))
    for (i in 1:N) {
      if (y[i]==0) {
        true_outcomes[i, "loss"] = 1
      }
      else if (y[i]==0.5) {
        true_outcomes[i, "tie"] = 1
      }
      else if (y[i]==1) {
        true_outcomes[i, "win"] = 1
      }
    }
    loss_l = (true_outcomes[,"loss"] - y.pred[, "p.loss"])**2
    tie_l = (true_outcomes[, "tie"] - y.pred[, "p.tie"])**2
    win_l = (true_outcomes[, "win"] - y.pred[, "p.win"])**2
    return(sum(loss_l, tie_l, win_l, na.rm = TRUE)/N)
  }
  
  info.loss <- function(df, coef, zeta, column = "rating.diff") {
    return(mean(-1 * log2(pred.prob(df, coef, zeta, column = column)), na.rm= TRUE))
  }
  
  glicko.probs = pred.prob.all(time.C.df, glicko.base.result.fit$coefficients, glicko.base.result.fit$zeta, column = "glicko.rating.diff")
  
  log.likelihood.hfa[j, 'quad.loss'] <- quad.loss(time.C.df$result, glicko.probs)
  log.likelihood.hfa[j, 'info.loss'] <- info.loss(time.C.df, glicko.base.result.fit$coefficients, 
                                                  glicko.base.result.fit$zeta, column="glicko.rating.diff")
  
  print(log.likelihood.hfa[j,])
}

best.ind <- which.min(log.likelihood.hfa$info.loss)
best.omega <- log.likelihood.hfa[best.ind, 'omega']
best.omega <- 60 




























