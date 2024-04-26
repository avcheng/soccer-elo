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

##### Set k0 and lambda for goal difference elo model
best.k <- 10
best.lambda <- 1.875

##### Set K for base elo model
best.k.base.elo <- 11/15 * 40

##### Set omega for Glicko model
best.omega <- 60

##### Write the H&A four-step procedure

### Step 1: Split time periods
time.A <- 2001:2005
time.B <- 2006:2010
time.C <- 2011:2015

### Step 2: Fit initial elo ratings over time period A 

# First use k0 = 30, lambda = 0 
ratings.elo.init <- elo.g(mls.g[mls.g$year == 2001,], init=1500, k0=30)
ratings.elo.g.init <- elo.g(mls.g[mls.g$year == 2001,], init=1500, k0=30, 
                            lambda=best.lambda)

# Then use optimal parameters to fit ratings
ratings.elo.A <- elo.g(mls.g[mls.g$year %in% time.A & mls$year > 2001,], init=1500, 
                     status=ratings.elo.init$ratings, k0=best.k.base.elo)
ratings.elo.g.A <- elo.g(mls.g[mls.g$year %in% time.A & mls.g$year > 2001,],
                         status=ratings.elo.g.init$ratings, k0=best.k, 
                         lambda=best.lambda, gamma=0)
ratings.glicko.A <- glicko(mls[mls$year %in% time.A,],
                           init=c(1500,300), cval=best.omega, gamma=best.hfa)

### Step 3: Fit ordered logit model on time period B, continue to update elo
###         ratings

# Create df for this time period
time.B.df <- mls.g[mls.g$year %in% time.B,]
time.B.df.g <- mls.g[mls.g$year %in% time.B,]

# Generate ratings for this time period
ratings.elo.B <- elo.g(time.B.df.g, init=1500, status=ratings.elo.A$ratings, 
                     k0=best.k.base.elo)
ratings.elo.g.B <- elo.g(time.B.df.g,
                         status=ratings.elo.g.A$ratings, k0=best.k, 
                         lambda=best.lambda, gamma=0)
ratings.glicko.B <- glicko(time.B.df[,c('year', 'home', 'away', 'result')],
                           status=ratings.glicko.A$ratings, cval=best.omega, 
                           gamma=best.hfa, history = TRUE)

# Add ratings at the time to time period df
ratings.glicko.B.history <- ratings.glicko.B$history[,,'Rating']
colnames(ratings.glicko.B.history) <- time.B

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

time.B.df.g$home.rating <- -1e6
time.B.df.g$away.rating <- -1e6
for (i in 1:dim(time.B.df.g)[1]) {
  if (sum(as.Date(colnames(ratings.elo.g.B$history)[2:dim(ratings.elo.g.B$history)[2]], 
                  format="%Y-%m-%d") < time.B.df.g[i, 'date']) < 1) {
    time.B.df.g[i, 'home.rating'] <- ratings.elo.g.B$history[time.B.df.g[i,'home'],1]
    time.B.df.g[i, 'away.rating'] <- ratings.elo.g.B$history[time.B.df.g[i,'away'],1]
  } else {
    before.df <- ratings.elo.g.B$history[,as.Date(colnames(ratings.elo.g.B$history)[2:dim(ratings.elo.g.B$history)[2]], 
                                                format="%Y-%m-%d") < time.B.df.g[i, 'date']]
    time.B.df.g[i, 'home.rating'] <- tail(na.omit(before.df[time.B.df.g[i,'home'],]), n=1)
    time.B.df.g[i, 'away.rating'] <- tail(na.omit(before.df[time.B.df.g[i,'away'],]), n=1)
  }
}


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

# Set gamma, our HFA parameter
gamma <- 0

# Create rating.diff column 
time.B.df$rating.diff <- time.B.df$home.rating + gamma - time.B.df$away.rating 
time.B.df.g$rating.diff <- time.B.df.g$home.rating + gamma - time.B.df.g$away.rating 
time.B.df$glicko.rating.diff <- time.B.df$glicko.home.rating + best.hfa - time.B.df$glicko.away.rating 

# Fit ordered logit model
elo.base.result.fit <- polr(as.factor(result) ~ rating.diff, data=time.B.df)
summary(elo.base.result.fit)

elo.g.result.fit <- polr(as.factor(result) ~ rating.diff, data=time.B.df.g)
summary(elo.g.result.fit)

glicko.base.result.fit <- polr(as.factor(result) ~ glicko.rating.diff, data=time.B.df)
summary(glicko.base.result.fit)

### Step 4: Predict match results for time period C, calculate loss

# Create df for this time period
time.C.df <- mls.g[mls.g$year %in% time.C,]
time.C.df.g <- mls.g[mls.g$year %in% time.C,]

# Generate ratings for this time period
ratings.elo.C <- elo.g(time.C.df, init=1500, status=ratings.elo.B$ratings, 
                       k0=best.k.base.elo)

ratings.elo.g.C <- elo.g(time.C.df.g ,
                         status=ratings.elo.g.B$ratings, k0=best.k, 
                         lambda=best.lambda, gamma=0)

ratings.glicko.C <- glicko(time.C.df[,c('year', 'home', 'away', 'result')],
                           status=ratings.glicko.B$ratings, cval=best.omega, 
                           gamma=best.hfa, history = TRUE)


# Add ratings at the time to time period df
ratings.glicko.C.history <- ratings.glicko.C$history[,,'Rating']
colnames(ratings.glicko.C.history) <- time.C

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

time.C.df.g$home.rating <- -1e6
time.C.df.g$away.rating <- -1e6
for (i in 1:dim(time.B.df.g)[1]) {
  if (sum(as.Date(colnames(ratings.elo.g.C$history)[2:dim(ratings.elo.g.C$history)[2]], 
                  format="%Y-%m-%d") < time.C.df.g[i, 'date']) < 1) {
    time.C.df.g[i, 'home.rating'] <- ratings.elo.g.C$history[time.C.df.g[i,'home'],1]
    time.C.df.g[i, 'away.rating'] <- ratings.elo.g.C$history[time.C.df.g[i,'away'],1]
  } else {
    before.df <- ratings.elo.g.C$history[,as.Date(colnames(ratings.elo.g.C$history)[2:dim(ratings.elo.g.C$history)[2]], 
                                                  format="%Y-%m-%d") < time.C.df.g[i, 'date']]
    time.C.df.g[i, 'home.rating'] <- tail(na.omit(before.df[time.C.df.g[i,'home'],]), n=1)
    time.C.df.g[i, 'away.rating'] <- tail(na.omit(before.df[time.C.df.g[i,'away'],]), n=1)
  }
}


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

# Set gamma, our HFA parameter
gamma <- 0

# Create rating.diff column 
time.C.df$rating.diff <- time.C.df$home.rating - time.C.df$away.rating 
time.C.df.g$rating.diff <- time.C.df.g$home.rating + gamma - time.C.df.g$away.rating 
time.C.df$glicko.rating.diff <- time.C.df$glicko.home.rating - time.C.df$glicko.away.rating 

# Predict match results
time.C.pred <- predict(elo.base.result.fit, newdata=time.C.df)
time.C.g.pred <- predict(elo.g.result.fit, newdata=time.C.df.g)
time.C.glicko.pred <- predict(glicko.base.result.fit, newdata=time.C.df)

# Predict match results for AVG and MAX methods
odds <- read.csv('data/mls_closing_odds.csv')

odds$year <- format(as.Date(odds$match_date), "%Y")

odds[odds$home_team == 'Houston Dynamo', 'home_team'] <- "Houston Dynamo FC"
odds[odds$home_team == 'Columbus Crew', 'home_team'] <- "Columbus Crew SC"
odds[odds$home_team == 'DC United', 'home_team'] <- "D.C. United"
odds[odds$home_team == 'Los Angeles Galaxy', 'home_team'] <- "LA Galaxy"
odds[odds$home_team == 'New England Revoluti', 'home_team'] <- 
  "New England Revolution"
odds[odds$home_team == 'Chicago Fire', 'home_team'] <- "Chicago Fire FC"
odds[odds$home_team == 'Seattle Sounders', 'home_team'] <- "Seattle Sounders FC"
odds[odds$home_team == 'Montreal Impact', 'home_team'] <- "CF Montréal"
odds[odds$home_team == 'Orlando City', 'home_team'] <- "Orlando City SC"
odds[odds$home_team == 'New York City', 'home_team'] <- "New York City FC"

odds[odds$away_team == 'Houston Dynamo', 'away_team'] <- "Houston Dynamo FC"
odds[odds$away_team == 'Columbus Crew', 'away_team'] <- "Columbus Crew SC"
odds[odds$away_team == 'DC United', 'away_team'] <- "D.C. United"
odds[odds$away_team == 'Los Angeles Galaxy', 'away_team'] <- "LA Galaxy"
odds[odds$away_team == 'New England Revoluti', 'away_team'] <- 
  "New England Revolution"
odds[odds$away_team == 'Chicago Fire', 'away_team'] <- "Chicago Fire FC"
odds[odds$away_team == 'Seattle Sounders', 'away_team'] <- "Seattle Sounders FC"
odds[odds$away_team == 'Montreal Impact', 'away_team'] <- "CF Montréal"
odds[odds$away_team == 'Orlando City', 'away_team'] <- "Orlando City SC"
odds[odds$away_team == 'New York City', 'away_team'] <- "New York City FC"


betting.odds.pred <- data.frame('avg.pred'=rep(-1e6, length(time.C.g.pred)),
                                'avg.prob'=rep(-1e6, length(time.C.g.pred)),
                                'avg.p.loss'=rep(-1e6, length(time.C.g.pred)),
                                'avg.p.tie'=rep(-1e6, length(time.C.g.pred)),
                                'avg.p.win'=rep(-1e6, length(time.C.g.pred)),
                                'max.pred'=rep(-1e6, length(time.C.g.pred)),
                                'max.prob'=rep(-1e6, length(time.C.g.pred)),
                                'max.p.loss'=rep(-1e6, length(time.C.g.pred)),
                                'max.p.tie'=rep(-1e6, length(time.C.g.pred)),
                                'max.p.win'=rep(-1e6, length(time.C.g.pred)))
for (i in 1:dim(time.C.df.g)[1]) {
  if (dim(odds[format(as.Date(odds$match_date), "%Y-%m") == 
               format(as.Date(time.C.df.g[i, 'date']), "%Y-%m") & 
                  odds$home_team == time.C.df.g[i, 'home'] &
                  odds$away_team == time.C.df.g[i, 'away'],])[1] < 1) {
    betting.odds.pred[i,] <- rep(NA, 10)
  } else {
    ind <- which(format(as.Date(odds$match_date), "%Y-%m") == 
                   format(as.Date(time.C.df.g[i, 'date']), "%Y-%m") & 
                   odds$home_team == time.C.df.g[i, 'home'] &
                   odds$away_team == time.C.df.g[i, 'away'])
    if (length(ind) > 1) {
      ind <- ind[which.min(abs(as.Date(odds[ind, 'match_date']) - 
                            as.Date(time.C.df.g[i, 'date'])))]
    }
    avg.odds <- c(odds[ind,'avg_odds_away_win'],
                  odds[ind, 'avg_odds_draw'],
                  odds[ind,'avg_odds_home_win'])
    betting.odds.pred[i, 'avg.pred'] <- which.min(avg.odds) / 2 - 0.5
    avg.probs <- avg.odds**(-1)
    avg.probs <- avg.probs * (1 / sum(avg.probs))
    betting.odds.pred[i, 'avg.prob'] <- 
      avg.probs[(time.C.df.g[i, 'result'] + 0.5) * 2]
    betting.odds.pred[i, 'avg.p.loss'] <- avg.probs[1]
    betting.odds.pred[i, 'avg.p.tie'] <- avg.probs[2]
    betting.odds.pred[i, 'avg.p.win'] <- avg.probs[3]
    
    max.odds <- 
      c(odds[ind,'max_odds_away_win'],
        odds[ind,'max_odds_draw'],
        odds[ind,'max_odds_home_win'])
    betting.odds.pred[i, 'max.pred'] <- which.min(max.odds) / 2 - 0.5
    max.probs <- max.odds**(-1)
    max.probs <- max.probs * (1 / sum(max.probs))
    betting.odds.pred[i, 'max.prob'] <- 
      max.probs[(time.C.df.g[i, 'result'] + 0.5) * 2]
    betting.odds.pred[i, 'max.p.loss'] <- max.probs[1]
    betting.odds.pred[i, 'max.p.tie'] <- avg.probs[2]
    betting.odds.pred[i, 'max.p.win'] <- avg.probs[3]
  }
}

betting.odds.pred <- na.omit(betting.odds.pred)
avg.probs.temp <- betting.odds.pred[,c('avg.p.loss', 'avg.p.tie', 'avg.p.win')]
colnames(avg.probs.temp) <- c("p.loss", "p.tie", "p.win")
max.probs.temp <- betting.odds.pred[,c('max.p.loss', 'max.p.tie', 'max.p.win')]
colnames(max.probs.temp) <- c("p.loss", "p.tie", "p.win")

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

frequency = data.frame("team"=unique(mls.regular.season$home),
                       "w"=0,
                       "t"=0,
                       "l"=0,
                       "total"=0)

time.AB.df.g = mls.g[mls.g$year %in% time.B | mls.g$year %in% time.A,]
for (i in 1:dim(time.AB.df.g)[1]) {
  home = time.AB.df.g[i,]$home
  away = time.AB.df.g[i,]$away
  if (time.AB.df.g[i,]$result == 1.0) {
    frequency$w[frequency$team == home] = frequency$w[frequency$team == home] + 1
    # frequency$l[frequency$team == away] = frequency$w[frequency$team == away] + 1
  } else if (time.AB.df.g[i,]$result == 0.5) { 
    frequency$t[frequency$team == home] = frequency$t[frequency$team == home] + 1
    # frequency$t[frequency$team == away] = frequency$t[frequency$team == away] + 1
  } else {
    frequency$l[frequency$team == home] = frequency$w[frequency$team == home] + 1
    # frequency$w[frequency$team == away] = frequency$l[frequency$team == away] + 1
  }
  frequency$total[frequency$team == home] = frequency$total[frequency$team == home] + 1
  # frequency$total[frequency$team == away] = frequency$total[frequency$team == away] + 1
  
}
time.C.freq.pred = rep(0, dim(time.C.df.g)[1])
frequency.probs = data.frame(p.loss=numeric(0),p.tie=numeric(0),p.win=numeric(0))

for (i in 1:dim(time.C.df.g)[1]) {
  home = time.C.df.g[i,]$home
  home_wins = frequency$w[frequency$team == home]
  home_ties = frequency$t[frequency$team == home]
  home_losses = frequency$l[frequency$team == home]
  home_total = frequency$total[frequency$team == home]
  if (home_total != 0) {
    result = sample(c(1, 0.5, 0), 
                    prob = c(home_wins/home_total, home_ties/home_total, home_losses/home_total),
                    size = 1)
    frequency.probs[i, "p.loss"] = home_losses/home_total
    frequency.probs[i, "p.tie"] = home_ties/home_total
    frequency.probs[i, "p.win"] = home_wins/home_total
  } else {
    # if there is no freq history, then just default to uniform
    result = sample(c(1, 0.5, 0), 
                    prob = c(1/3, 1/3, 1/3),
                    size = 1)
    frequency.probs[i, "p.loss"] = 1/3
    frequency.probs[i, "p.tie"] = 1/3
    frequency.probs[i, "p.win"] = 1/3
  }
  
  time.C.freq.pred[i] = result
  # update frequency table
  if (time.C.df[i,]$result == 1.0) {
    frequency$w[frequency$team == home] = home_wins + 1
  } else if (time.C.df[i,]$result == 0.5) { 
    frequency$t[frequency$team == home] = home_ties + 1
  } else {
    frequency$l[frequency$team == home] = home_losses + 1
  }
  frequency$total[frequency$team == home] = home_total + 1
}

set.seed(143)

pred.prob(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)
pred.prob(time.C.df.g, elo.g.result.fit$coefficients, elo.g.result.fit$zeta)
unif_outcomes = (ceiling(runif(dim(time.C.df)[1], min=-1, max=2)))/2
pred.prob(time.C.df, glicko.base.result.fit$coefficients, glicko.base.result.fit$zeta, column = "glicko.rating.diff")

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
quad.loss.sd <- function(y, y.pred) {
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
  return(sd(loss_l+tie_l+win_l, na.rm = TRUE))
}
info.loss <- function(df, coef, zeta, column = "rating.diff") {
   return(mean(-1 * log2(pred.prob(df, coef, zeta, column = column)), na.rm= TRUE))
}
info.loss.sd <- function(df, coef, zeta, column = "rating.diff") {
  return(sd(-1 * log2(pred.prob(df, coef, zeta, column = column)), na.rm= TRUE))
}

elo.b.probs = pred.prob.all(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta)
elo.g.probs = pred.prob.all(time.C.df.g, elo.g.result.fit$coefficients, elo.g.result.fit$zeta)
glicko.probs = pred.prob.all(time.C.df, glicko.base.result.fit$coefficients, glicko.base.result.fit$zeta, column = "glicko.rating.diff")

unif.probs.temp <- data.frame('p.loss'=rep(1/3, length(unif_outcomes)),
                              'p.tie'=rep(1/3, length(unif_outcomes)),
                              'p.win'=rep(1/3, length(unif_outcomes)))
pred.freq.prob <- function(df) {
  mini.fn <- function(row) {
    # print(typeof(row[['rating.diff']]))
    # print(row[['rating.diff']])
    home = time.C.df.g[i,]$home
    home_wins = frequency$w[frequency$team == home]
    home_ties = frequency$t[frequency$team == home]
    home_losses = frequency$l[frequency$team == home]
    home_total = frequency$total[frequency$team == home]
    
    p.loss <- home_losses / home_total
    p.tie <- home_ties / home_total
    p.win <- home_wins / home_total
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

indx <- as.integer(rownames(betting.odds.pred))
# Calculate loss
loss.df <- data.frame('method'=c('ELO.b', 'ELO.g', 'GLICKO', 'AVG', 'MAX', 'UNIF', 'FREQ'),
                      'quad.loss'= 
                        c(quad.loss(time.C.df$result, elo.b.probs),
                          quad.loss(time.C.df.g$result, elo.g.probs),
                          quad.loss(time.C.df$result, glicko.probs),
                          quad.loss(time.C.df[indx, 'result'], 
                                    avg.probs.temp),
                          quad.loss(time.C.df[indx,'result'], 
                                    max.probs.temp),
                          quad.loss(time.C.df$result, unif.probs.temp),
                          quad.loss(time.C.df$result, frequency.probs)),
                      'quad.loss.sd'= 
                        c(quad.loss.sd(time.C.df$result, elo.b.probs),
                          quad.loss.sd(time.C.df.g$result, elo.g.probs),
                          quad.loss.sd(time.C.df$result, glicko.probs),
                          quad.loss.sd(time.C.df[indx, 'result'], 
                                    avg.probs.temp),
                          quad.loss.sd(time.C.df[indx,'result'], 
                                    max.probs.temp),
                          quad.loss.sd(time.C.df$result, unif.probs.temp),
                          quad.loss.sd(time.C.df$result, frequency.probs)),
                      'info.loss'=
                        c(info.loss(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta),
                          info.loss(time.C.df.g, elo.g.result.fit$coefficients, elo.g.result.fit$zeta),
                          info.loss(time.C.df, glicko.base.result.fit$coefficients, glicko.base.result.fit$zeta, column = "glicko.rating.diff"),
                          mean(-1 * log2(betting.odds.pred$avg.prob)),
                          mean(-1 * log2(betting.odds.pred$max.prob)),
                          -1 * log2(1/3),
                          mean(-1 * log2(pred.freq.prob(time.C.df)))),
                      'info.loss.sd'=
                        c(info.loss.sd(time.C.df, elo.base.result.fit$coefficients, elo.base.result.fit$zeta),
                          info.loss.sd(time.C.df.g, elo.g.result.fit$coefficients, elo.g.result.fit$zeta),
                          info.loss.sd(time.C.df, glicko.base.result.fit$coefficients, glicko.base.result.fit$zeta, column = "glicko.rating.diff"),
                          sd(-1 * log2(betting.odds.pred$avg.prob)),
                          sd(-1 * log2(betting.odds.pred$max.prob)),
                          0,
                          sd(-1 * log2(pred.freq.prob(time.C.df)))))

loss.df.2 <- data.frame('method'=c('ELO.b', 'ELO.g', 'GLICKO', 'AVG', 'MAX', 'UNIF', 'FREQ'),
                      'quad.loss'= 
                        c(quad.loss(time.C.df$result[indx], elo.b.probs[indx,]),
                          quad.loss(time.C.df.g$result[indx], elo.g.probs[indx,]),
                          quad.loss(time.C.df$result[indx], glicko.probs[indx,]),
                          quad.loss(time.C.df[indx, 'result'], 
                                    avg.probs.temp),
                          quad.loss(time.C.df[indx,'result'], 
                                    max.probs.temp),
                          quad.loss(time.C.df$result[indx], unif.probs.temp[indx,]),
                          quad.loss(time.C.df$result[indx], frequency.probs[indx,])),
                      'quad.loss.sd'= 
                        c(quad.loss.sd(time.C.df$result[indx], elo.b.probs[indx,]),
                          quad.loss.sd(time.C.df.g$result[indx], elo.g.probs[indx,]),
                          quad.loss.sd(time.C.df$result[indx], glicko.probs[indx,]),
                          quad.loss.sd(time.C.df[indx, 'result'], 
                                       avg.probs.temp),
                          quad.loss.sd(time.C.df[indx,'result'], 
                                       max.probs.temp),
                          quad.loss.sd(time.C.df$result, unif.probs.temp),
                          quad.loss.sd(time.C.df$result[indx], frequency.probs[indx,])),
                      'info.loss'=
                        c(info.loss(time.C.df[indx,], elo.base.result.fit$coefficients, elo.base.result.fit$zeta),
                          info.loss(time.C.df.g[indx,], elo.g.result.fit$coefficients, elo.g.result.fit$zeta),
                          info.loss(time.C.df[indx,], glicko.base.result.fit$coefficients, glicko.base.result.fit$zeta, column = "glicko.rating.diff"),

                          mean(-1 * log2(betting.odds.pred$avg.prob)),
                          mean(-1 * log2(betting.odds.pred$max.prob)),
                          -1 * log2(1/3),
                          mean(-1 * log2(pred.freq.prob(time.C.df)))),
                      'info.loss.sd'=
                        c(info.loss.sd(time.C.df[indx,], elo.base.result.fit$coefficients, elo.base.result.fit$zeta),
                          info.loss.sd(time.C.df.g[indx,], elo.g.result.fit$coefficients, elo.g.result.fit$zeta),
                          info.loss.sd(time.C.df[indx,], glicko.base.result.fit$coefficients, glicko.base.result.fit$zeta, column = "glicko.rating.diff"),
                          sd(-1 * log2(betting.odds.pred$avg.prob)),
                          sd(-1 * log2(betting.odds.pred$max.prob)),
                          0,
                          sd(-1 * log2(pred.freq.prob(time.C.df)))))

##### Recreate Fig. 3 for our ordered logit model with goal difference
dummy.diff <- -600:600
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

png('pred-prob_vs_rating-diff.png', width=600, height=500)
plot(dummy.loss.df$prob ~ dummy.loss.df$rating.diff, col='red', type='l', 
     main="Ordered Logit Predicted Probabilities vs. Rating Difference,\nwith Goal Difference", 
     ylab="Predicted Probability",
     xlab="Rating Difference\nHFA = 75.29",
     ylim=c(0,1))
lines(dummy.draw.df$prob ~ dummy.draw.df$rating.diff, col='purple')
lines(dummy.win.df$prob ~ dummy.win.df$rating.diff, col='blue')
legend(200,0.5, c('Home Loss', 'Draw', 'Home Win'), 
       col=c('red', 'purple', 'blue'), lty=1)
dev.off()

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
hfa.est.g <- range[which.min(prob.diffs)]

##### Recreate Fig. 3 for our ordered logit model with base elo
dummy.diff <- -600:600
dummy.loss.df <- data.frame('result'=as.factor(rep(0, length(dummy.diff))), 
                            'rating.diff'=dummy.diff)
dummy.loss.df$prob <- pred.prob(dummy.loss.df, elo.base.result.fit$coefficients, 
                                elo.base.result.fit$zeta)
dummy.draw.df <- data.frame('result'=as.factor(rep(0.5, length(dummy.diff))), 
                            'rating.diff'=dummy.diff)
dummy.draw.df$prob <- pred.prob(dummy.draw.df, elo.base.result.fit$coefficients, 
                                elo.base.result.fit$zeta)
dummy.win.df <- data.frame('result'=as.factor(rep(1, length(dummy.diff))), 
                           'rating.diff'=dummy.diff)
dummy.win.df$prob <- pred.prob(dummy.win.df, elo.base.result.fit$coefficients, 
                               elo.base.result.fit$zeta)

png('pred-prob_vs_rating-diff_base-elo.png', width=600, height=500)
plot(dummy.loss.df$prob ~ dummy.loss.df$rating.diff, col='red', type='l', 
     main="Ordered Logit Predicted Probabilities vs. Rating Difference,\nwithout Goal Difference", 
     ylab="Predicted Probability",
     xlab="Rating Difference\nHFA = 75.00", 
     ylim=c(0,1))
lines(dummy.draw.df$prob ~ dummy.draw.df$rating.diff, col='purple')
lines(dummy.win.df$prob ~ dummy.win.df$rating.diff, col='blue')
legend(200,0.5, c('Home Loss', 'Draw', 'Home Win'), 
       col=c('red', 'purple', 'blue'), lty=1)
dev.off()

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


# Create table for write-up/presentation
loss.df.2.rounded <- loss.df.2
loss.df.2.rounded$quad.loss <- round(loss.df.2.rounded$quad.loss, 3)
loss.df.2.rounded$quad.loss.sd <- round(loss.df.2.rounded$quad.loss.sd, 3)
loss.df.2.rounded$info.loss <- round(loss.df.2.rounded$info.loss, 3)
loss.df.2.rounded$info.loss.sd <- round(loss.df.2.rounded$info.loss.sd, 3)
oo <- order(loss.df.2.rounded$info.loss, decreasing=TRUE)
loss.df.2.rounded <- loss.df.2.rounded[oo,]

knitr::kable(loss.df, 'latex', vline='')
knitr::kable(loss.df.2.rounded, 'latex', vline='')


##### Evaluate methods based on hypothetical bets

## Gather maximum odds for each game in time.C.df
max.odds.all <- data.frame('away.win'=rep(-1e6, dim(time.C.df)[1]),
                           'draw'=rep(-1e6, dim(time.C.df)[1]), 
                           'home.win'=rep(-1e6, dim(time.C.df)[1]),
                           'result'=time.C.df$result)

for (i in 1:dim(time.C.df.g)[1]) {
  if (dim(odds[format(as.Date(odds$match_date), "%Y-%m") == 
               format(as.Date(time.C.df.g[i, 'date']), "%Y-%m") & 
               odds$home_team == time.C.df.g[i, 'home'] &
               odds$away_team == time.C.df.g[i, 'away'],])[1] < 1) {
    max.odds.all[i,c('away.win', 'draw', 'home.win')] <- rep(NA, 3)
  } else {
    ind <- which(format(as.Date(odds$match_date), "%Y-%m") == 
                   format(as.Date(time.C.df.g[i, 'date']), "%Y-%m") & 
                   odds$home_team == time.C.df.g[i, 'home'] &
                   odds$away_team == time.C.df.g[i, 'away'])
    if (length(ind) > 1) {
      ind <- ind[which.min(abs(as.Date(odds[ind, 'match_date']) - 
                                 as.Date(time.C.df.g[i, 'date'])))]
    }
    max.odds.all[i,c('away.win', 'draw', 'home.win')] <- 
      c(odds[ind,'max_odds_away_win'],
        odds[ind,'max_odds_draw'],
        odds[ind,'max_odds_home_win'])
  }
}

# For each prediction method, determine when to make a bet, and then determine
# the outcome of these bets
max.odds.all$elo.b <- apply(elo.b.probs, 1, max)
max.odds.all$elo.b.result <- apply(elo.b.probs, 1, which.max)
max.odds.all$elo.b.bet <- as.integer(max.odds.all$elo.b * 
  ifelse(max.odds.all$elo.b.result == 1, max.odds.all$away.win, 
         ifelse(max.odds.all$elo.b.result == 2, max.odds.all$draw, 
                                                max.odds.all$home.win)) > 1)
max.odds.all[is.na(max.odds.all$elo.b.bet), 'elo.b.bet'] <- 0
max.odds.all$elo.b.earnings <- max.odds.all$elo.b.bet
# max.odds.all[max.odds.all$elo.b.bet == 1 & 
#                max.odds.all$result != max.odds.all$elo.b.result / 2 - 0.5, 
#             'elo.b.earnings'] <- -1
max.odds.all[max.odds.all$elo.b.bet == 1 & 
               max.odds.all$result == (max.odds.all$elo.b.result / 2 - 0.5), 
             'elo.b.earnings'] <- 
  ifelse(max.odds.all[max.odds.all$elo.b.bet == 1 & 
                        max.odds.all$result == (max.odds.all$elo.b.result / 2 - 0.5), 
                      'elo.b.result'] == 1, 
         max.odds.all[max.odds.all$elo.b.bet == 1 & 
                        max.odds.all$result == (max.odds.all$elo.b.result / 2 - 0.5), 
                      'away.win'], 
         ifelse(max.odds.all[max.odds.all$elo.b.bet == 1 & 
                               max.odds.all$result == (max.odds.all$elo.b.result / 2 - 0.5), 
                               'elo.b.result'] == 2, 
                max.odds.all[max.odds.all$elo.b.bet == 1 & 
                               max.odds.all$result == (max.odds.all$elo.b.result / 2 - 0.5), 
                               'draw'], 
                max.odds.all[max.odds.all$elo.b.bet == 1 & 
                               max.odds.all$result == (max.odds.all$elo.b.result / 2 - 0.5), 
                             'home.win']))

max.odds.all$elo.g <- apply(elo.g.probs, 1, max)
max.odds.all$elo.g.result <- apply(elo.g.probs, 1, which.max)
max.odds.all$elo.g.bet <- as.integer(max.odds.all$elo.g * 
                                       ifelse(max.odds.all$elo.g.result == 1, max.odds.all$away.win, 
                                              ifelse(max.odds.all$elo.g.result == 2, max.odds.all$draw, 
                                                     max.odds.all$home.win)) > 1)
max.odds.all[is.na(max.odds.all$elo.g.bet), 'elo.g.bet'] <- 0
max.odds.all$elo.g.earnings <- max.odds.all$elo.g.bet
# max.odds.all[max.odds.all$elo.g.bet == 1 & 
#                max.odds.all$result != (max.odds.all$elo.g.result / 2 - 0.5), 
#             'elo.g.earnings'] <- -1
max.odds.all[max.odds.all$elo.g.bet == 1 & 
               max.odds.all$result == (max.odds.all$elo.g.result / 2 - 0.5), 
             'elo.g.earnings'] <- 
  ifelse(max.odds.all[max.odds.all$elo.g.bet == 1 & 
                        max.odds.all$result == (max.odds.all$elo.g.result / 2 - 0.5), 
                      'elo.g.result'] == 1, 
         max.odds.all[max.odds.all$elo.g.bet == 1 & 
                        max.odds.all$result == (max.odds.all$elo.g.result / 2 - 0.5), 
                      'away.win'], 
         ifelse(max.odds.all[max.odds.all$elo.g.bet == 1 & 
                               max.odds.all$result == (max.odds.all$elo.g.result / 2 - 0.5), 
                             'elo.g.result'] == 2, 
                max.odds.all[max.odds.all$elo.g.bet == 1 & 
                               max.odds.all$result == (max.odds.all$elo.g.result / 2 - 0.5), 
                             'draw'], 
                max.odds.all[max.odds.all$elo.g.bet == 1 & 
                               max.odds.all$result == (max.odds.all$elo.g.result / 2 - 0.5), 
                             'home.win']))

max.odds.all$glicko <- apply(glicko.probs, 1, max)
max.odds.all$glicko.result <- apply(glicko.probs, 1, which.max)
max.odds.all$glicko.bet <- as.integer(max.odds.all$glicko * 
                                       ifelse(max.odds.all$glicko.result == 1, max.odds.all$away.win, 
                                              ifelse(max.odds.all$glicko.result == 2, max.odds.all$draw, 
                                                     max.odds.all$home.win)) > 1)
max.odds.all[is.na(max.odds.all$glicko.bet), 'glicko.bet'] <- 0
max.odds.all$glicko.earnings <- max.odds.all$glicko.bet
# max.odds.all[max.odds.all$glicko.bet == 1 & 
#                max.odds.all$result != (max.odds.all$glicko.result / 2 - 0.5) , 
#             'glicko.earnings'] <- -1
max.odds.all[max.odds.all$glicko.bet == 1 & 
               max.odds.all$result == (max.odds.all$glicko.result / 2 - 0.5), 
             'glicko.earnings'] <- 
  ifelse(max.odds.all[max.odds.all$glicko.bet == 1 & 
                        max.odds.all$result == (max.odds.all$glicko.result / 2 - 0.5), 
                      'glicko.result'] == 1, 
         max.odds.all[max.odds.all$glicko.bet == 1 & 
                        max.odds.all$result == (max.odds.all$glicko.result / 2 - 0.5), 
                      'away.win'], 
         ifelse(max.odds.all[max.odds.all$glicko.bet == 1 & 
                               max.odds.all$result == (max.odds.all$glicko.result / 2 - 0.5), 
                             'glicko.result'] == 2, 
                max.odds.all[max.odds.all$glicko.bet == 1 & 
                               max.odds.all$result == (max.odds.all$glicko.result / 2 - 0.5), 
                             'draw'], 
                max.odds.all[max.odds.all$glicko.bet == 1 & 
                               max.odds.all$result == (max.odds.all$glicko.result / 2 - 0.5), 
                             'home.win']))

max.odds.all$avg <- NA
max.odds.all[rownames(betting.odds.pred),'avg'] <- 
  apply(betting.odds.pred[,c("avg.p.loss", "avg.p.tie", "avg.p.win")], 1, max)
max.odds.all$avg.result <- NA
max.odds.all[rownames(betting.odds.pred), 'avg.result'] <- 
  apply(betting.odds.pred[,c("avg.p.loss", "avg.p.tie", "avg.p.win")], 1, 
        which.max)
max.odds.all$avg.bet <- as.integer(max.odds.all$avg * 
                                        ifelse(max.odds.all$avg.result == 1, max.odds.all$away.win, 
                                               ifelse(max.odds.all$avg.result == 2, max.odds.all$draw, 
                                                      max.odds.all$home.win)) > 1)
max.odds.all[is.na(max.odds.all$avg.bet), 'avg.bet'] <- 0
max.odds.all$avg.earnings <- max.odds.all$avg.bet
# max.odds.all[max.odds.all$avg.bet == 1 & 
#                max.odds.all$result != (max.odds.all$avg.result / 2 - 0.5), 
#             'avg.earnings'] <- -1
max.odds.all[max.odds.all$avg.bet == 1 & 
               max.odds.all$result == (max.odds.all$avg.result / 2 - 0.5), 
             'avg.earnings'] <- 
  ifelse(max.odds.all[max.odds.all$avg.bet == 1 & 
                        max.odds.all$result == (max.odds.all$avg.result / 2 - 0.5), 
                      'avg.result'] == 1, 
         max.odds.all[max.odds.all$avg.bet == 1 & 
                        max.odds.all$result == (max.odds.all$avg.result / 2 - 0.5), 
                      'away.win'], 
         ifelse(max.odds.all[max.odds.all$avg.bet == 1 & 
                               max.odds.all$result == (max.odds.all$avg.result / 2 - 0.5), 
                             'avg.result'] == 2, 
                max.odds.all[max.odds.all$avg.bet == 1 & 
                               max.odds.all$result == (max.odds.all$avg.result / 2 - 0.5), 
                             'draw'], 
                max.odds.all[max.odds.all$avg.bet == 1 & 
                               max.odds.all$result == (max.odds.all$avg.result / 2 - 0.5), 
                             'home.win']))

max.odds.all$max <- NA
max.odds.all[rownames(betting.odds.pred),'max'] <- 
  apply(betting.odds.pred[,c("max.p.loss", "max.p.tie", "max.p.win")], 1, max)
max.odds.all$max.result <- NA
max.odds.all[rownames(betting.odds.pred), 'max.result'] <- 
  apply(betting.odds.pred[,c("max.p.loss", "max.p.tie", "max.p.win")], 1, 
        which.max)
max.odds.all$max.bet <- as.integer(max.odds.all$max * 
                                        ifelse(max.odds.all$max.result == 1, max.odds.all$away.win, 
                                               ifelse(max.odds.all$max.result == 2, max.odds.all$draw, 
                                                      max.odds.all$home.win)) > 1)
max.odds.all[is.na(max.odds.all$max.bet), 'max.bet'] <- 0
max.odds.all$max.earnings <- max.odds.all$max.bet
# max.odds.all[max.odds.all$max.bet == 1 & 
#               max.odds.all$result != (max.odds.all$max.result / 2 - 0.5), 
#             'max.earnings'] <- -1
max.odds.all[max.odds.all$max.bet == 1 & 
               max.odds.all$result == (max.odds.all$max.result / 2 - 0.5), 
             'max.earnings'] <- 
  ifelse(max.odds.all[max.odds.all$max.bet == 1 & 
                        max.odds.all$result == (max.odds.all$max.result / 2 - 0.5), 
                      'max.result'] == 1, 
         max.odds.all[max.odds.all$max.bet == 1 & 
                        max.odds.all$result == (max.odds.all$max.result / 2 - 0.5), 
                      'away.win'], 
         ifelse(max.odds.all[max.odds.all$max.bet == 1 & 
                               max.odds.all$result == (max.odds.all$max.result / 2 - 0.5), 
                             'max.result'] == 2, 
                max.odds.all[max.odds.all$max.bet == 1 & 
                               max.odds.all$result == (max.odds.all$max.result / 2 - 0.5), 
                             'draw'], 
                max.odds.all[max.odds.all$max.bet == 1 & 
                               max.odds.all$result == (max.odds.all$max.result / 2 - 0.5), 
                             'home.win']))

dollars.earned <- apply(max.odds.all[,c('elo.b.earnings', 'elo.g.earnings', 
                                        'glicko.earnings', 'avg.earnings', 
                                        'max.earnings')], 2, sum)

roi <- apply(max.odds.all[,c('elo.b.earnings', 'elo.g.earnings', 'glicko.earnings', 
                      'avg.earnings', 'max.earnings')], 2, sum) / 
  apply(max.odds.all[,c('elo.b.bet', 'elo.g.bet', 'glicko.bet', 'avg.bet',
                        'max.bet')], 2, sum) * 100 - 100

unit.bet.results <- data.frame('earnings' = dollars.earned, 
                               'roi' = roi)
rownames(unit.bet.results) <- c('ELO.b', 'ELO.g', 'GLICKO', 'AVG', 'MAX')

knitr::kable(round(t(unit.bet.results), 2), 'latex', vline='', 
  caption="Results from UNIT BET strategy compared across prediction methods.")


















