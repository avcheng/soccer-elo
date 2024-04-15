library(PlayerRatings)
library(MASS) # required for ordinal logistic regression

##### Data Wrangingling
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

elo.g <- function(df, init=1500, k0=30, lambda=0, gamma=0) {
  # Set up matrix of ratings, with one for each date
  teams <- unique(df$home)
  dates <- unique(df$date)
  history <- matrix(data=NA, nrow=length(teams), ncol=length(dates)+1)
  dimnames(history) <- list(teams, c('init', dates))
  
  # Set initial ratings
  history[,1] <- init
  ratings <- 0
  if (length(init) > 1 ) {
    ratings <- init
  } else {
    ratings <- rep(init, length(teams))
  }
  names(ratings) <- teams
  
  # Loop through games, and update ratings
  for (i in 1:dim(df)[1]) {
    ratings[df[i, 'home']] <-
      ratings[df[i, 'home']] + k0*(1 + abs(df[i, 'gd']))**lambda * 
      (df[i, 'result'] - We(ratings[df[i, 'home']], ratings[df[i, 'away']], 
                            gamma=gamma))
    
    ratings[df[i, 'away']] <- 
      ratings[df[i, 'away']] + k0*(1 + abs(df[i, 'gd']))**lambda * 
      ((1 - df[i, 'result']) - We(ratings[df[i, 'away']], 
                                  ratings[df[i, 'home']], gamma=gamma))
    
    history[df[i, 'home'], df[i, 'date']] <- ratings[df[i, 'home']]
    history[df[i, 'away'], df[i, 'date']] <- ratings[df[i, 'away']]
  }
  
  
  return(list('ratings'=ratings, 'history'=history))
}

my.elo <- elo.g(mls.g)

##### Compare our function to elo in PlayerRatings package
mls.mini <- mls[1:50,]
mls.g.mini <- mls.g[1:50,]
elo.mini <- elo(mls.mini, init=1500, gamma=0, kfac=30)
elo.g.mini <- elo.g(mls.g.mini)
# These results are fairly close to one another, but are not exactly the same

elo.g.full <- elo.g(mls.g)
elo.full <- elo(mls, init=1500, gamma=0, kfac=30, history=FALSE, sort=FALSE)
plot(elo.g.full$ratings[elo.full$ratings$Player] ~ elo.full$ratings$Rating)
cor(elo.g.full$ratings[elo.full$ratings$Player], elo.full$ratings$Rating)
# So are these — possible sources of error? 

elo.g(mls.g, init=elo.g.full$ratings)$ratings

##### Cross validate k0 and lambda
ls.lambda <- 0:100/20
ls.k <- 0:80/2
log.likelihood.hfa <- expand.grid(ls.k, ls.lambda)
colnames(log.likelihood.hfa) <- c("k0", "lambda")
log.likelihood.hfa$ll <- rep(-1e6, dim(log.likelihood.hfa)[1])
kfac.init <- 30

# Skeleton from hw5 that needs to be updated with our functions and data
for (j in 1:dim(log.likelihood.hfa)[1]) {
  ratings.elo <- elo.g(nrl.clean[nrl.clean$season==2009,], init=1500, 
                       k0=kfac.init, 
                       gamma=0)
  ratings.elo <- elo(nrl.clean[nrl.clean$season>=2009 & 
                                 nrl.clean$season <= 2018,], 
                     kfac = log.likelihood.hfa[j, 'K'], 
                     status=ratings.elo$ratings, 
                     gamma=log.likelihood.hfa[j, 'gamma'], 
                     sort=FALSE)
  
  # Validation on val set
  ll.tmp = 0
  for (val.year in 2019:2022){
    pred = predict(ratings.elo, nrl.clean[nrl.clean$season==val.year,], 
                   gamma=log.likelihood.hfa[j, 'gamma'])
    # Log-likelihood
    ll.tmp <- ll.tmp +
      sum(nrl.clean[nrl.clean$season==val.year,]$outcome * log(pred) +
            (1 - nrl.clean[nrl.clean$season==val.year,]$outcome) * 
            log(1-pred))
    # Update model 
    ratings.elo <- elo(nrl.clean[nrl.clean$season==val.year,], 
                       kfac = log.likelihood.hfa[j, 'K'], 
                       status = ratings.elo$ratings, 
                       gamma=log.likelihood.hfa[j, 'gamma'], 
                       sort=FALSE)
  }
  log.likelihood.hfa[j, 'll'] = ll.tmp
}































