library(PlayerRatings)
library(reshape2)
library(MASS) # required for ordinal logistic regression

##### Data Wrangling
mls.original <- read.csv('soccer-elo/data/mls2001-2021.csv')
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

##### Write elo.g function for goal difference 

mls.g <- mls.regular.season[mls.regular.season$year %in% 2002:2022,
                            c('year', 'date_corrected', 'home', 'away', 
                              'result', 'score_difference')]
colnames(mls.g) <- c('year', 'date', 'home', 'away', 'result', 'gd')


##### Write the H&A four-step procedure

### Step 1: Split time periods
time.A <- 2002:2007
time.B <- 2008:2013
time.C <- 2014:2017

time.AB.df.g = mls.g[mls.g$year %in% time.B | mls.g$year %in% time.A,]

### Step 3: Get frequency of win/draw/loss for each team over time periods A and B
frequency = data.frame("team"=unique(mls.regular.season$home),
                       "w"=0,
                       "t"=0,
                       "l"=0,
                       "total"=0
                       )
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


### Step 4: Predict match results for time period C, calculate loss

# Create df for this time period
time.C.df.g <- mls.g[mls.g$year %in% time.C,]

time.C.pred = rep(0, dim(time.C.df.g)[1])

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
  } else {
    # if there is no freq history, then just default to uniform
    result = sample(c(1, 0.5, 0), 
                    prob = c(1/3, 1/3, 1/3),
                    size = 1)
  }
  time.C.pred[i] = result
  # update frequency table
  frequency$w[frequency$team == home] = home_wins + 1
  frequency$t[frequency$team == home] = home_ties + 1
  frequency$l[frequency$team == home] = home_losses + 1
  frequency$total[frequency$team == home] = home_total + 1
}


# Define function to get model's predicted probability for the true outcome
pred.prob <- function(df) {
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
# pred.prob(time.C.df.g, elo.g.result.fit$coefficients, elo.g.result.fit$zeta)

# Define loss functions
quad.loss <- function(y, y.pred) {
  return (mean((y - y.pred)**2))
}
info.loss <- function(df) {
  return(mean(-1 * log2(pred.prob(df))))
}

# Calculate loss
quad.loss(time.C.df.g$result, as.double(time.C.pred))
info.loss(time.C.df.g)




