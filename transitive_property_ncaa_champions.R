
############  NECESSARY PACKAGES  ###################################################################

install.packages("data.table")
install.packages("curl")
install.packages("stringr")
library(data.table)
library(curl)
library(stringr)

#############  DATA INPUT  ##########################################################################

#load in the dataset from the website
games_data <- fread("https://www.masseyratings.com/scores.php?s=298892&sub=12801&all=1&mode=3&format=0",sep='\n')
data <- games_data[-(1:8)]
head(data)
setDT(data)
names(data) <- "String"

#split it up neatly into columns
data[1] <- str_replace(data[1],"<hr><pre>","")
data[,"Date":=str_sub(String,1,10)]
data[,Team1:=str_sub(String,13,36)]
data[,Team1:=str_trim(Team1)]
data[,Score1:=str_sub(String,37,39)]
data[,Score1:=as.numeric(str_trim(Score1))]
data[,Team2:=str_sub(String,42,65)]
data[,Team2:=str_trim(Team2)]
data[,Score2:=str_sub(String,66,68)]
data[,Score2:=as.numeric(str_trim(Score2))]

##############  FUNCTIONS  ##########################################################################

#function to determine who beat a given team
who_beat_team_x <- function(x){
  outcomes_table <- data[Team1==x | Team2==x,2:6][
    (Team1==x & Score1 < Score2) | (Team2==x & Score1 > Score2)][
      ,.(Team1,Team2)]
  
  teams <- union(outcomes_table$Team1,outcomes_table$Team2)
  teams[!teams == x]
}

#function to determine who beat a list of teams
who_beat_teams_xyz_etc <- function(y){
  transitive_teams <- c()
  for(team in y){
    transitive_teams <- append(transitive_teams, who_beat_team_x(team))
    transitive_teams <- unique(transitive_teams)
  }
  transitive_teams
}

#function to check if team a beat team b
did_team_a_beat_team_b <- function(a,b){
  a %in% who_beat_team_x(b)
}

#function to determine the number of degrees of separation from the championship for a given team
count_degrees <- function(z, group_list){
  connection = FALSE
  for(group_index in 1:length(group_list)){
    if(z %in% group_list[[group_index]]){
      connection=TRUE
      break
    }
  }
  if(connection==FALSE){
    "No path"
  }
  else {
    group_index-1
  }
}

#function that finds a team in the next degree-of-separation group to establish a link in the path to Villanova
find_link <- function(team,list){
  for(member in list){
    if(did_team_a_beat_team_b(team,member)){
      link <- member
      break
    }
  }
  link
}

#function to establish and display the transitive chain from a given team to Villanova (if it exists)
transitive_chain <- function(team) {
  deg <- count_degrees(team, transitive_group_list)
  chain <- c()
  link <- team
  for(index in rev(1:deg)){
    link <- find_link(link, transitive_group_list[[index]])
    chain <- append(chain, link)
  }
  chain_display <- cat(team," -> ")
  for(link in chain[1:length(chain)-1]){
    chain_display <- cat(chain_display,link," -> ")
  }
  chain_display <- cat(chain_display,"Villanova")
  
}

######### CALCULATIONS ##############################################################################


#Define Villanova as having 0 degrees of separation from the championship
transitive_0_degrees <- "Villanova"

#Teams that beat Villanova have 1 degree of separation
transitive_1_degrees <- who_beat_team_x("Villanova")

#teams that have 2 degrees of separation
transitive_2_degrees <- who_beat_teams_xyz_etc(who_beat_team_x("Villanova"))

#3 degrees of separation
transitive_3_degrees <- who_beat_teams_xyz_etc(transitive_2_degrees)

#and so on
transitive_4_degrees <- who_beat_teams_xyz_etc(transitive_3_degrees)
transitive_5_degrees <- who_beat_teams_xyz_etc(transitive_4_degrees)
transitive_6_degrees <- who_beat_teams_xyz_etc(transitive_5_degrees)
transitive_7_degrees <- who_beat_teams_xyz_etc(transitive_6_degrees)
transitive_8_degrees <- who_beat_teams_xyz_etc(transitive_7_degrees)
transitive_9_degrees <- who_beat_teams_xyz_etc(transitive_8_degrees)
transitive_10_degrees <- who_beat_teams_xyz_etc(transitive_9_degrees)
transitive_11_degrees <- who_beat_teams_xyz_etc(transitive_10_degrees)
transitive_12_degrees <- who_beat_teams_xyz_etc(transitive_11_degrees)
transitive_13_degrees <- who_beat_teams_xyz_etc(transitive_12_degrees)
transitive_14_degrees <- who_beat_teams_xyz_etc(transitive_13_degrees)

#the greatest number of degrees of separation is 13
sum(sort(transitive_13_degrees) != sort(transitive_14_degrees))

#the number of teams that are transitive property national champions is 1185
length(transitive_13_degrees)

#there are 1362 teams that played games
length(unique(union(data$Team1,data$Team2)))

#create cumulative ds groups
cum_ds_2 <- union(transitive_2_degrees,who_beat_team_x("Villanova"))
cum_ds_3 <- union(transitive_3_degrees, cum_ds_2)
cum_ds_4 <- union(transitive_4_degrees, cum_ds_3)
cum_ds_5 <- union(transitive_5_degrees, cum_ds_4)
cum_ds_6 <- union(transitive_6_degrees, cum_ds_5)
cum_ds_7 <- union(transitive_7_degrees, cum_ds_6)
cum_ds_8 <- union(transitive_8_degrees, cum_ds_7)
cum_ds_9 <- union(transitive_9_degrees, cum_ds_8)
cum_ds_10 <- union(transitive_10_degrees, cum_ds_9)
cum_ds_11 <- union(transitive_11_degrees, cum_ds_10)
cum_ds_12 <- union(transitive_12_degrees, cum_ds_11)
cum_ds_13 <- union(transitive_13_degrees, cum_ds_12)

#teams that are exactly 2 degrees of separation away
ds2 <- setdiff(cum_ds_2,who_beat_team_x("Villanova"))
#3 degrees of separation
ds3 <- setdiff(cum_ds_3,cum_ds_2)
#and so on
ds4 <- setdiff(cum_ds_4,cum_ds_3)
ds5 <- setdiff(cum_ds_5,cum_ds_4)
ds6 <- setdiff(cum_ds_6,cum_ds_5)
ds7 <- setdiff(cum_ds_7,cum_ds_6)
ds8 <- setdiff(cum_ds_8,cum_ds_7)
ds9 <- setdiff(cum_ds_9,cum_ds_8)
ds10 <- setdiff(cum_ds_10,cum_ds_9)
ds11 <- setdiff(cum_ds_11,cum_ds_10)
ds12 <- setdiff(cum_ds_12,cum_ds_11)
ds13 <- setdiff(cum_ds_13,cum_ds_12)

#make sure I captured everything (should equal 1185)
length(c(who_beat_team_x("Villanova"),ds2,ds3,ds4,ds5,ds6,ds7,ds8,ds9,ds10,ds11,ds12,ds13))

#having those groups in a list will make it convenient to find paths
transitive_group_list <- list("Villanova",who_beat_team_x("Villanova"),ds2,ds3,ds4,ds5,ds6,ds7,ds8,ds9,ds10,ds11,ds12,ds13)

#transitive chain for a few example teams
transitive_chain("Wake Forest")
transitive_chain("Lenoir Rhyne")
transitive_chain("ETSU")
transitive_chain("Vanderbilt")

