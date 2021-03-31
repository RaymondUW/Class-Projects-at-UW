devtools::install_github("abresler/nbastatR")
install.packages("devtools")
library(nbastatR)
assign_nba_players()
HOF <- c("Kareem Abdul-Jabbar", "Ray Allen", 
                    "Nate Archibald", "Paul Arizin", "Al Attles", 
                    "Charles Barkley", "Don Barksdale", "Rick Barry",
                    "Elgin Baylor", "Zelmo Beaty", "Walt Bellamy",
                    "Dave Bing", "Larry Bird", "Bill Bradley",
                    "Carl Braun", "Kobe Bryant", "Al Cervi",
                    "Wilt Chamberlain", "Maurice Cheeks", "Nat Clifton",
                    "Chuck Cooper", "Bob Cousy", "Dave Cowens",
                    "Billy Cunningham", "Louie Dampier", "Mel Daniels",
                    "Adrian Dantley", "Bob Davies", "Dave DeBusschere",
                    "Vlade Divac", "Clyde Drexler", "Joe Dumars",
                    "Tim Duncan", "Alex English", "Julius Erving",
                    "Patrick Ewing", "Walt Frazier", "Joe Fulks",
                    "Harry Gallatin", "Kevin Garnett", "George Gervin",
                    "Artis Gilmore", "Tom Gola", "Gail Goodrich",
                    "Hal Greer", "Richie Guerin", "Cliff Hagan",
                    "John Havlicek", "Connie Hawkins", "Elvin Hayes",
                    "Spencer Haywood", "Tom Heinsohn", "Grant Hill",
                    "Bob Houbregs", "Bailey Howell", "Dan Issel",
                    "Allen Iverson", "Buddy Jeannette", "Dennis Johnson",
                    "Gus Johnson", "Magic Johnson", "Neil Johnston",
                    "Bobby Jones", "K.C. Jones", "Sam Jones",
                    "Michael Jordan", "Jason Kidd", "Bernard King",
                    "Bob Lanier", "Clyde Lovellette", "Jerry Lucas",
                    "Ed Macauley", "Karl Malone", "Moses Malone",
                    "Pete Maravich", "Sarunas Marciulionis",
                    "Slater Martin", "Bob McAdoo", "George McGinnis",
                    "Tracy McGrady", "Dick McGuire", "Kevin McHale",
                    "George Mikan", "Vern Mikkelsen", "Reggie Miller",
                    "Yao Ming", "Sidney Moncrief", "Earl Monroe",
                    "Alonzo Mourning", "Chris Mullin", "Calvin Murphy",
                    "Dikembe Mutombo", "Steve Nash", "Hakeen Olajuwon",
                    "Shaquille O'Neal", "Robert Parish", "Gary Payton",
                    "Drazen Petrovic", "Bob Pettit", "Andy Phillip",
                    "Scottie Pippen", "Jim Pollard", "Dino Radja",
                    "Frank Ramsey", "Willis Reed", "Mitch Richmond",
                    "Arnie Risen", "Oscar Robertson", "David Robinson",
                    "Guy Rodgers", "Dennis Rodman", "Bill Russell",
                    "Arvydas Sabonis", "Ralph Sampson", "Dolph Schayes",
                    "Bill Sharman", "Jack Sikma", "John Stockton",
                    "Maurice Stokes", "Isiah Thomas", "David Thompson",
                    "Nate Thurmond", "Jack Twyman", "Wes Unseld",
                    "Chet Walker", "Bill Walton", "Bobby Wanzer",
                    "Jerry West", "Paul Westphal", "Jo Jo White",
                    "Lenny Wilkens", "Jamaal Wilkes", "Dominique Wilkins",
                    "James Worthy", "George Yardley")

playids <- df_dict_nba_players$idPlayer
random <- sample.int(4589, 350)

Test <- players_careers(players = 
                          c("Kareem Abdul-Jabbar", "Ray Allen", 
                            "Nate Archibald", "Paul Arizin", "Al Attles", 
                            "Charles Barkley", "Don Barksdale", "Rick Barry",
                            "Elgin Baylor", "Zelmo Beaty", "Walt Bellamy",
                            "Dave Bing", "Larry Bird", "Bill Bradley",
                            "Carl Braun", "Kobe Bryant", "Al Cervi",
                            "Wilt Chamberlain", "Maurice Cheeks", "Nat Clifton",
                            "Chuck Cooper", "Bob Cousy", "Dave Cowens",
                            "Billy Cunningham", "Louie Dampier", "Mel Daniels",
                            "Adrian Dantley", "Bob Davies", "Dave DeBusschere",
                            "Vlade Divac", "Clyde Drexler", "Joe Dumars",
                            "Tim Duncan", "Alex English", "Julius Erving",
                            "Walt Frazier", "Joe Fulks",
                            "Harry Gallatin", "Kevin Garnett", "George Gervin",
                            "Artis Gilmore", "Tom Gola", "Gail Goodrich",
                            "Hal Greer", "Richie Guerin", "Cliff Hagan",
                            "John Havlicek", "Connie Hawkins", "Elvin Hayes",
                            "Spencer Haywood", "Tom Heinsohn", "Grant Hill",
                            "Bob Houbregs", "Bailey Howell", "Dan Issel",
                            "Allen Iverson", "Buddy Jeannette", "Dennis Johnson",
                            "Gus Johnson", "Magic Johnson", "Neil Johnston",
                            "K.C. Jones", "Sam Jones",
                            "Michael Jordan", "Jason Kidd", "Bernard King",
                            "Bob Lanier", "Clyde Lovellette", "Jerry Lucas",
                            "Ed Macauley", "Karl Malone", "Moses Malone",
                            "Pete Maravich", "Sarunas Marciulionis",
                            "Slater Martin", "Bob McAdoo", "George McGinnis",
                            "Tracy McGrady", "Dick McGuire", "Kevin McHale",
                            "George Mikan", "Vern Mikkelsen", "Reggie Miller",
                            "Yao Ming", "Sidney Moncrief", "Earl Monroe",
                            "Alonzo Mourning", "Chris Mullin", "Calvin Murphy",
                            "Dikembe Mutombo", "Steve Nash", "Hakeen Olajuwon",
                            "Shaquille O'Neal", "Robert Parish",
                            "Drazen Petrovic", "Bob Pettit", "Andy Phillip",
                            "Scottie Pippen", "Jim Pollard", "Dino Radja",
                            "Frank Ramsey", "Willis Reed", "Mitch Richmond",
                            "Arnie Risen", "Oscar Robertson", "David Robinson",
                            "Guy Rodgers", "Dennis Rodman", "Bill Russell",
                            "Arvydas Sabonis", "Ralph Sampson", "Dolph Schayes",
                            "Bill Sharman", "Jack Sikma", "John Stockton",
                            "Maurice Stokes", "Isiah Thomas", "David Thompson",
                            "Nate Thurmond", "Jack Twyman", "Wes Unseld",
                            "Chet Walker", "Bill Walton", "Bobby Wanzer",
                            "Jerry West", "Paul Westphal", "Jo Jo White",
                            "Lenny Wilkens", "Jamaal Wilkes", "Dominique Wilkins",
                            "James Worthy", "George Yardley"), 
                        player_ids = c("121", "56", "77193", playids[random] ),
                        modes = c("PerGame"))



names <- dataPlayerCareerTotalsRegularSeason$namePlayer
gp <- dataPlayerCareerTotalsRegularSeason$gp
pts <- dataPlayerCareerTotalsRegularSeason$ptsPerGame
ast <- dataPlayerCareerTotalsRegularSeason$astPerGame
treb <- dataPlayerCareerTotalsRegularSeason$trebPerGame
oreb <- dataPlayerCareerTotalsRegularSeason$orebPerGame
dreb <- dataPlayerCareerTotalsRegularSeason$drebPerGame
blk <- dataPlayerCareerTotalsRegularSeason$blkPerGame
stl <- dataPlayerCareerTotalsRegularSeason$stlPerGame
tov <- dataPlayerCareerTotalsRegularSeason$tovPerGame
pf <- dataPlayerCareerTotalsRegularSeason$pfPerGame
mins <- dataPlayerCareerTotalsRegularSeason$minutesPerGame

HallofFame <- ifelse(names[1:468] %in% HOF == TRUE, 1, 0)

#Stats of interest
PlayerData <- data.frame(names, HallofFame, gp, pts, ast, 
                       treb, oreb,dreb, blk, stl, tov, 
                       pf, mins)

#Stats complete for all Hall of Fame members
CompleteStats <- data.frame(names, HallofFame, gp, pts, ast, treb, 
                       pf ,mins)

Data2 <- na.omit(CompleteStats)
Data <- na.omit(PlayerData)

## 85% of the sample size
samp_size <- floor(0.85 * nrow(Data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Data)), size = samp_size)

trainset <- Data[train_ind, ]
testset <- Data[-train_ind, ]

logit <- glm(HallofFame ~ gp + pts + ast + treb + oreb + dreb
             + blk + stl + tov + pf + mins, data=trainset)

step(logit, direction = "both")

reduced <- glm(HallofFame ~ gp + pts  + ast + treb + oreb +
                 dreb + blk + pf + mins,
               data=trainset, family = binomial)
prediction = predict(reduced, type = 'response', 
                      newdata=testset)
testset$percent <- prediction
