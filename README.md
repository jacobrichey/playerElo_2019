# playerElo

###### *All numbers updated to August 12th, 2019*

## Introduction
Consider the following comparison between Freddie Freeman (29) and Carlos Santana (33). Both players were starters for the 2019 All-Star teams of their respective leagues and are enjoying breakout seasons, beyond their usual high production level, with nearly identical statistics across the board.  

| PA | wOBA | xwOBA | wRC+ |
| -- | ---- | ----- | ---- |
| Freeman, 1B | 486 | 0.395 | 0.393 | 144 |
| Santana, 1B | 452 | 0.383 | 0.366 | 137 |

However, I argue there is an underlying statistic that makes Santana’s success less impressive and Freeman’s MVP-consideration worthy. Recall the quality of competition of pitchers faced. The Atlanta Braves’ division, the NL East, contains the respectable pitching competition of the Marlins (13th league-wide in ERA), Nationals (14th), Mets (16th), and Phillies (18th). Contrast this with the competition of the Cleveland Indians in the AL Central: The Twins (7th), Royals (24th), White Sox (25th), and Tigers (27th). Over 452 plate appearances, Santana has faced a top fifteen pitcher (ranked by FIP) just 9 times, compared to 46 times by Freeman over 486 plate appearances. wRC+ controls for park effects and the current run environment, while xwOBA takes into account quality of contact, but all modern sabermetrics fail to address the problem of Freeman and Santana’s near-equal statistics, despite widely different qualities of competition. Thus, I present the modeling system of playerElo.


## Methodology
Conceived out of inspiration from Arpad Elo’s rating system for zero-sum games like chess, and FiveThirtyEight’s use of an Elo modeling scheme for MLB team ratings and season-wide predictions, playerElo treats all at-bats as events and maintains a running power ranking of all MLB batters and pitchers. The system uses expected run values over the twenty-four possible base-out states. Additionally, run values are calculated for each at-bat event by subtracting the run expectancy of the beginning state from the ending state, and adding the runs scored.
`Run Value of Play = RE End State - RE Beginning State + Runs Scored`
The following run expectancy matrix presents the expected runs scored for the remainder of the inning, given the current run environment, baserunners, and number of outs. Data is sourced from all at-bats from 2016-2018, and expected run values are rounded to the second decimal place. For example, a grand slam hit with one out would shift the run expectancy from 1.54 to 0.27 and score four runs, so the run value of the play would be 2.73. 

| 1B | 2B | 3B | 0 outs | 1 out | 2 outs |
| -- | -- | -- | ------ | ----- | ------ |
| -- | -- | -- | 0.51 | 0.27 | 0.11 |
| 1B | -- | -- | 0.88 | 0.52 | 0.22 |


--	2B	--	1.15	0.69	0.32
--	--	3B	1.39	0.97	0.36
1B	2B	--	1.45	0.93	0.44
1B	--	3B	1.77	1.20	0.48
--	2B	3B	1.97	1.40	0.56
1B	2B	3B	2.21	1.54	0.75

The model begins with a calibration year of 2018, and for 2019, players begin with their previous seasons’ ending playerElo, regressed to the mean slightly. If a player did not have a single plate appearance or batter faced pitching in 2018, for example Vladimir Guerrero Jr. or Chris Paddack, they are assigned a baseline playerElo of 1000 (calibration year of 2018 began every player at 1000). For every at-bat, given the current base-out state, an expected run value for both the batter and pitcher is calculated, based on quadratic formulas of historic performance of players of that caliber in the given situation. The dependency of the Elo formula on the base-out state ensures the model is context-dependent, meaning it incorporates the fact that a bases-loaded double is far more valuable than a double with bases empty, however, it also takes into account that runs were more likely to be scored in the former situation compared to the latter. It is important to note playerElo is a raw batting statistic and does not evaluate overall production, meaning stolen bases are not factored in to the ranking system. Additionally, while the model does not take into account defense, it also does not count stolen bases or passed balls negatively against a pitcher, and likewise does not count changes in game states due to wild pitches positively for a batter.
Once an expected run value is synthesized from the current state and the playerElo of the batter and the pitcher, park factor and home field advantage adjustments (if applicable) are made, and the expected run value of the play is then compared to the true run value outcome. The playerElo of both the batter and pitcher are then updated accordingly, dependent on the difference between the true run value and the expected run value. For example, if an excellent pitcher strikes out a mediocre batter, the batter will not lose much Elo, and the pitcher will not gain much Elo. Likewise, if a below-average batter does extremely well against a top pitcher, there will be a far greater change in the Elo of both players. Errors are also taken into account and will prevent a positive run value from counting against a pitcher or positively for a batter. 




Analysis
Elo Rankings
It is interesting to note Charlie Blackmon and Nolan Arenado do particularly well in the model, even with park factor adjustments. This is can be attributed to the difficulty of schedule of the Rockies, who regularly face the powerhouse pitching of the LA Dodgers, and the respectable competition of the Diamondbacks, Padres, and Giants. Blackmon and Arenado have faced 52 and 51 top fifteen pitchers (rated by FIP) respectively thus-far, numbers which are in the top seven among all batters. Quality of contact does leave room to be desired for Blackmon, however, playerElo does not incorporate statistics like exit velocity and launch angle in its calculations, and thus the model is a better reflection of on-field performance than underlying swing metrics. Likewise, Cody Bellinger, while he still ranks high, is knocked down a few rungs to 8th overall among batters due to ease of competition (only 26 top pitchers faced).
Pitching wise, the breakout performances of Ryu, Gallegos, and Morton are all captured and supported by the playerElo model. Scherzer, Hader, and Yates’ continued success looks to be sustainable. Mike Minor ranks particularly high (23rd among SPs) after seeing success against the high-powered offense of the Astros (8th in runs scored), Angels (9th), Mariners (11th), and Athletics (12th), and would have been an impact pitcher if dealt at the deadline.
In contrast, playerElo has little faith in Trevor Bauer, one of the most discussed pitchers at the deadline, especially in the impact he will have on the Reds. Bauer is ranked 83rd among SPs with more than 100 batters faced, with a currentElo of 990.5 after beginning the season with a preseasonElo of 1111.30. He has struggled mightily against a relatively easy division of the AL Central (apart from the Twins), regularly facing the Royals (24th in runs scored), White Sox (26th), and Tigers (27th), posting an xFIP of 4.28. However, the Reds play in a division only slightly tougher offensively than the Indians and given the recent trend of quality starts from Bauer (despite his most recent, which included an impressive temper tantrum), it’s entirely possible Bauer does indeed turn things around for the second half. We can visualize Bauer’s season long playerElo trend within the context of the league with the following graphic. Displayed below is the playerElo trends of the entire MLB for 2019, with specific batters and pitchers highlighted. The bold line on both graphs denotes the average playerElo. 

 
The graph illustrates Mike Trout has dominated since the start of the year and Harper has consistently been great and continues to rise in playerElo (now ranked 9th among batters). Tatis Jr. elevates from a rookie with a playerElo of 1000 to approaching the top tier of players, and Joey Votto has slowly declined in value all season, with occasional flashes of his former ability. On the pitcher side, both Scherzer and Hader have maintained their elite status all season. Aaron Nola’s curve looks to be a long valley, but by his current trend he can be expected to regain his former value and playerElo score.



Sabermetric Analysis
As playerElo only evaluates historical season performance to-date, and is not a prediction statistic, comparing playerElo with underlying swing metrics can enable accurate forecasts as to potential second-half decliners, and identify first-half breakouts that are for real. 
The playerElo rankings are displayed alongside exit velocity, hard hit percentage, and expected weighted on-Base average (xwOBA), per Baseball Savant. By evaluating the four statistics in collectively, it is easy to identify players such as Jason Castro, Carlos Correa, J.D. Davis, Mitch Moreland, and Franmil Reyes whose quality of contact has been excellent, but have faced relatively easier pitching or performed worse contextually than other batters with similar quality of contact. Additionally, players such as Trout, Rendon, Yelich, Freeman, and Devers have performed extremely well, and their level of competition backs up their success. Harper and Bogaerts are likely due for excellent second halves, as they have continuously faced tough competition, and thrived under the circumstances, all-the-while maintaining solid quality of contact.

Pitching wise, Kirby Yates and Josh Hader standout among all pitchers, both with a high playerElo yet extremely low quality of contact stats. Gallegos, Scherzer, Cole, and deGrom all have a high currentElo and excellent quality of contact numbers. In contrast, Wendelken, Y. Garcia, Neris, Morgan, and James have posted great numbers, but their low currentElo indicate they have struggled contextually or against tougher competition (or simply have not faced tough competition), perhaps a sign of second-half regression.

Teams
Aggregate Team Elo power rankings can also be created from playerElo, with weights for each individual playerElo assigned by the plate appearances of the batter, or batters faced of the pitcher. Recall playerElo does not take into account defense or stolen bases, which will influence the ratings slightly. Team Elo rankings are calculated with weights on Team Batting Elo and Team Pitching Elo, based on a team’s Pythagorean win percentage However, the Team Elo rankings still give a fairly accurate view of the current state of the MLB, and particularly highlight upstart teams such as the Nationals and Athletics that could be better than they have seemed thus-far. Team Batting Elo is highly correlated with Runs Scored (r = 0.89), as is Team Pitching Elo with Runs Against (r = -0.87). 

Application
The playerElo system is able to reveal characteristics of the game and performance current metrics miss. For example, the context-dependent nature and run value calculations of playerElo appropriately credits a reliever who gets three outs without allowing a run after the previous pitcher loaded the bases, and similarly splits the credit of those runs being allowed between the pitcher who loaded the bases, and the reliever who failed to get the necessary outs to end the inning. Additionally, playerElo does not assign extra value to players who simply come up to the plate in high-powered offenses who often come up to the plate with runners on, nor docks players who simply do not get the same opportunities as their peers to accumulate as many counting statistics such as RBIs by nature of playing for a bad team. 
The difference between the playerElo rankings when run context-dependent, treating at-bats differently depending on the runs-out state, and context-free, meaning it treats all at-bats with equal weights of importance, highlight the important of including context in player evaluation. The playerElo system accurately captures if hitters consistently bat in favorable situations, and appropriately does not allow this to overinflate their value. 
But most importantly, the fundamental aspect of the playerElo system is that the quality of competition is factored into player analysis. Once again, consider the comparison between Freeman and Santana, who have nearly identical statistics but clearly face different levels of pitching competition. 
	PA	wOBA	xwOBA	wRC+	playerElo
Freeman 1B	486	0.395	0.393	144	1254.88
Santana 1B	452	0.383	0.366	137	1074.14

There is now a distinguishing factor between the two players, reflected by Freeman’s 3rd best overall playerElo among batters versus Santana’s 69th overall playerElo. 
