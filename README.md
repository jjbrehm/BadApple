# BadApple
BadApple is a library to extend the imitation simulations from WSS to be able to explore the question of the effect of contagion of isolated bad actors (saboteurs) among otherwise normally behaving bureaucrats. 

The idea is to test questions around police brutality: is police brutality confined to a few bad actors? does police brutality tend to spread among good actors as well? can police brutality be fixed by punishment, by increasing contact with good actors, or by replacement of bad actors with others?

The core of the present version of the simulation is that all of the bureaucrats learn to behave on the basis of imitattion, employing a "social proof" algorithm: bureaucrats begin with preferences and initial responses, acquire utility on the basis of the product of preferences and responses, are sanctioned for responses that fall far outside of the norms (or with other variations), and adapt by imitating the responses of the bureaucrat with the highest utility.

(Yes, there is an explicit social comparison of utility in the model. This is realistic to the extent that bureaucrats may be conscious of one anothers' utility, as in a pay check or observed success. The model also allows for reversion to a prior response if the prior response achieved a higher utility than the present utility.)

There are two core functions: ImitSim() and BadApple(). 

`foo <- ImitSim(1000)` - produces an ImitSim-object, foo, which contains the preferences, responses, etc, for each replication (run of the model under initial conditions) for each iteration (or step per replication).

`boo <- BadApple(1000)` produces a BadApple-object, boo, which contains the same as the quantities above, but also a variable denoting the number of saboteurs, a vector indicating which are the saboteurs, and their preferences.

There are numerous estimation and diagnostic functions.

betamle() estimates the Type I (nu, omega) formulation of the beta density for a given formula.  
mean_beta() produces the mean performance (DV) of the beta density at a given set of test values.  
effs_beta() calculates the first diffferences, and uses mean_beta

analyze() calls an igraph function to return the connectivity of an observability matrix among the bureaucrats (an instantiated adjecency matrix, or n-graph)  
plot_bgraph() produces a plot of the n-graph at a given replcation (using igraph graphing functions)  
plot_gg_ngraph() produces ggplot object of the n-graph at a given replication (using ggplot, and can be combined with grid.arrange, unlike plot_bgraph())  
plot_performance() plots a trace of the performance of the bureaucrats over iterations  
perf_by_iteration() plots a histogram of the mean performance for an optional list of iterations  

calc_saboteurs_vs_others() returns a matrix of values comparing the performance of the saboteurs and the others, calculating the difference, the contagion rate (where behavior of saboteurs spreads to others) and the conversion rate (where the saboteurs adopt the behavior of others)
snitch_tab() displays the results of calc_saboteurs_vs_others() in a neat kable (table) format  
snith_plots() plots a histogram and a density plot of the performance of saboteurs and others, returns a list of ggplot objects  

rearranger() is probably an unnecessary function since the tallperformance option of ImitSim/BadApple is no longer necessary for plots.



