### File 1: Hihi_AM_script

AM main simulation. Consists of multiple blocks. The simulation is run in parallel to speed computing times.
- Block 1 SETTINGS
Simulation settings, package loading, and alert setting. 
Sources "All_strategies_source.R" file, which contains the code for the decision rules and other additional functions.
-- LOOP 1 opens here: which strategies to run (between 1 and 7).

- Block 2 PRIOR INFORMATION
Lists existing and new sites, their type and age.
Loads and processes elicited information about carrying capacities.
Loads and processes estimated detection probabilities (from another model's posteriors).
Sets priors for post-release effects and population growth rate.
All priors are used to draw TRUE parameters for each simulation run, and passed as ESTIMATED priors to the NIMBLE model.

- Block 3 SIMULATION STORAGE
Creates empty arrays to store the results of the simulation: true population demographics and estimated parameters.

- Block 4 INITIAL VALUES
Sets values for t=1 in each simulation run. 
Population sizes drawn from another model's posterior, Harvest set to zero, feeding set to current status. 
Estimated parameters at t=1 set equal to the priors.

- Block 5 ADDITIONAL MANAGEMENT THRESHOLDS
Sets cutoff and criteria used to make management decisions in the "All_strategies_source" functions.

- Block 6 SIMULATION RUN
Set up parallel structure and progress bars and start simulation
-- LOOP 2 opens here: foreach (parallel simulations for this strategy).

STEP 1. Generate TRUE values (K and r) using the priors. These are used through this simulation run.
-- LOOP 3 opens here: annual processes.
STEP 2. Demographic process. Based on management settings for this year and TRUE parameters, determine population growth. Then generate counts based on TRUE population sizes and TRUE detection probabilities.
STEP 3. Learning process. Bundle data (site counts, management records and ESTIMATED priors) together, pass to NIMBLE model, initialize and run. Then update ESTIMATED values of K, r and N.
STEP 4. Decision process. Call "All_strategies_source", pass current ESTIMATED information and apply decision rules for this strategy. Update Harvest and Feeding variables to reflect what management has been decided for t+1 and t+2.
STEP 5. Effects of decision. Determine next year's TRUE carrying capacities, based on decision to feed or not. Add new sites to the list. 
-- LOOP 3 closes here (years).
Wrap results for this simulation in a list, so they can be processed later.
-- LOOP 2 closes here (foreach)

Block 7 PROCESS SIMULATION RESULTS
Remove failed simulations and wrap all results for this strategy in a list, so they can be processed later.
-- LOOP 1 closes here (strategies)
Create arrays with results for all simulation across all strategies.


### File 2: Hihi_AM_all_strategies_source
Script with decision rules. All follow the same structure. Note feeding decisions are for t+1, harvest and release for t+2 reflecting different logistic constraints.

Step 1 Harvest: determine which sites we can harvest birds from.
Step 2 Release: determine which sites should receive those birds. Assigned on criteria and priority rankings.
Step 3 Allocation: match harvest and release sites, then allocate birds.
Step 4 Remote islands: if some of the releases are to remote islands, source half those birds from Hauturu.
Step 5 Genetic releases: every 5th year, add extra releases for genetic flow.
Step 6 Feeding: determine which sites we are going to feed.

Other functions
- NIMBLE model for estimation of population parameters
- Estimation of beta parameters from mean and sd
- Send email alerts upon completion
- Combine multiple ggplots in one figure
