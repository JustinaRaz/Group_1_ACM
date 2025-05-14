# Advanced Cognitive Modeling 

Repository contains the code for portfolio assignments.

The assignments within this repository are completed by **study group 1**, from Advanced Cognitive Modeling course in Aarhus University (2025).

This repository contains code for 4 assignments.

## Structure of the folders
### Assignment 1: Matching Pennies Game

```
Assignment-1/
├── agents.R   ---- The code that simulates random agent w/ bias and reinforcement learning agent's behaviour.
├── arena.R    ---- 
└── plays.R    ---- Agents play against one another.
```

### Assignment 2: Building a Cognitive Model of behaviour when playing Matching Pennies Game

```
Assignment_2/
├── models/                    ---- The folder with stan models.
├── ReinforcementLearning.R    ---- Running the stan.
├── param_recov_RL.R           ---- Script with parameter recovery.
└── u_func.R                   ---- Pre-defined functions, used in the scripts.
```

### Assignment 3: Analysis of real world data using Bayesian models of Cognition

```
Assignment-3/
├── data/                              ---- Data should be downloaded from https://pubmed.ncbi.nlm.nih.gov/30700729/. 
├── models/                            ---- The folder with stan models.
│   ├── blank_slate_beta.stan          ---- Simple model without priors.
│   ├── prior_trust_beta.stan          ---- Simple model with priors.
│   └── test_model_comparison_2.stan   ---- Simplest test model for model comparison pipeline.
├── blank_slate_beta.R                 ---- Running the 1st model on simulated data.
├── prior_trust_beta.R                 ---- Running the 2nd model on simulated data.
├── model_comparison.R                 ---- Script for running all three models on real data.
├── comparison_functions.R             ---- Pre-defined functions, used in model comparison script.
└── u_func.R                           ---- Pre-defined functions, used in other scripts as well.
```

### Assignment 4: GCM

```
Assignment-4/
├── data/                              ---- 
├── model/                             ---- The folder with stan model.
│   └── a4.stan                        ---- Stan model for categorization task.
├── empirical_data_visualization.R     ---- Code to visualize empirical data.              
└── func.R                             ---- Pre-defined functions, used in the scripts.
```
