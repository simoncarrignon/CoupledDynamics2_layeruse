# CoupledDynamics2_layeruse

This repository stores the files needed to run the stochastic model to compare the value of information on local infection in an individual's "communication" layer versus their "infection" layer.  

Content:

* `FunctionsForPaper2_nv.R`: Functions used by the main scripts
* `ScriptForPaper2[abc]_nv.R`: 3 scripts running all
* `model_params.csv`: A `csv` files storing the values of the parameters explored in all the simulations
* `params1.csv`: A `csv` files storing the value of the parameters `soc_eff` and `hea_eff`, which are randomly generated 
* `network_params.csv`: A `csv` files storing the properties of the networks explored.

## Simulation

In order to run the model you will need to install some specific libraries. To do that one can open R and  do:

```R
install.packages(c("igraph","boost","rlist"))
```

Then, to run all simulation one can do from the command line::

```bash
Rscript ScriptForPaper2a_nv.R
Rscript ScriptForPaper2b_nv.R
Rscript ScriptForPaper2c_nv.R
```
This may take time (around 10 days in an average computer). These scripts will run all simulations using the parameters values stored in `model_params.csv` and the networks structure stored in `networks2/`. The result of each simulation will be stored as an RDS file in the folder `results/`. Each RDS summarises the dynamics of the simulation as a list of matrices. Each matrix keeps track of a given metrics of interest (number of new infections, number of new hospitalisations,...) for each time step of the simulation.

To retrieve the output of a single simulation in R:

```R
ex_sim = read.RDS("results/Bnets2mods1d_eff1p0.05.RDS") #replace Bnets2mods1d_eff1p0.05.RDS by any RDS already present in results/
```

## Visualisation
The plot presented in the paper can be reproduced using the script `Paper2Plotting.R`. __Warning__, this supposed that _all_ simulations had been run by the previous scripts.

First, other libraries need to be installed:

```R
install.packages("lme4","viridis")
```

Then the script `LayerUsePlotting_Revised.R` allows one to reproduce all the figure shown in the paper. 


## Parallel runs 
The branches contain elements of the code to run the simulations in parallel on high performance computing clusters

```bash
git checkout parallel
```
