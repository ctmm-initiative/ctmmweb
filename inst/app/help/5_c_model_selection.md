- After fitting finished, a summary table of attempted models is shown. Individual name is colored by same color theme in visualization page. Other columns are colored by model type, so same model type will have same color. 
- You can use the search box to filter the table. For example, `ML` will show the ML rows only, `OUF` will show OUF models only.
- The first model for each individual is pre-selected. Selected models are applied to Variograms in modeled mode, `Home Range` and `Occurrence`. Just select models and switch to `Home Range` or `Occurrence` page will calculate by selected models. The figure height and column control also apply to them all.
- Reference table of models

  |Movement Models            |Position Autocorrelation  |Velocity Autocorrelation |Home Range |Parameterization |
  |:--------------------------|:-------------------------|:------------------------|:----------|:----------------|
  |Ind. Ident. Distr. (IID)   |No                        |No                       |Yes        |τ = NULL         |
  |Brownian Motion (BM)       |Yes                       |No                       |No         |τ = ∞            |
  |Ornstein–Uhlenbeck (OU)    |Yes                       |No                       |Yes        |τ = τr           |
  |Integrated OU (IOU)        |Yes                       |Yes                      |No         |τ = {∞, τv}      |
  |Ornstein-Uhlenbeck F (OUF) |Yes                       |Yes                      |Yes        |τ = {τr, τv}     |
