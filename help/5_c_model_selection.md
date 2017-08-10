- Clicking `Fit Models` will start to fit possible models for every individual in current subset. It could be time taking so it need to be manually activated.
    + Depend on platform (Windows/Mac/Linux), different parallel processes are used. There will be a time report in console when running app in local mode. The `user` time is the total cpu time spent on all threads, and `elapsed` time is the actual time used.
- After fitting finished, a summary table of attempted models is shown. The first model for each individual is pre-selected. Selected models are applied to Variograms in modeled mode, `home range` and `occurrence`. Just select models and switch to `home range` or `occurrence` page will calculate by selected models. The figure height and column control also apply to them all.
- Reference table of models

  |Movement Models            |Position Autocorrelation  |Velocity Autocorrelation |Home Range |Parameterization |
  |:--------------------------|:-------------------------|:------------------------|:----------|:----------------|
  |Ind. Ident. Distr. (IID)   |No                        |No                       |Yes        |τ = NULL         |
  |Brownian Motion (BM)       |Yes                       |No                       |No         |τ = ∞            |
  |Ornstein–Uhlenbeck (OU)    |Yes                       |No                       |Yes        |τ = τr           |
  |Integrated OU (IOU)        |Yes                       |Yes                      |No         |τ = {∞, τv}      |
  |Ornstein-Uhlenbeck F (OUF) |Yes                       |Yes                      |Yes        |τ = {τr, τv}     |
