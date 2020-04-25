### Try Models
- In this tab, the app will automatically [test possible models in parallel](https://ctmm-initiative.github.io/ctmm/articles/variogram.html#maximum-likelihood-fitting-the-easy-way) for every individual in current subset.
    + If the app was terminated by force, there could be forked R sessions remained active. It's better to clean up all R sessions in your system's task manager after force quitting the app.

### Model Summary Table and Variograms
- After model fitting finished, a summary table of attempted models is shown. Confidence intervals are shown as value pairs. Note models can be less accurate when small sampled data is used.
- Animal name is colored by same color theme in visualization page. Other columns are colored by model type, so same model type will have same color. You can use the search box to filter the table. 
- **Selected models in table will be basis of all latter analyses**. 
  - By default the best models (according to [AICc](https://ctmm-initiative.github.io/ctmm/reference/ctmm.fit.html)) are selected.
  - If you select different individuals/models, all the later analysis will be based on the current selection. 
- `Refit` button will take selected models as initial condition to fit again. Select a model from the dropdown list to fine-tune it will update the model result, and `Refit` will use updated result if available.
- `Initial Parameter` checkbox means the initial condition for model fitting, which could be the (fine-tuned) guesstimate value from last tab, or (fine-tuned) model result of this tab before refit.
- `Clean Up` button can remove all the less optimal models for each model type and animal.


### Reference
- For more information see [vignette](https://ctmm-initiative.github.io/ctmm/articles/variogram.html#maximum-likelihood-fitting-the-easy-way), [`ctmm.select`](https://ctmm-initiative.github.io/ctmm/reference/ctmm.fit.html)
- Reference table of models

  |Movement Models     &nbsp; &nbsp;   |Position <br>Autocorrelation  &nbsp; &nbsp; |Velocity <br>Autocorrelation  &nbsp; &nbsp;|Home Range  &nbsp; &nbsp;|Parameterization |
  |:--------------------------|:-------------------------|:------------------------|:----------|:----------------|
  |Ind. Ident. Distr. (IID)   |No                        |No                       |Yes        |τ = NULL         |
  |Brownian Motion (BM)       |Yes                       |No                       |No         |τ = ∞            |
  |Ornstein–Uhlenbeck (OU)    |Yes                       |No                       |Yes        |τ = τr           |
  |Integrated OU (IOU)        |Yes                       |Yes                      |No         |τ = {∞, τv}      |
  |Ornstein-Uhlenbeck F (OUF) |Yes                       |Yes                      |Yes        |τ = {τr, τv}     |
  |Special case of OUF (OUf) |Yes                       |Yes                      |Yes        |τ = {τ, τ}, τr = τv |
  |Oscillatory (OUΩ) |Yes                       |Yes                      |Yes        |τ = {τ, τ}, Ω > 0|

- [C. H. Fleming, J. M. Calabrese, T. Mueller, K.A. Olson, P. Leimgruber, W. F. Fagan. From fine-scale foraging to home ranges: A semi-variance approach to identifying movement modes across spatiotemporal scales. The American Naturalist, 183:5, E154-E167 (2014).](https://doi.org/10.1086/675504)

- [C. H. Fleming, Y. Subasi, J. M. Calabrese. A maximum-entropy description of animal movement. Physical Review E, 91, 032107 (2015).](https://doi.org/10.1103/PhysRevE.91.032107)

- [C. H. Fleming, D. Sheldon, E. Gurarie, W. F. Fagan, S. LaPoint, J. M. Calabrese. Kálmán filters for continuous-time movement models. Ecological Informatics, 40, 8-21 (2017).](https://doi.org/10.1016/j.ecoinf.2017.04.008)
