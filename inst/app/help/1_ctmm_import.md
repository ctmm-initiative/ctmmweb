### Dataset in ctmm package
- There are some data set available in `ctmm` package. You can load them to app directly.
- In exploring app features, You can take a sample of dataset so that the time consuming tasks can be finished quickly.
- Some datasets are anonymized, i.e. the real location and time information removed from data, with only relative values of `x`, `y` and `t`. To make them work in app, fake origin of location and time are added to simulate normal dataset. All individuals are projected to same origin of location, so `Overlap` page will not make sense and thus disabled. `Map` page is also meaningless for them and disabled.
