```{r}
ctl = lmerControl(optimizer = "bobyqa",
                  optCtrl = list(maxfun = 1e6),
                  check.conv.singular = "ignore")

```
