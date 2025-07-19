library(doBy)
load_all()
## load_all("_doby")


interaction_plot(dat, sugpct ~ harvest + interaction(sow:block))

interaction_plot(dat, sugpct ~ sow + harvest)
interaction_plot(dat, sugpct ~ harvest + block)
interaction_plot(dat, sugpct ~ sow + block)
interaction_plot(dat, yield ~ harvest + sow)
interaction_plot(dat, yield ~ sow + harvest)
interaction_plot(dat, yield ~ harvest + block)
interaction_plot(dat, yield ~ block + harvest)
interaction_plot(dat, yield ~ sow + block)



dat <- SASmixed::Demand
names(dat) <- tolower(names(dat))
#dat$yearf <- as.factor(dat$year)
dat |> head()

## Fails
response_plot(dat, log(d) ~ ., 
              geoms=geom_point(alpha=.5), 
              global_aes=list(color="state"))

## Fails
response_plot(dat, d ~ ., 
              geoms=geom_point(alpha=.5) + geom_line(alpha=.5), 
              global_aes=list(color="state"))



## GOES TO DOBY ????
create_fun_list <- function(fun, list_of_arg_lists, method="def"){
    if (!inherits(list_of_arg_lists, "list"))
        stop("list_of_arg_lists must be a list\n")
    z <- sapply(arg_list, inherits, "list")
    if (!all(z))
        stop("not all elements in list_of_arg_lists are lists\n")
    
    ff <- lapply(list_of_arg_lists, function(a){
        doBy::section_fun(fit_ggm, list_of_arg_lists=a, method="def")
    })
    out <- bquote_fun_list(ff)
    out
}

out <- bquote_fun_list(ff)
lapply(out, eval)


is_list_of_lists <- function(x){
    inherits(x, "list") &&
        all(sapply(x, inherits, "list"))
}


vls4 <- lapply(vls3, function(v){names(v)<-nms;v})
ff <- lapply(vls4, function(v) section_fun(f, nms=v))
lapply(ff, eval)























