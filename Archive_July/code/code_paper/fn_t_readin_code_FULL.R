

##########################################################################################
## Read in csv file with Beta_t, Alpha_t, Kappa_t, Delta_t

fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
fn_t_readin = as.data.frame(read.csv(fn_t_readin_path, sep=",",stringsAsFactors = FALSE))

##########################################################################################
## Evaluate Beta_t, Beta_y from readin
start_time <- 45
Beta_t_dates <- as.Date(fn_t_readin$Beta_t)
Beta_t_dates[1] <- Beta_t_dates[1]-start_time
Beta_t <- round(as.numeric(Beta_t_dates - as.Date("2020-03-01")) + start_time)

mu_y_chr <- fn_t_readin$Beta_y
assign("mu.0",1)
assign("mu.1", R0_redux1)
assign("mu.2", R0_redux2)
assign("mu.3", R0_redux2) #0.31)
#assign("mu.3",4*(mu.2 - mu.1))
#assign("mu.4",(mu.3+mu.2)/2)
#assign("mu.5",mu.2)

mu_y <- as.vector(length(Beta_t))
for (i in 1:length(Beta_t)){
  mu_y[i] = get(mu_y_chr[i])
}
R0_y <- R0*mu_y


## Get Beta_y as a function of R0, R0_redux, r, and Alpha

Br.function <- function(R0.in, r.in, Alpha.in){
  d_IH <- 10   #days between illness onset and hospitalization
  d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
  Br <- R0.in * ( 1 / ( (r.in/ ((Alpha.in/d_IH) + ((1-Alpha.in)/d_IR)))  + (1-r.in)*d_IR ))
  return(Br)
}

Beta_y <- as.vector(length(Beta_t))
Beta_y<- c(
  Br.function(R0.in<-R0_y[1], r.in<-r1, Alpha.in<-Alpha1) ,
  Br.function(R0.in<-R0_y[2], r.in<-r1, Alpha.in<-Alpha1),
  Br.function(R0.in<-R0_y[3], r.in<-r1, Alpha.in<-Alpha1),
  Br.function(R0.in<-R0_y[4], r.in<-r1, Alpha.in<-Alpha1),
  Br.function(R0.in<-R0_y[5], r.in<-r1, Alpha.in<-Alpha1),
  Br.function(R0.in<-R0_y[6], r.in<-r2, Alpha.in<-Alpha2),
  Br.function(R0.in<-R0_y[7], r.in<-r2, Alpha.in<-Alpha2),
  Br.function(R0.in<-R0_y[8], r.in<-r2, Alpha.in<-Alpha2),
  Br.function(R0.in<-R0_y[9], r.in<-r2, Alpha.in<-Alpha2),
  Br.function(R0.in<-R0_y[10], r.in<-r2, Alpha.in<-Alpha2),
  Br.function(R0.in<-R0_y[11], r.in<-r2, Alpha.in<-Alpha2),
  Br.function(R0.in<-R0_y[12], r.in<-r2, Alpha.in<-Alpha2),
  Br.function(R0.in<-R0_y[13], r.in<-r2, Alpha.in<-Alpha2)
)

##########################################################################################
## Evaluate r_t from readin

r_t_dates <- as.Date(fn_t_readin$r_t)
r_t_dates <- na.omit(r_t_dates)
r_t_dates[1] <- r_t_dates[1]-start_time
r_t <- round(as.numeric(r_t_dates - as.Date("2020-03-01")) + start_time)

r_y_chr <- fn_t_readin$r_y
assign("r1",r1)
assign("r2", r1)

r_y <- as.vector(length(r_t))
for (i in 1:length(r_t)){
  r_y[i] = get(r_y_chr[i])
}

##########################################################################################
## Evaluate Alpha_t from readin

alpha_t_readin_path <- path(data.dir, "alpha_t_readin.csv")
alpha_t_readin = as.data.frame(read.csv(alpha_t_readin_path, sep=",",stringsAsFactors = FALSE))

Alpha_t_dates <- as.Date(alpha_t_readin$Alpha_t)
Alpha_t_dates[1] <- Alpha_t_dates[1]-start_time
Alpha_t <- round(as.numeric(Alpha_t_dates - as.Date("2020-03-01")) + start_time)

Alpha_y_chr <- alpha_t_readin$Alpha_y
assign("Alpha1",Alpha1)
assign("Alpha2", Alpha2)

Alpha_y <- as.vector(length(Alpha_t))
for (i in 1:length(Alpha_t)){
  Alpha_y[i] = get(Alpha_y_chr[i])
}

##########################################################################################
## Kappa_t

Kappa_t <- Alpha_t

Kappa_y_chr <- alpha_t_readin$Kappa_y
assign("Kappa1",Kappa1)
assign("Kappa2", Kappa2)

Kappa_y <- as.vector(length(Alpha_t))
for (i in 1:length(Alpha_t)){
  Kappa_y[i] = get(Kappa_y_chr[i])
}

##########################################################################################
## Delta_t

Delta_t <- Alpha_t

Delta_y_chr <- alpha_t_readin$Delta_y
assign("Delta1",Delta1)
assign("Delta2", Delta2)

Delta_y <- as.vector(length(Alpha_t))
for (i in 1:length(Alpha_t)){
  Delta_y[i] = get(Delta_y_chr[i])
}


