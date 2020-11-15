
# TRANSITION EQUATIONS

## Core equations for transitions between compartments:
update(S) <- S - n_SE
update(E) <- E + n_SE - n_Eout
update(I) <- I + n_EoutI - n_Iout
update(A) <- A + n_EoutA - n_AR
update(H) <- H + n_IoutH - n_Hout
update(Q) <- Q + n_HoutQ - n_Qout
update(D) <- D + n_QoutD
update(R) <- R + n_IoutR + n_HoutR + n_QoutR + n_AR

## Htot = H + Q
update(Htot) <- H + Q + n_IoutH - n_HoutR - n_Qout  # Htot represents all in Hospital: Non-ICU + ICU

## Ventilators (tracking as frac of Q, do not go to other compartments)
update(V) <- p_QV*Q            #V + n_QV - n_Vout

## Tracking cumulative numbers in compartments:
update(Idetectcum) <- Idetectcum + n_EoutI
update(Itotcum) <- Itotcum + n_Eout
update(Htotcum) <- Htotcum + n_IoutH   #Htotcum represents cumulative of all in Hospital: Non-ICU + ICU
update(Qcum) <- Qcum + n_HoutQ
update(Vcum) <- p_QV*Qcum      #Vcum + n_QV

## New daily numbers
output(I_detect_new) <- n_EoutI
output(I_tot_new) <- n_Eout
output(H_new) <- n_IoutH
output(Q_new) <- n_HoutQ
output(D_new) <- n_QoutD
#output(d_EI_rand) <- d_EI

#Alpha <- min(.01*I,1) #I_tot_new #.00001*I_tot_new


####################################################################################

# PROBABILITIES

## Individual probabilities of transition:
p_SE <- 1 - exp(-(Beta * (I+A)) / N)                         # S to E
p_Eout <- 1 - exp(-1/d_EI)                               # E to I
p_Iout <- 1 - exp(-((Alpha/d_IH) + ((1-Alpha)/d_IR)))  #exp(-((1/d_IH) + (1/d_IR)))                        # I to H and R
p_Hout <- 1 - exp(-((Kappa/d_HQ) + ((1-Kappa)/d_HR)))  #exp(-((1/d_HQ) + (1/d_HR)))                        # H to Q and R
p_Qout <- 1 - exp(-((Delta/d_QD) + ((1-Delta)/d_QR)))  #exp(-((1/d_QD) + (1/d_QR)))                        # Q to D and R
p_AR <- 1 - exp(-1/d_IR)
#p_Vout <- 1 - exp(-1/d_V)                              # Leaving V



# RANDOM DRAWS FOR NUMBERS CHANGING BETWEEN COMPARTMENTS
## Draws from binomial and multinomial distributions for numbers changing between compartments:

### S to E
n_SE <- rbinom(S, p_SE)

#Alpha <- min(max(.00001 + 1/(n_SE) , .1), 1)

### E to I and A
n_Eout <- rbinom(E, p_Eout)
n_EoutIA[] <- rmultinom(n_Eout, p_EoutIA)
p_EoutIA[1] <- r
p_EoutIA[2] <- 1-r
dim(p_EoutIA) <- 2
dim(n_EoutIA) <- 2
n_EoutI <- n_EoutIA[1]
n_EoutA <- n_EoutIA[2]

### A to R
n_AR <- rbinom(A, p_AR)

### I to H and R
n_Iout <- rbinom(I, p_Iout)                                           # Total no. leaving I
n_IoutHR[] <- rmultinom(n_Iout, p_IoutHR)                             # Divide total no. leaving I into I->H and I->R
p_IoutHR[1] <- Alpha #(Alpha/d_IH)/((Alpha/d_IH) + ((1-Alpha)/d_IR))         # Goes to H and R with relative rates
p_IoutHR[2] <- 1-Alpha #((1-Alpha)/d_IR)/((Alpha/d_IH) + ((1-Alpha)/d_IR))     # 1-p_IoutHR[1]
dim(p_IoutHR) <- 2
dim(n_IoutHR) <- 2
n_IoutH <- n_IoutHR[1]                                                # Total no. I->H
n_IoutR <- n_IoutHR[2]                                                # Total no. I->R

### H to Q and R
n_Hout <- rbinom(H, p_Hout)
n_HoutQR[] <- rmultinom(n_Hout, p_HoutQR)
p_HoutQR[1] <- Kappa #(Kappa/d_HQ)/((Kappa/d_HQ) + ((1-Kappa)/d_HR))
p_HoutQR[2] <- 1-Kappa #((1-Kappa)/d_HR)/((Kappa/d_HQ) + ((1-Kappa)/d_HR))
dim(p_HoutQR) <- 2
dim(n_HoutQR) <- 2
n_HoutQ <- n_HoutQR[1]
n_HoutR <- n_HoutQR[2]

### Q to D and R
n_Qout <- rbinom(Q, p_Qout)
n_QoutDR[] <- rmultinom(n_Qout, p_QoutDR)
p_QoutDR[1] <- Delta #(Delta/d_QD)/((Delta/d_QD) + ((1-Delta)/d_QR))
p_QoutDR[2] <- 1-Delta #((1-Delta)/d_QR)/((Delta/d_QD) + ((1-Delta)/d_QR))
dim(p_QoutDR) <- 2
dim(n_QoutDR) <- 2
n_QoutD <- n_QoutDR[1]
n_QoutR <- n_QoutDR[2]

### Q to V and Vout
#n_QV <- rbinom(Q, p_QV)
#n_Vout <- rbinom(V, p_Vout)

######################################################################

# TOTAL POPULATION SIZE
N <- S + E + I + A + H + Q + D + R

######################################################################

# INITIAL STATES
## Core compartments
initial(S) <- S_ini
initial(E) <- E_ini
initial(I) <- 0
initial(A) <- 0
initial(H) <- 0
initial(Q) <- 0
initial(D) <- 0
initial(R) <- 0
initial(V) <- 0
initial(Htot) <- 0

## Cumulative counts
initial(Idetectcum) <- 0
initial(Itotcum) <- 0
initial(Htotcum) <- 0
initial(Qcum) <- 0
initial(Vcum) <- 0

######################################################################

# USER DEFINED PARAMETERS
## Default in parentheses:

### Initial conditions
S_ini <- user(1e7) # susceptibles
E_ini <- user(10) # infected

### Parameters - random
#d_EI <- runif(3, 8)

### Parameters - fixed
d_EI <- user(5.2)  #days between exposure and infectivity (incubation period)
d_IH <- user(10)   #days between illness onset and hospitalization
d_IR <- user(7)    #days between illness onset and recovery (hospitalization not required)
d_HQ <- user(1)    #days between hospitalization start and ICU
d_HR <- user(12)   #days in hospital (ICU not required)
d_QD <- user(8)    #days in ICU before death (given death)
d_QR <- user(7)    #days in ICU before recovery (given recovery)
#d_V <- user(3)     #days on ventilator (within ICU)

### Parameters - weighted average risk probabilities: input from JAM + population prevalence
#Alpha <- user(0.14)   #probability infected (I) requires hospitalization (vs. recovers)
#Kappa <- user(0.23)   #probability hospitalized (H) requires ICU (vs. recovers)
#Delta <- user(0.06)   #probability ICU (Q) patient dies
p_QV <- user(0.667)   #probability in ICU and requires ventilation
r <- user(0.25)

### Other variables
#R0 <- user(2.2)     #Current estimates from other models

### Parameters - calculated from inputs
#Br <- R0 * ( 1 / ( (r/ ((Alpha/d_IH) + ((1-Alpha)/d_IR)))  + (1-r)*d_IR ))



#########################################
### TIME VARYING BETA (INTERPOLATION) ###
#########################################

Beta <- interpolate(Beta_t, Beta_y,"linear")

Beta_t[] <- user()# R0 * ((Alpha/d_IH)+((1-Alpha)/d_IR))
Beta_y[] <- user()
dim(Beta_t) <- user()
dim(Beta_y) <- user()

####################################################
### TIME VARYING PROBABILITIES OF SEVERE ILLNESS ###
####################################################

Alpha <- interpolate(Alpha_t, Alpha_y,"linear")
Alpha_t[] <- user()# R0 * ((Alpha/d_IH)+((1-Alpha)/d_IR))
Alpha_y[] <- user()
dim(Alpha_t) <- user()
dim(Alpha_y) <- user()

Kappa <- interpolate(Kappa_t, Kappa_y,"linear")
Kappa_t[] <- user()# R0 * ((Alpha/d_IH)+((1-Alpha)/d_IR))
Kappa_y[] <- user()
dim(Kappa_t) <- user()
dim(Kappa_y) <- user()

Delta <- interpolate(Delta_t, Delta_y,"linear")
Delta_t[] <- user()# R0 * ((Alpha/d_IH)+((1-Alpha)/d_IR))
Delta_y[] <- user()
dim(Delta_t) <- user()
dim(Delta_y) <- user()
