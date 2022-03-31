## Load deSolve package
library(deSolve)


## Define as a function
epi203 <- function(pars) {

    ## Show parameters
    print(pars)

    ## Additional parameters
    times <- seq(from = 0, to = 600, by = 1)              # we want to run the model for 3000 time steps
    yinit <- c(Susc = 0.8, Infected1 = 0.1, Infected2 = 0.1, Recovered = 0) # this parameter sets the initial conditions

    ## below is the code for the actual model including the equations that you should recognize
    SIR_model <- function(times, yinit, pars){

    with(as.list(c(yinit,pars)), {
            dSusc <- birth - natural_death*Susc - beta1*Infected1*Susc - beta2*Infected2*Susc + loss_of_immunity*Recovered
            dInfected1  <- beta1*Infected1*Susc - recovery1*Infected1 - virus_death1*Infected1 - natural_death*Infected1
            dInfected2 <- beta2*Infected2*Susc - recovery2*Infected2 - virus_death2*Infected2 - natural_death*Infected2
            dRecovered <- recovery1*Infected1 + recovery2*Infected2 - natural_death*Recovered - loss_of_immunity*Recovered

            return(list(c(dSusc, dInfected1, dInfected2, dRecovered)))
        })
    }

    ## run the ode solver for the function specified (function defined above is used)
    ## return the value of each compartment (Susc, Infected, Recovered) for each time step.
    results <- ode(func = SIR_model, times = times, y = yinit, parms = pars)
    results <- as.data.frame(results)

    ## Return result
    return(results)
}

##############################################################################

death_rate_c = 0.01

# Define the parameters for days infected as a function of virulence
n = 6
w = 8
c = 0.005
r = 200000



R0_1 = 3.0
virus_death1 = death_rate_c/R0_1
# https://www.desmos.com/calculator/ymr7o1cmye
days_infected1 = exp(-r*(virus_death1-c)*(virus_death1-c))*w + n
recover_rate1 = 1/days_infected1
beta1 = R0_1*recover_rate1

R0_2 = 2.1
virus_death2 = death_rate_c/R0_2
days_infected2 = exp(-r*(virus_death2-c)*(virus_death2-c))*w + n
recover_rate2 = 1/days_infected2
beta2 = R0_2*recover_rate2

test.pars <- c(beta1 = beta1, recovery1 = recover_rate1, virus_death1 = virus_death1, beta2 = beta2, recovery2 = recover_rate2, virus_death2 = virus_death2, loss_of_immunity = 0.3, natural_death = 0.001, birth = 0.001)
results   <- epi203(test.pars)

print(days_infected1)
print(days_infected2)

##############################################################################
## Plotting
matplot(results[, 1], results[, 2:5], type="l", lty=1)
legend("topright", col=1:4, legend=c("S", "I1", "I2", "R"), lwd=1)
