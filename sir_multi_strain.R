## Load deSolve package
library(deSolve)


## Define as a function
epi203 <- function(pars) {

    ## Show parameters
    print(pars)

    ## Additional parameters
    times <- seq(from = 0, to = 600, by = 1)              # we want to run the model for 3000 time steps
    yinit <- c(Susc = 0.7, Infected1 = 0.1, Infected2 = 0.2, Recovered = 0) # this parameter sets the initial conditions

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

test.pars <- c(beta1 = 0.3, recovery1 = 0.1, virus_death1 = 0.002, beta2 = 0.2, recovery2 = 0.1, virus_death2 = 0.005, loss_of_immunity = 0.3, natural_death = 0.001, birth = 0.001)
results   <- epi203(test.pars)

##############################################################################
## Plotting
matplot(results[, 1], results[, 2:5], type="l", lty=1)
legend("topright", col=1:4, legend=c("S", "I1", "I2", "R"), lwd=1)
