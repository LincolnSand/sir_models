# Our differential equation
diff <- function(x,y)
{
    return(x/y) #Try also x+y
}

# Line function
TheLine <- function(x1,y1,slp,d)
{
    z = slope*(d-x1)+y1
    return(z)
}

# Domains
x = seq(-20,20,0.5)
y = seq(-20,20,0.5)

# Points to draw our graph
f = c(-5,5) 
h = c(-5,5)
plot(f,h,main="Slope field")


# Let's generate the slope field
for(j in x)
{
    for(k in y)
    {
        slope = diff(j,k)
        domain = seq(j-0.07,j+0.07,0.14)
        z = TheLine(j,k,slope,domain)
        arrows(domain[1],z[1],domain[2],z[2],length=0.08)
    }
}
