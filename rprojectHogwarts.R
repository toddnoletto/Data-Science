
#########

mylag = function(x, lags){
    l = length (lags)
    result +matrix(NA, length(x), 1 +l)
    result[, 1]= x
    for(i in 1:length(lags){
        result[(1 + lags[i]):length(x), 1 +i] = x[1:(length(x)-lags[i])]
    }
    result
}


#Hogwarts
Hogwarts = function(){
    
        gry = 0
        sly = 0
        rav = 0
        
        continue = TRUE
        while (continue) {
            q1 = readline(prompt = "Are you brave (y/n >")
            if (q1 == "y") {
                gry = gry + 1
                continue = FALSE
            }
            else if (q1 == "n"){
                sly = sly + 1
                continue = FALSE
            }
            else print("Please put yes or no")
        }
        continue = TRUE
        while (continue) {
            q = readline(prompt = "Are you cunning (y/n >")
            if (q == "n") {
                gry = gry + 1
                continue = FALSE
            }
            else if (q == "y"){
                sly = sly + 1
                continue = FALSE
            }
            else print("Please put yes or no")
}

            if (gry > sly){
                print("you're in Griffindore")
                return("gry")
            }
            if (sly > gry) {
                print("you're in slytherin")
                return("sly")
            } 
            else {
                print("you're in Ravenclaw")
                return("Ravenclaw")
            }



#Power Function (raises an integer to power)
pow = function(x, p){
    if (p == 0) return(1)
    else{
       return(x + pow(x, p - 1))
    }
}

lagutil =function(x, la){
    (if !is.vector(x)) stop("x must be a vecto")
    if (la < 0) stop ("la must be bigger than 1")
    if (la == 0) return(x)
    else {
        n = length(x)
        xnew= c(NA, x[-n])
        return(lagutil(xnew, 1a -1))
    }
}

chewie = readLines("https://en.wikipedia.org/wiki/Chewbacca")


install.packages("ggplot2")

corner = matrix(1 : 16, 4, 4)

corner= matrix(c(1 : 4, 1 : 4))

corner= matrix(c(1 , 4 : 1, 4))

corner = matrix(1 : 16, 4, 4)

corner = function(x=matrix(4, 4 : 4, 4))
                 
corner = function (x){
    rval= c(x, (a = matrix(1:16, 4, 4))
    return(rval)
}
corner(a, c(2, 3))

matrix(1:16, nrow=4, ncol=4)
(a, c(2, 3))
