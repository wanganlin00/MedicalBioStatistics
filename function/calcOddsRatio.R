calcOddsRatio <- function(mymatrix,alpha=0.05,referencerow=2,quiet=FALSE)
{
    numrow <- nrow(mymatrix)
    myrownames <- rownames(mymatrix)
    
    for (i in 1:numrow)
    {
        rowname <- myrownames[i]
        DiseaseUnexposed <- mymatrix[referencerow,1]
        ControlUnexposed <- mymatrix[referencerow,2]
        if (i != referencerow)
        {
            DiseaseExposed <- mymatrix[i,1]
            ControlExposed <- mymatrix[i,2]
            
            totExposed <- DiseaseExposed + ControlExposed
            totUnexposed <- DiseaseUnexposed + ControlUnexposed
            
            probDiseaseGivenExposed <- DiseaseExposed/totExposed
            probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
            probControlGivenExposed <- ControlExposed/totExposed
            probControlGivenUnexposed <- ControlUnexposed/totUnexposed
            
            # calculate the odds ratio
            oddsRatio <- (probDiseaseGivenExposed*probControlGivenUnexposed)/
                (probControlGivenExposed*probDiseaseGivenUnexposed)
            if (quiet == FALSE)
            {
                print(paste("category =", rowname, ", odds ratio = ",oddsRatio))
            }
            
            # calculate a confidence interval
            confidenceLevel <- (1 - alpha)*100
            sigma <- sqrt((1/DiseaseExposed)+(1/ControlExposed)+
                              (1/DiseaseUnexposed)+(1/ControlUnexposed))
            # sigma is the standard error of our estimate of the log of the odds ratio
            z <- qnorm(1-(alpha/2))
            lowervalue <- oddsRatio * exp(-z * sigma)
            uppervalue <- oddsRatio * exp( z * sigma)
            if (quiet == FALSE)
            {
                print(paste("category =", rowname, ", ", confidenceLevel,
                            "% confidence interval = [",lowervalue,",",uppervalue,"]"))
            }
        }
    }
    if (quiet == TRUE && numrow == 2) # If there are just two treatments (exposed/nonexposed)
    {
        return(oddsRatio)
    }
}