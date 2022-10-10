## This project aim to analyze the influence of the duplicity 
## on the results of meta-meta-analyses (meta-analysis pooling the pooled effect of meta-analyses of primary studies). 

library(dmetar)
library(metafor)
library(metasens)
library(tidyverse)

#Create n effect sizes 
set.seed(101)

#CREATION OF THE SIMULATED DATABASE OF PRIMARY STUDIES
    n=50 #Number of primary studies
    TE <- sample(seq(from = -0.8,to = 0.8, by = 0.01), n, replace = TRUE) 
    #- Simulating a field where effect sizes tend to be positive
    sample <- sample(seq(from = 30,to = 150, by = 1), n, replace = TRUE) #Create n sample size with 30 to 150 participants
    id <- seq(from = 1, to = n) #add ids for the primary studies
    id <- as.factor(id)
    study <- data.frame(id, TE, sample) #Merge primary studies in a database 
    study$varTE <- (((sample)/((sample/2)*(sample/2))) + ((TE^2)/(2*(sample)))) #Calculate variance
    study$seTE <- sqrt(study$varTE) #Calculate standard error

#Generate empty data frames and vectors
    isquared_primary <- c()
    isquared_ur <- c()
    k_primary <- c()
    LL_primary <- c()
    LL_ur <- c()
    maxdupl <- c()
    maxsample <- c()
    meandupl <- c()
    meansample <- c()
    mindupl <- c()
    minsample <- c()
    percentdupl <- c()
    primary <- data.frame()
    primary_n <- c()
    primary_n <- c()
    Qhet_primary <- c()
    Qhet_ur <- c()
    sddupl <- c()
    sdsample <- c()
    tausquared_primary <- c()
    tausquared_ur <- c()
    TE_primary <- c()
    TE_ur <- c()
    UL_primary <- c()
    UL_ur <- c()
    ur <- data.frame()
    ur_seTE <- c()
    ur_TE<- c()


#Loop to generate m primary studies databases for future meta-analyses including each one between q and r primary studies, 
# a database of the primary studies included in the meta-analyses  & the different meta-analyses 
minMA = 3 #Minimal number of meta-analyses per meta-meta-analysis
maxMA = 40 #Minimal number of meta-analyses per meta-meta-analysis
q <- 3 #Minimal number of studies per meta-analysis
r <- 30 #Maximal number of studies per meta-analysis

for(m in minMA:maxMA) { #The loop will create meta-meta-analyses including from minMA to maxMA meta-analyses of primary studies.
  for(i in 1:m) { #Loop to create the m number of meta-analysis of primary studies
            nam <- paste("MA", i, sep = "")#Create a name MAx for the database used for the meta-analysis of primary studies
            x <- assign(nam, 
                sample_n(study, (sample(q:r)))) #Sample a random number of primary studies from the simulated database
            primary <- rbind(primary,x)
            nam2 <- paste("m.gen", i, sep = "") #Create a name m.genx for the meta-analysis of primary studies
            y <- assign(nam2, 
                metagen(TE = TE,
                        seTE = seTE,
                        studlab = id,
                        data = x,
                        sm = "SMD",
                        comb.fixed = FALSE,
                        comb.random = TRUE,
                        method.tau = "REML",
                        hakn = TRUE,
                        title = "MA"))
        
            ur_TE[i] <- y$TE.random[1:i] #Extract Effect sizes of the different meta-analyses for the meta-meta analysis 
            ur_seTE[i] <- y$seTE.random[1:i]  #Extract standard errors of the different meta-analyses for the meta-meta analysis
            primary_n <- c(primary_n, nrow(x))  #Count how many primary studies there are per meta-analysis
                }
            #Mean, SD, min and max of the number of primary studies per meta-analysis
            meansample <- c(meansample,mean(primary_n))
            sdsample<- c(sdsample,sd(primary_n))
            maxsample <- c(maxsample,max(primary_n))
            minsample <- c(minsample,min(primary_n))

            #Create the dataframe for the meta-meta-analysis
            ur <- data.frame(ur_TE,ur_seTE)
            #Add ids for the different meta-analysis
            ur$id <- as_factor(1:i)
            #Meta-meta-analysis
            m.gen_ur <- metagen(TE = ur_TE,
                            seTE = ur_seTE,
                            studlab = id,
                            data = ur,
                            sm = "SMD",
                            comb.fixed = FALSE,
                            comb.random = TRUE,
                            method.tau = "REML",
                            hakn = TRUE,
                            title = "MMA")
            #Examination of the duplicity - How many time each primary studies is repeated in the different meta-analyses? Mean, SD, max and min
            n_occur <- data.frame(table(primary$id))
            meandupl <- c(meandupl,mean(n_occur$Freq))
            sddupl <- c(sddupl,sd(n_occur$Freq))
            maxdupl <- c(maxdupl,max(n_occur$Freq))
            mindupl <- c(mindupl,min(n_occur$Freq))

            primary <- primary[!duplicated(primary$id), ] #Delete the duplicates for the meta-analysis of primary studies

            #Meta-analysis of primary studies
            m.gen_primary <- metagen(TE = TE,
                            seTE = seTE,
                            studlab = id,
                            data = primary,
                            sm = "SMD",
                            comb.fixed = FALSE,
                            comb.random = TRUE,
                            method.tau = "REML",
                            hakn = TRUE,
                            title = "MA primary")

            #Extract k number of primary studies, Q, Tau² and I² for the meta-analysis of primary studies
            k_primary <- c(k_primary, m.gen_primary$k)
            Qhet_primary <- c(Qhet_primary, m.gen_primary$Q)
            tausquared_primary <- c(tausquared_primary, m.gen_primary$tau2)
            isquared_primary <- c(isquared_primary, m.gen_primary$I2)

            #Extract k number of primary studies, Q, Tau² and I² for the meta-meta-analysis
            Qhet_ur <- c(Qhet_ur, m.gen_ur$Q)
            tausquared_ur <- c(tausquared_ur, m.gen_ur$tau2)
            isquared_ur <- c(isquared_ur, m.gen_ur$I2)

            n_occur$Freq <- ifelse(n_occur$Freq == 1,n_occur$Freq == 0,n_occur$Freq)
            percentdupl <- c(percentdupl, (sum(n_occur$Freq) / (nrow(primary)*nrow(ur)))*100)

            TE_primary <- c(TE_primary, m.gen_primary$TE.random)
            LL_primary <- c(LL_primary, m.gen_primary$lower.random)
            UL_primary <- c(UL_primary, m.gen_primary$upper.random)
            TE_ur <- c(TE_ur, m.gen_ur$TE.random)
            LL_ur <- c(LL_ur, m.gen_ur$lower.random)
            UL_ur <- c(UL_ur, m.gen_ur$upper.random)

            
            #Reset vector for the next round of loop (increased number of primary studies per meta-meta-analyses[m])
            ur_TE<- c()
            ur_seTE <- c()
            primary <- data.frame()
            ur <- data.frame()
            primary_n <- c()
}

#PREPARATION OF THE GRAPH DATAFRAME
    #Create dataframes for the ggplot_smooth
    plot_primary <- data.frame(TE_primary, LL_primary,UL_primary)
    colnames(plot_primary) <- c("TE","LL","UL")
    plot_ur <- data.frame (TE_ur, LL_ur,UL_ur)
    colnames(plot_ur) <- c("TE","LL","UL")


    plot_primary$group <- rep("Primary", each=(maxMA-(minMA-1)))
    plot_primary$number <- c(minMA:(maxMA))
    plot_ur$group <- rep("MMA", each=(maxMA-(minMA-1)))
    plot_ur$number <- c(minMA:(maxMA))

    plot <- rbind(plot_primary,plot_ur)

#CREATION OF THE GRAPH
    p <- ggplot(plot,
        aes(x = number,
            y = TE)) +
        geom_point(aes(color=group)) +
        # Add a ribbon with the confidence band
        geom_smooth(
            aes(
                # lower and upper bound of the ribbon
                ymin = LL, ymax = UL,
                fill = group, colour = group
            ), alpha=0.3,
            stat = "identity")
    p <- p + geom_hline(yintercept=0, linetype="longdash", color = "black", size=0.7)
    p <- p + labs(colour="Type of \n Meta-Analysis", fill="Type of \n Meta-Analysis")
    p <- p + ggtitle("Comparison between Meta-Meta-Analyses and PrimaryStudies-only Meta-analysis")
    p <- p + labs(x = "Number of Meta-analyses of Primary Studies", y = "Effect Size")
    p <- p + scale_x_continuous(breaks=seq(3,50,by=1))
    #If necessary to specify from when all the primary studies have been included:  p <- p + geom_ribbon(aes(xmin=30, xmax=50),fill="yellow", alpha=0.15)
    #Change xmin to the m number of meta-analyses per meta-meta-analysis from when "maxsample"=r
    p

    #Save the graph in .tiff format
    tiff(file = "Graph.tiff", width = 3000, height = 3000, compression = "lzw", res = 300)
    p
    dev.off()