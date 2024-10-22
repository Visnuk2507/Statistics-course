
###########################################################################
## Sæt working directory

## I RStudio kan man nemt sætte working directory med menuen 
## "Session -> Set Working Directory -> To Source File Location" 
## Bemærk: i R bruges kun "/" til separering i stier 
## (altså ingen backslash).
setwd("Erstat her med stien til den mappe, hvor projektfilerne er gemt.")


###########################################################################
## Indlæs data

## Indlæs data fra finans1_data.csv
D <- read.table("finans1_data.csv", header=TRUE, sep=";", as.is=TRUE)

## Udvælg tids-variablen samt ETF'erne AGG, VAW, IWN og SPY
D <- D[ ,c("t","AGG","VAW","IWN","SPY")]


###########################################################################
## Simpel opsummering af data

## Dimensionen af D (antallet af rækker og søjler)
dim(D)
## Standardafvigelse
sd(D)
## Søjle-/variabelnavne
names(D)
## De første rækker/observationer
head(D)
## De sidste rækker/observationer
tail(D)
## Udvalgte opsummerende størrelser
summary(D)
## En anden type opsummering af datasættet
str(D)


###########################################################################
## Histogram (empirisk tæthed)

## Histogram der beskriver den empiriske tæthed for afkastene for AGG
## (histogram for AGG-afkast normaliseret så arealet er lig 1)
hist(D$AGG, xlab="Afkast (AGG)", prob=TRUE)
mu <- mean(D$AGG)
sigma <- sd(D$AGG)
curve (dnorm(x, mean=mu, sd=sigma), col="red", lwd=2, add=TRUE)


###########################################################################
## Konverter variabel til dato

## Konverterer variablen 't' til en dato-variabel i R
D$t <- as.Date(x=D$t, format="%Y-%m-%d")
## Tjekker resultatet
summary(D$t)


###########################################################################
## Plot af udvikling over tid

## Plot af ugentligt afkast over tid for hver af de 4 ETF'er
y=c(-0.2,0.2)
## Plot af det ugentlige afkast for ETF'en AGG over tid
plot(D$t, D$AGG, type="l", ylim=y, xlab="Tid", ylab="Afkast (AGG)")
## Tilsvarende plots for de tre andre ETF'er
plot(D$t, D$VAW, type="l", ylim=y, xlab="Tid", ylab="Afkast (VAW)")
plot(D$t, D$IWN, type="l", ylim=y, xlab="Tid", ylab="Afkast (IWN)")
plot(D$t, D$SPY, type="l", ylim=y, xlab="Tid", ylab="Afkast (SPY)")


###########################################################################
## Boxplot opdelt efter ETF

## Boxplot af afkast opdelt efter ETF
boxplot(D$AGG, D$VAW, D$IWN, D$SPY, names=c("AGG", "VAW", "IWN", "SPY"),
        xlab="ETF", ylab="Afkast")
abline(h=0.1, col="red", lty=2)
abline(h=0.0, col="red", lty=2)
abline(h=-0.1, col="red", lty=2)

###########################################################################
## Opsummerende størrelser for afkastet af de fire ETF'er.

## Antal observationer af AGGs, VAWs, IWNs og SPYs afkast.
sum(!is.na(D$AGG))
sum(!is.na(D$VAW))
sum(!is.na(D$IWN))
sum(!is.na(D$SPY))

## Stikprøvegennemsnittet for AGGs, VAWs, IWNs og SPYs afkast.
mean(D$AGG, na.rm=TRUE)
mean(D$VAW, na.rm=TRUE)
mean(D$IWN, na.rm=TRUE)
mean(D$SPY, na.rm=TRUE)

## Stikprøvevariansen for AGGs, VAWs, IWNs og SPYs afkast.
var(D$AGG, na.rm=TRUE)
var(D$VAW, na.rm=TRUE)
var(D$IWN, na.rm=TRUE)
var(D$SPY, na.rm=TRUE)

## Stikprøvestandafvigelsen for AGGs, VAWs, IWNs og SPYs afkast.
sd(D$AGG, na.rm=TRUE)
sd(D$VAW, na.rm=TRUE)
sd(D$IWN, na.rm=TRUE)
sd(D$SPY, na.rm=TRUE)

## Nedre- og øvre kvartil, samt median for AGGs, VAWs, IWNs og SPYs afkast.
quantile(D$AGG, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(D$VAW, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(D$IWN, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(D$SPY, probs=c(0.25,0.5,0.75), na.rm=TRUE)

## Tabel med følgende værdier er lavet og indskrevet i Word.

## Argumentet 'na.rm=TRUE' sørger for at størrelsen
## udregnes selvom der eventuelt er manglende værdier


###########################################################################
## qq-plot til modelkontrol

## qq-plot for AGG afkast
qqnorm(D$AGG)
qqline(D$AGG)


qqnorm(D$VAW)
qqline(D$VAW)


qqnorm(D$IWN)
qqline(D$IWN)


qqnorm(D$SPY)
qqline(D$SPY)

## Ni QQ-plots
par(mfrow=c(3,3))

etfs <- list(D$AGG, D$VAW, D$IWN, D$SPY)
etf_names <- c("D$AGG", "D$VAW", "D$IWN", "D$SPY")

for (i in 1:9) {
  if (i <= 4) {
    qqnorm(etfs[[i]], main=etf_names[i])
    qqline(etfs[[i]])
  } else {
    simulated_data <- rnorm(length(etfs[[1]]))
    qqnorm(simulated_data, main=paste("Simuleret Data", i-4))
    qqline(simulated_data)
  }
}


###########################################################################
## Konfidensinterval for middelværdi

## Konfidensinterval for middelafkast for AGG
t.test(D$AGG, conf.level=0.95)$conf.int
t.test(D$VAW, conf.level=0.95)$conf.int
t.test(D$IWN, conf.level=0.95)$conf.int
t.test(D$SPY, conf.level=0.95)$conf.int

## Teststørrelsens fordeling
qt(0.975, 453)

## AAGs konfidensinterval udregnet med formlen
0.000265757+1.97*(0.005976/sqrt(454))
0.000265757-1.97*(0.005976/sqrt(454))

###########################################################################
## T-test for en enkelt stikprøve

## Test af hypotesen mu=0 for AGG afkast
t.test(D$AGG, mu=0)


###########################################################################
## Welch t-test for sammenligning af to (uafhængige) stikprøver

## Sammenligning af afkast for VAW og AGG
t.test(D$VAW, D$AGG)

## Teststørrelsens fordeling
qt(0.975, 478)

###########################################################################
## Beregning af korrelation

## Beregning af korrelation mellem udvalgte ETF'er
cor(D[ ,c("AGG","VAW","IWN","SPY")], use="pairwise.complete.obs")

## Scatterplot
plot(D$VAW, D$IWN, main = "Scatterplot for VAW og IWN", xlab = "Afkast fra VAW", ylab = "Afkast fra IWN")
abline(lm(D$IWN ~ D$VAW), col = "red")

###########################################################################
## Delmængder i R

## Ekstra bemærkning om måder at udtage delmænger i R
##
## En logisk (logical) vektor med sandt (TRUE) eller falsk (FALSE) for 
## hver række i D - f.eks: 
## De uger hvor der er tab (negativ afkast) på AGG
D$AGG < 0
## Vektoren kan bruges til at udvælge alle de negative AGG afkast
D$AGG[D$AGG < 0]
## Alternativt kan man bruge funktionen 'subset'
subset(D, AGG < 0)
## Mere komplekse logiske udtryk kan laves, f.eks.:
## Find alle observationer fra 2009
subset(D, "2009-01-01" < t & t < "2010-01-01")


###########################################################################
## Flere R-tips

## Lav en for-løkke med beregning af et par opsummerende størrelser
## og gem resultatet i en ny data.frame
num <- 2:5
Tbl <- data.frame()
for(i in num){
  Tbl[i-1,"mean"] <- mean(D[ ,i])
  Tbl[i-1,"var"] <- var(D[ ,i])
}
row.names(Tbl) <- names(D)[num]
## Se hvad der er i Tbl
Tbl

## I R er der endnu mere kortfattede måder sådanne udregninger kan 
## udføres. For eksempel
apply(D[, num], 2, mean, na.rm=TRUE)
## eller flere ad gangen i et kald
apply(D[, num], 2, function(x){
  c(mean=mean(x, na.rm=TRUE),
    var=var(x, na.rm=TRUE))
})
## Se flere smarte funktioner med: ?apply, ?aggregate og ?lapply
## og for ekstremt effektiv databehandling se f.eks. pakkerne: dplyr,
## tidyr, reshape2 og ggplot2.

## LaTeX tips:
##
## R-pakken "xtable" kan generere LaTeX tabeller og skrive dem direkte 
## ind i en fil, som derefter kan inkluderes i et .tex dokument.
## 
## R-pakken "knitr" kan anvendes meget elegant til at lave et .tex 
## dokument der inkluderer R koden direkte i dokumentet. Dette 
## dokument og bogen er lavet med knitr.

