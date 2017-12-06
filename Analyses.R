library(multcomp)


Data=read.csv("Ataques.csv",row.names=1)

#Remove arthropod attacks

DataNoBugs=Data[-which(Data$Predator=="Artropodo"),]

#Fit full model

glmLogFull=glm(Attacked~Site*Color,family=binomial(link="logit"),data=DataNoBugs)

print("####### FULL GLM WITH FOR BIRD+MAMMAL ATTACKS #######")

summary(glmLogFull)


### Pairwise comparisons, based on examples explained here https://cran.r-project.org/web/packages/multcomp/vignettes/multcomp-examples.pdf
# The genereal approach here is to create a new dummy variable that represents the interaction between Color and Site. For example, a yellow model placed in Altamira is coded Yellow.Altamira. Then we fit a one-way glm with the dummy explanatory variable and whether a model was attacked, and perform the Tukey HSD pairwise comparisons that we are interested in. In this case, we wanted comparisons between all colors at each site (but not between colors at different sites). 


# Create Dummy variable

DataNoBugs$CbS=with(DataNoBugs, interaction(Color, Site))


#Fit model

Dummy=glm(Attacked~CbS, family=binomial(link="logit"), data=DataNoBugs)


#Make a matrix coding which comparisons we want to make using a TukeyHSD approach. 

#First create a matrix for all possible comparisons. We will pull the comparisons that we want from here. Look at the matrix and pick out the ones you want. 

AllComp=glht(Dummy, linfct = mcp(CbS="Tukey"))$linfct

#Create a vector with the comparisons that you want

comp=c("Red.Altamira - Brown.Altamira","Yellow.Altamira - Brown.Altamira","Yellow.Altamira - Red.Altamira","Red.Los Catios - Brown.Los Catios","Yellow.Los Catios - Brown.Los Catios","Yellow.Los Catios - Red.Los Catios")


#Matrix looks like this at the end. The compared variables are 1 and -1, the rest are 0. 

 #                                 (Intercept) CbSRed.Altamira CbSYellow.Altamira CbSBrown.Los Catios CbSRed.Los Catios	CbS	Yellow.Los Catios
#Red.Altamira - Brown.Altamira                  0               1                  0                   0                 0		0
#Yellow.Altamira - Brown.Altamira               0               0                  1                   0                 0		0
#Yellow.Altamira - Red.Altamira                 0              -1                  1                   0                 0		0
#Red.Los Catios - Brown.Los Catios              0               0                  0                  -1                 1		0
#Yellow.Los Catios - Brown.Los Catios           0               0                  0                  -1                 0		1
#Yellow.Los Catios - Red.Los Catios             0               0                  0                   0                -1		1



## Perform comparisons

print("####### PAIRWISE COMPARISONS FROM FULL GLM FOR BIRD+MAMMAL ATTACKS #######")

summary(glht(Dummy, linfct = AllComp[comp,]),adjusted("fdr"))


#Now see if interaction term actually makes model better by fitting a model with no interaction and comparing likelihoods. 

glmLogNoInt=glm(Attacked~Color+Site,family=binomial(link="logit"),data=DataNoBugs)
#summary(glmLogNoInt)

#Compare them using a likelihood ratio test

print("####### COMPARISON OF FULL GLM AND GLW W/O COLOR x LOCALITY INTERACTION FOR BIRD+MAMMAL ATTACKS #######")

cat(paste('\n',"LRS = ",2*(logLik(glmLogFull)[1]-logLik(glmLogNoInt)[1]),", p = ",1-pchisq(2*(logLik(glmLogFull)[1]-logLik(glmLogNoInt)[1]),df=2),'\n\n', sep=""))



## Power analysis 

library(pwr)

#Make contingency table of red and yellow attacks 

cat("####### POWER ANALYSIS BASED ON A Chi2 DISTRIBUTION #######\nALTAMIRA\n")

redYellowAltamira=table(DataNoBugs[DataNoBugs$Site=="Altamira",c(1,3)])[2:3,]

pwr.chisq.test(w=ES.w2(redYellowAltamira/sum(redYellowAltamira)),df=1,sig.level=0.05,power=0.95)

cat("CATIOS\n")

redYellowCatios=table(DataNoBugs[DataNoBugs$Site=="Los Catios",c(1,3)])[2:3,]

pwr.chisq.test(w=ES.w2(redYellowCatios/sum(redYellowAltamira)),df=1,sig.level=0.05,power=0.95)


#####################################
#### Now repeat with Bird attacks ###	We can reuse our comparison matrix thankfully
#####################################

print("####### FULL GLM WITH FOR BIRD ATTACKS #######")

DataBirds=DataNoBugs[-which(DataNoBugs$Predator=="Mamifero"),]

Bird_glmLogFull=glm(Attacked~Site*Color,family=binomial(link="logit"),data=DataBirds)
summary(Bird_glmLogFull)


DataBirds$CbS=with(DataBirds, interaction(Color, Site))

Bird_Dummy=glm(Attacked~CbS, family=binomial(link="logit"), data=DataBirds)


print("####### PAIRWISE COMPARISONS FROM FULL GLM FOR BIRD ATTACKS #######")

summary(glht(Bird_Dummy, linfct = AllComp[comp,]),adjusted("fdr"))


Bird_glmLogNoInt=glm(Attacked~Color+Site,family=binomial(link="logit"),data=DataBirds)
#summary(Bird_glmLogNoInt)

#Compare them using a likelihood ratio test

print("####### COMPARISON OF FULL GLM AND GLM W/O COLOR x LOCALITY INTERACTION FOR BIRD ATTACKS #######")

cat(paste('\n',"LRS = ",2*(logLik(Bird_glmLogFull)[1]-logLik(Bird_glmLogNoInt)[1]),", p = ",1-pchisq(2*(logLik(Bird_glmLogFull)[1]-logLik(Bird_glmLogNoInt)[1]),df=2),'\n\n', sep=""))

#1-pchisq(2*(logLik(Bird_glmLogFull)[1]-logLik(Bird_glmLogNoInt)[1]),df=2)



########################
### Now with Mammals ###
########################

DataMammals=DataNoBugs[-which(DataNoBugs$Predator=="Ave"),]

print("####### FULL GLM WITH FOR MAMMAL ATTACKS #######")

Mammal_glmLogFull=glm(Attacked~Site*Color,family=binomial(link="logit"),data=DataMammals)
summary(Mammal_glmLogFull)


DataMammals$CbS=with(DataMammals, interaction(Color, Site))

Mammal_Dummy=glm(Attacked~CbS, family=binomial(link="logit"), data=DataMammals)

print("####### PAIRWISE COMPARISONS FROM FULL GLM FOR MAMMAL ATTACKS #######")

summary(glht(Mammal_Dummy, linfct = AllComp[comp,]),adjusted("fdr"))


Mammal_glmLogNoInt=glm(Attacked~Color+Site,family=binomial(link="logit"),data=DataMammals)
#summary(Mammal_glmLogNoInt)

#Compare them using a likelihood ratio test

print("####### COMPARISON OF FULL GLM AND GLM W/O COLOR x LOCALITY INTERACTION FOR MAMMAL ATTACKS #######")

cat(paste('\n',"LRS = ",2*(logLik(Mammal_glmLogFull)[1]-logLik(Mammal_glmLogNoInt)[1]),", p = ",1-pchisq(2*(logLik(Mammal_glmLogFull)[1]-logLik(Mammal_glmLogNoInt)[1]),df=2),'\n\n', sep=""))

#1-pchisq(2*(logLik(Mammal_glmLogFull)[1]-logLik(Mammal_glmLogNoInt)[1]),df=2)


#############################################################
### REPEAT ANALYSES CONSIDERING MISSING MODELS AS ATTACKS ###
#############################################################

# Since we can't assign missing models to any predator group, then we just removed insects and analyzed all attacks together. 

## Figure out how many models are missing from each morph at each place

DataNoBugsMissing=DataNoBugs[,1:5]

nModel=300

missing=nModel-table(Data[,1:2])


for(i in 1:length(missing[,1])){
	for(j in 1:length(missing[1,])){
	
	#print(missing[i,j])
	
	Color=rep(rownames(missing)[i],missing[i,j])
	Site=rep(colnames(missing)[j],missing[i,j])
	Attacked=rep("Attacked",missing[i,j])
	Predator=rep("Missing",missing[i,j])
	Certainty=rep("Certain",missing[i,j])
	DataNoBugsMissing=rbind(DataNoBugsMissing,data.frame(Color,Site,Attacked,Predator,Certainty))
	}
}

#Check that tables match 

table(DataNoBugsMissing[DataNoBugsMissing$Predator=="Missing",1:2])

############################################
#### Now repeat with Missing as attacked ###	We can reuse our comparison matrix thankfully
############################################

print("####### FULL GLM WITH FOR BIRD+MAMMAL ATTACKS + MISSING MODELS #######")


Missing_glmLogFull=glm(Attacked~Site*Color,family=binomial(link="logit"),data=DataNoBugsMissing)
summary(Missing_glmLogFull)


DataNoBugsMissing$CbS=with(DataNoBugsMissing, interaction(Color, Site))

Missing_Dummy=glm(Attacked~CbS, family=binomial(link="logit"), data=DataNoBugsMissing)


print("####### PAIRWISE COMPARISONS FROM FULL GLM FOR BIRD+MAMMAL ATTACKS + MISSING MODELS #######")

summary(glht(Missing_Dummy, linfct = AllComp[comp,]),adjusted("fdr"))


Missing_glmLogNoInt=glm(Attacked~Color+Site,family=binomial(link="logit"),data=DataNoBugsMissing)
#summary(Missing_glmLogNoInt)

#Compare them using a likelihood ratio test

print("####### COMPARISON OF FULL GLM AND GLM W/O COLOR x LOCALITY INTERACTION FOR BIRD+MAMMAL ATTACKS + MISSING MODELS #######")

cat(paste('\n',"LRS = ",2*(logLik(Missing_glmLogFull)[1]-logLik(Missing_glmLogNoInt)[1]),", p = ",1-pchisq(2*(logLik(Missing_glmLogFull)[1]-logLik(Missing_glmLogNoInt)[1]),df=2),'\n\n', sep=""))
