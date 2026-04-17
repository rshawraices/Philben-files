#Using all variables

Aerobicmodel<-lm(Aerobic.CO2~ Depth + pH + C.N + TotalInorganicN+Rha_C+ X.CasSugars+ X.Glucose+
                   X.Rhamnose+X..NasAA+AADegIndex+X..CasAA + Mol.Hydroxyproline+
                   Mol.Glycine..w.o.Norvaline. + CNormYieldHydroxyproline, data=PCA.data)
summary(Aerobicmodel)

Anaerobicmodel<-lm(Anaerobic.CO2~Depth + pH + C.N + TotalInorganicN+Rha_C+ X.CasSugars+ X.Glucose+
                   X.Rhamnose+X..NasAA+AADegIndex+X..CasAA + Mol.Hydroxyproline+
                   Mol.Glycine..w.o.Norvaline. + CNormYieldHydroxyproline, data=PCA.data)
summary(Anaerobicmodel)

CH4model<-lm(CH4~Depth + pH + C.N + TotalInorganicN+Rha_C+ X.CasSugars+ X.Glucose+
                   X.Rhamnose+X..NasAA+AADegIndex+X..CasAA + Mol.Hydroxyproline+
                   Mol.Glycine..w.o.Norvaline. + CNormYieldHydroxyproline, data=PCA.data)
summary(CH4model)


Aerobicmodel<-lm(Aerobic.CO2~pH + C.N + TotalInorganicN+ X.CasSugars+ X.Glucose+
                   X.Rhamnose+X..NasAA+AADegIndex+X..CasAA, data=PCA.data)
summary(Aerobicmodel)

#Using only significant variables
Aerobicmodel<-lm(Aerobic.CO2~Depth + pH + TotalInorganicN + X.Glucose
                   , data=PCA.data)
summary(Aerobicmodel)

Anaerobicmodel<-lm(Anaerobic.CO2~C.N +X..NasAA+X..CasAA, data=PCA.data)
summary(Anaerobicmodel)

CH4model<-lm(CH4~Depth+TotalInorganicN+X..CasAA + Mol.Hydroxyproline
             + CNormYieldHydroxyproline, data=PCA.data)
summary(CH4model)
