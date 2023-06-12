library(ResistanceGA)
load(here::here("data","NewlyCreatedData","Cover_rasters.RData"))

data(samples)

#get group position in the landscape and transform it to a matrix
dat<-data.frame(Group=1:length(loc$Group),st_coordinates(loc[,1]))
mati<-as.matrix(dat,ncol=3)

#prepare pbject
sample.locales <- SpatialPoints(mati[, c(2, 3)])
gdist.inputs <- gdist.prep(n.Pops = length(sample.locales),
                           samples = sample.locales,
                           method = 'costDistance')

resistance_surfaces<-stack(coverStack[c(1:10)])

GA.inputs <- GA.prep(method = "LL",
                     ASCII.dir = resistance_surfaces,
                     Results.dir = "all_comb",
                     max.cat = 500,
                     max.cont = 500,
                     seed = 123,
                     parallel = 6)


PARM <- c(1, 10,50,500,250) #1=forest; 2=non-forest; 3=crops; 4=water; 5=buildings


Resist <- Combine_Surfaces(PARM = PARM,
                           gdist.inputs = gdist.inputs,
                           GA.inputs = GA.inputs,
                           out = NULL,
                           rescale = TRUE,
                           p.contribution = TRUE)
# View transformed composite (truth)
plot(Resist$combined.surface,
     main = "True resistance surface")

