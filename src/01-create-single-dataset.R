


source("0-config.R")



bd <- load_MICS_dataset("Bangladesh")
cg <- load_MICS_dataset("Congo")
ki <- load_MICS_dataset("Kiribati")
laPDR <- load_MICS_dataset("LaoPDR")
le <- load_MICS_dataset("Lesotho")
md <- load_MICS_dataset("Madagascar")
mo <- load_MICS_dataset("Mongolia")
np <- load_MICS_dataset("Nepal")
ni <- load_MICS_dataset("Nigeria")
pakPun <- load_MICS_dataset("PakistanPunjab")
par <- load_MICS_dataset("Paraguay")
SL <- load_MICS_dataset("SierraLeone")
sur <- load_MICS_dataset("Suriname")
tg <- load_MICS_dataset("Togo")
tun <- load_MICS_dataset("Tunisia")
ze <- load_MICS_dataset("Zimbabwe")
bd13 <- load_MICS_dataset("Bangladesh2013")
CI <- load_MICS_dataset("CoteIvoire")
#DRC <- load_MICS_dataset("DRC") missing cleaned dataset
gb <- load_MICS_dataset("Gambia")
ga <- load_MICS_dataset("Georgia")
gh <- load_MICS_dataset("Ghana")

ls()
d <- bind_rows(bd, bd13, cg,CI,ga,gb,
     gh,ki,laPDR, 
     #DRC, 
     le,md,mo,          
     ni, np, pakPun, par, SL,              
     sur, tg, tun, ze)
dim(d)

saveRDS(d, here("data/compiled_raw_MICS_survey.rds"))
