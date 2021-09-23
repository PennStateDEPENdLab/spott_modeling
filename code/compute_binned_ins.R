#compute binned behavior on ins blocks
setwd("~/Box/DEPENd/PANDAA/Data/Raw/Subject ID folders")
insfiles <- grep("_bad", list.files(pattern=".*insblock.csv", recursive=TRUE), value = TRUE, invert = TRUE)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

allins <- c()
allold <- c()
for (f in insfiles) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  id <- as.numeric(sub(".*/(\\d+)_insblock.csv", "\\1", f, perl=TRUE))
  df$id <- id
  #if (! "trialstart_vblt" %in% names(df) ) {
  if ("buttonA" %in% names(df) ) { #this seems to be the right differentiator
    message("ID ", id, " appears to be old SPOTT (pre 2018)")
    allold <- bind_rows(allold, df)
  } else {
    allins <- bind_rows(allins, df)
  }
}

table(allins$duration)
has3 <- filter(allins, key=="3#") #none (good)
table(allins$key) #yes just 1/2

#sanity checks on the data
table(allins$prewardA) #yes, all at 0.2
table(allins$positionB) #I guess there's no real counterbalancing if we have equal probabilities... looks like at least one subj still had the 1/2/3 nomenclature
table(allins$positionA) #I guess there's no real counterbalancing if we have equal probabilities... 
table(allins$duration) #good
hist(allins$ITI) #reasonably exponential
summary(allins$ITI)
table(allins$insblock) #good, one block
table(allins$instrial) #good, one block

#handle the missing key problem. This occurs when the person presses two keys at once.
filter(allins, key=="") %>% select(key, outcome) %>% group_by(key) %>% count(outcome)
allins <- allins %>% group_by(id, instrial) %>% mutate(
  nextkey=lead(key, order_by = eventNumber, n = 1),
  prevkey=lag(key, order_by = eventNumber, n = 1)
) %>% ungroup() %>% arrange(id, instrial, eventNumber)

#looks accurate
allins %>% select(id, instrial, eventNumber, key, nextkey, prevkey, outcome) %>% print(n=50)

#as long as the next and preceding keys match, assume that the missing key is more of the same (i.e., not a switch)
allins <- allins %>% mutate(key=if_else(key=="" & nextkey==prevkey, nextkey, key))

#still 12 unaccounted for -- come back to this
filter(allins, key=="") %>% select(id, instrial, eventNumber, key, nextkey, prevkey, outcome)
filter(allins, is.na(outcome)) %>% select(id, instrial, eventNumber, key, nextkey, prevkey, outcome)

#pull the -1 from outcome and create a new switch variable
allins <- allins %>% filter(key!="") %>% mutate(switch=if_else(outcome==-1, 1, 0), outcome=if_else(outcome==-1, 0, outcome))

allins %>% group_by(id) %>% dplyr::summarize(sum(outcome, na.rm=T)) %>% print(n=100) #generally hitting the ~72 target. Variation reflects that we don't terminate the trial immediately (give full 6s)
allins %>% group_by(id) %>% dplyr::summarize(sumrewards=sum(outcome, na.rm=T)) %>% psych::describe() #generally hitting the ~72 target. Variation reflects that we don't terminate the trial immediately (give full 6s)

prop.table(table(allins$outcome)) #0 = no win, 1 = win, -1 = switch. Looks like we get the 80/20 population probabilities (that's good!)
prop.table(table(allins$switch)) #5% switches

#time binning
filter(allins, latency > 6000) %>% count() #very occasional allowance of > 6000ms due to flip loop
allins <- allins %>% mutate(latency=case_when(
  latency > 6000 ~ 6000,
  latency < 100 ~ NA_real_, #dplyr enforces data type
  TRUE ~ latency
))

bins_300 <- seq(0, 6000, by=300) #300ms bins
bins_400 <- seq(0, 6000, by=400) #400ms bins
bins_50 <- seq(0, 6000, by=50) #50ms bins
#need to know how often a switch happens.
allins_binned_300 <- allins %>% mutate(key=substr(key,1,1), latency_bin=Hmisc::cut2(latency, bins_300)) %>% 
  group_by(id, instrial, latency_bin, key) %>%
  dplyr::summarize(binmedian=median(latency), npress=n(), nreward=sum(outcome)) %>% ungroup()

allins_binned_400 <- allins %>% mutate(key=substr(key,1,1), latency_bin=Hmisc::cut2(latency, bins_400)) %>% 
  group_by(id, instrial, latency_bin, key) %>%
  dplyr::summarize(binmedian=median(latency), npress=n(), nreward=sum(outcome)) %>% ungroup()

#wrap n() in as.numeric() because mutate_at below blows up otherwise.
allins_binned_50 <- allins %>% mutate(key=as.numeric(substr(key,1,1)), latency_bin=Hmisc::cut2(latency, bins_50)) %>% 
  group_by(id, instrial, latency_bin, key) %>%
  dplyr::summarize(binmedian=median(latency), npress=as.numeric(n()), nreward=sum(outcome), switch=sum(switch)) %>% ungroup()

#need to fill in all bins, keeping a zero for no press
master_bins_50 <- expand.grid(id=unique(allins$id), instrial=unique(allins$instrial), 
                              latency_bin=unique(allins_binned_50$latency_bin))
                              #latency_bin=unique(Hmisc::cut2(1:6000, bins_50)))

#Because ins is variable length, need to drop trials that did not occur for a given subject
#these result because the expand.grid assumes all subjects have the same setup
allins_binned_50 <- left_join(master_bins_50, allins_binned_50, by=c("id", "instrial", "latency_bin")) %>%
  group_by(id, instrial) %>% filter(sum(npress, na.rm=TRUE) > 0) %>% ungroup() %>%
  arrange(id, instrial, latency_bin) %>% filter(!is.na(latency_bin)) %>%
  mutate_at(vars(key, npress, nreward, switch), list(~if_else(is.na(.), 0, .)))

#note some subject don't have presses in the first trials
#xtabs(~id + instrial, allins_binned_combined)

#leads to a nice-looking Poisson distribution with a mode of 1
lattice::histogram(~npress | key, allins_binned_300)
lattice::histogram(~nreward | key, allins_binned_300)

lattice::histogram(~npress | key, allins_binned_400)
lattice::histogram(~nreward | key, allins_binned_400)

lattice::histogram(~npress | key, allins_binned_50)
lattice::histogram(~nreward | key, allins_binned_50)

prop.table(xtabs(~nreward + key, allins_binned_50), margin=2)

#get a sense of press statistics at this bin size
xtabs(~npress + key + latency_bin, allins_binned_300)
xtabs(~nreward + key + latency_bin, allins_binned_300)

allins_binned_300_wide <- allins_binned_300 %>% gather(key="variable", value="value", binmedian, npress, nreward) %>%
  reshape2::dcast(id + instrial + latency_bin ~ variable + key) %>% 
  mutate_at(vars(npress_1, npress_2, nreward_1, nreward_2), funs(if_else(is.na(.), 0, .))) %>%
  arrange(id, instrial, latency_bin)

allins_binned_400_wide <- allins_binned_400 %>% gather(key="variable", value="value", binmedian, npress, nreward) %>%
  reshape2::dcast(id + instrial + latency_bin ~ variable + key) %>% 
  mutate_at(vars(npress_1, npress_2, nreward_1, nreward_2), funs(if_else(is.na(.), 0, .))) %>%
  arrange(id, instrial, latency_bin)

allins_binned_50_wide <- allins_binned_50 %>% gather(key="variable", value="value", binmedian, npress, nreward) %>%
  reshape2::dcast(id + instrial + latency_bin + switch ~ variable + key) %>% 
  mutate_at(vars(npress_1, npress_2, nreward_1, nreward_2), funs(if_else(is.na(.), 0, .))) %>%
  arrange(id, instrial, latency_bin) %>% select(-npress_0, -nreward_0, -binmedian_0)


prop.table(table(allins_binned_300_wide$nreward_1))
prop.table(table(allins_binned_300_wide$nreward_2))

#how often does a switch occur in a bin? 6% at 400ms, 3% at 300ms
allins_binned_300_wide %>% filter(npress_1 > 0 & npress_2 > 0) %>% tally()
nrow(allins_binned_300_wide)

allins_binned_400_wide %>% filter(npress_1 > 0 & npress_2 > 0) %>% tally()
nrow(allins_binned_400_wide)

allins_binned_50_wide %>% filter(npress_1 > 0 & npress_2 > 0) %>% tally()
nrow(allins_binned_50_wide)

#for Zita: 1) an active key variable on any trial; 2) time elapsed since previous press
library(gsubfn)

# bincenter <- unlist(strapply(as.character(allins_binned_50$latency_bin), ("\\[\\s*(\\d+),\\s*(\\d+)"), function(low, high) { 
#     as.numeric(low) + (as.numeric(high) - as.numeric(low)) / 2
#   }))

#hacky way to get bin center after using cut above
allins_binned_50 <- allins_binned_50 %>% mutate(bincenter=unlist(strapply(as.character(latency_bin), ("\\[\\s*(\\d+),\\s*(\\d+)"), function(low, high) { 
  as.numeric(low) + (as.numeric(high) - as.numeric(low)) / 2
})))

allins_binned_50 <- allins_binned_50 %>% group_by(id, instrial) %>% do({
  df <- . #for some reason, using . throughout results in returning the same data.frame repeatedly (loses grouping structure)
  df$curkey <- NA_integer_
  df$tdiff <- NA_real_
  
  curkey <- 0
  lastrt <- 0 #the start of the trial (time = 0) is treated as last rt. thus, tdiff accumulates up to first button press
  
  #2020: remove clairvoyance of model in giving it subject's first choice. Otherwise, model predicts ramping up toward the choice
  #firstkey <- df$key[df$key > 0][1] #give the model the first press (i.e., give the model a bit of clairvoyance wrt the subject's first choice)
  #curkey <- firstkey
  for (i in 1:nrow(df)) {
    if (df$key[i] != 0 && df$key[i] != curkey ) {
      curkey <- df$key[i]
    }
    
    if (!is.na(df$binmedian[i])) {
      rt_k <- lastrt
      lastrt <- df$binmedian[i] #now update the running tracker of last response
      tau <- lastrt #use the last rt versus this rt as the tdiff in a response bin
    } else {
      tau <- df$bincenter[i]
      rt_k <- lastrt
    }
    
    df$curkey[i] <- curkey
    df$tdiff[i] <- tau - rt_k
  }
  
  df
}) %>% ungroup()


# inspect IRTs

allins <- allins %>% group_by(id, instrial) %>% mutate(irt=c(NA, diff(latency))) %>% ungroup()

hist(filter(allins, irt<600) %>% pull(irt),50)

pdf(file = "spott_irt_density_by_sub.pdf", width = 6, height = 6)
ggplot(filter(allins, irt<500), aes(x = irt)) + geom_density(adjust = 2) + facet_wrap(~id)
dev.off()

outdir <- "~/Data_Analysis/spott_modeling/data"
write.csv(allins_binned_300, file=file.path(outdir, "allins_binned_300_long.csv"), row.names=FALSE)
write.csv(allins_binned_300_wide, file=file.path(outdir, "allins_binned_300_wide.csv"), row.names=FALSE)

write.csv(allins_binned_400, file=file.path(outdir, "allins_binned_400_long.csv"), row.names=FALSE)
write.csv(allins_binned_400_wide, file=file.path(outdir, "allins_binned_400_wide.csv"), row.names=FALSE)

write.csv(allins_binned_50, file=file.path(outdir, "pandaa_allins_binned_50_long.csv"), row.names=FALSE)
write.csv(allins_binned_50_wide, file=file.path(outdir, "allins_binned_50_wide.csv"), row.names=FALSE)

#per subject splits for VBA
out_dir <- "/Users/mnh5174/Data_Analysis/spott_modeling/data/pandaa_vba_input"
if (!dir.exists(out_dir)) { dir.create(out_dir) }
dsplit <- split(allins_binned_50, allins_binned_50$id)
sapply(1:length(dsplit), function(d) {
  id <- names(dsplit)[d]
  data <- dsplit[[d]]
  write.csv(data, file=file.path(out_dir, sprintf("%03s_spott_50.csv", id)), row.names=F)
})


write.csv(allins, file=file.path(outdir, "allins.csv"), row.names=FALSE)
length(unique(allins$id))
