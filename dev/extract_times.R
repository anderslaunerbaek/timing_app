rm(list = ls())
source("./R/app.R")
source("./dev/print_to_html_table.R")
library(dplyr)

#
path <- "~/dev/timing_app/data/2018/"
# extract times
df_time <- read.csv2(paste0(path, "Krengeruploebet2018.csv")) %>%
    select(Nr, Angiv.dit.navn, Alder, Evt..angiv.klub, Løb) %>%
    mutate(Tid = NA,
           Løb = as.character(Løb))

# load start times
load(paste0(path, "timestamps/start_time/start_time_3km.rda"))
start_time_3km <- tmp
load(paste0(path, "timestamps/start_time/start_time_5km.rda"))
start_time_5km <- tmp
load(paste0(path, "timestamps/start_time/start_time_10km.rda"))
start_time_10km <- tmp
load(paste0(path, "timestamps/start_time/start_time_halv.rda"))
start_time_halv <- tmp
load(paste0(path, "timestamps/start_time/start_time_26km.rda"))
start_time_26 <- tmp
load(paste0(path, "timestamps/start_time/start_time_52km.rda"))
start_time_52 <- tmp
# remove unused variables
rm(tmp)



files_in_path <- list.files(paste0(path,"timestamps/timestamp/"))[!(list.files(paste0(path,"timestamps/timestamp/")) %in% "tot_df.rda")]

for (ff in files_in_path) {
    # load number
    tmp_nr <- as.integer(unlist(strsplit(unlist(strsplit(ff, "_"))[2], "[.]"))[1])
    # load timestamp
    load(paste0(path, "timestamps/timestamp/", ff))
    tmp_idx <- which(df_time$Nr == tmp_nr)
    #
    if (df_time$Løb[tmp_idx] == "Løb - Halvmar. Pris 100,-") {
        df_time$Tid[tmp_idx] <- get_time_dif(start_time_halv, tmp_time)
    } else if (df_time$Løb[tmp_idx] == "Cykling - 53 km. Pris 120,-") {
        df_time$Tid[tmp_idx] <- get_time_dif(start_time_52, tmp_time)
    } else if (df_time$Løb[tmp_idx] == "Løb - 10 km. Pris 75,-") {
        df_time$Tid[tmp_idx] <- get_time_dif(start_time_10km, tmp_time)
    } else if (df_time$Løb[tmp_idx] == "Løb - 5 km. Pris 75,-") {
            df_time$Tid[tmp_idx] <- get_time_dif(start_time_10km, tmp_time)
    } else if (df_time$Løb[tmp_idx] == "Løb - 3 km. Pris 50,-") {
        df_time$Tid[tmp_idx] <- get_time_dif(start_time_3km, tmp_time)
    } else if (df_time$Løb[tmp_idx] == "Cykling - 26 km. Pris 100,-") {
        df_time$Tid[tmp_idx] <- get_time_dif(start_time_26km, tmp_time)
    }
}
#

# unique(df_time$Løb)
# [1] "Cykling - 53 km. Pris 120,-" "Løb - 10 km. Pris 75,-"      "Løb - 5 km. Pris 75,-"       "Løb - 3 km. Pris 50,-"
# [5] "Løb - Halvmar. Pris 100,-"   "Cykling - 26 km. Pris 100,-"

tmp_idx <- !(df_time$Nr %in% c(3,13,17,22,23,24,25,34,35,37,40,49,50,51,52,53,54,55,56,57,94,95,99,102))

#
df_time[tmp_idx, ] %>% filter(Løb == "Løb - Halvmar. Pris 100,-") %>%
    arrange(Tid) %>%
    mutate(Placering = 1:n(),
           Navn = Angiv.dit.navn,
           Nummer = Nr) %>%
    select(Placering, Navn, Nummer, Tid) %>%
    print_to_html_table(.)



