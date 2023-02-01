mechacar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(mechacar_mpg)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_mpg)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_mpg))

suspension_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
head(suspension_coil)
total_summary <- suspension_coil %>% summarize(MEAN=mean(PSI), MEDIAN=median(PSI), VARIANCE=var(PSI), SD=sd(PSI))
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(MEAN=mean(PSI), MEDIAN=median(PSI), VARIANCE=var(PSI), SD=sd(PSI), .groups = 'keep')

head(suspension_coil)

t.test(suspension_coil$PSI, mu=1500)

t.test(subset(suspension_coil, Manufacturing_Lot == "Lot1")$PSI, mu=1500)
t.test(subset(suspension_coil, Manufacturing_Lot == "Lot2")$PSI, mu=1500)
t.test(subset(suspension_coil, Manufacturing_Lot == "Lot3")$PSI, mu=1500)


