library(tidyverse)
library(cowplot)
library(effsize)
library(beepr)
library(viridis)
theme_set(theme_cowplot())

#general variables
MEAN_1 <- 100
MEAN_2 <- 110
SD <- 15
d <- abs(MEAN_1 - MEAN_2) / SD

range_low <- 40
range_high <- 160

xval <- seq(range_low,range_high)
plot_data <- expand.grid(x = xval,condition = c("control","treatment"))

plot_data <- plot_data %>%
  mutate(
    y = case_when(
      condition == "control" ~dnorm(x,mean=MEAN_1, sd = SD),
      condition == 'treatment' ~dnorm(x, mean=MEAN_2, sd=SD)
    ),
    mean_1 = MEAN_1,
    mean_2 = MEAN_2,
    sd = SD,
    d = round(d,2)
  )

d_text <- paste("Cohen's d = ",as.character(round(d,2)),sep="")

ggplot(plot_data,aes(x=x, y=y, color=condition))+
  geom_line(size=3)+
  scale_color_brewer(palette="Set1")+
  geom_vline(xintercept=MEAN_1)+
  geom_vline(xintercept=MEAN_2)+
  annotate("text", label = d_text, x = 60, y = 0.02,size=5)


##### simulator ####
set.seed(123)
MEAN_1 <- 100
SD <- 15

### set up parameters
num_samples <- 20000
#num_samples <- 10
percent_non_significant <- c(0, 1/3, 2/3, 1.0)
sample_size_per_condition_seq <- c(10,15,20,30,40,50,100,200)
# set desired d by adjusting MEAN_2
d_val <- seq(0,2, 0.01)

results <- data.frame(
)

for (cur_percent_ns in percent_non_significant) {
  for (sample_size_per_condition in sample_size_per_condition_seq ) {
    print(sample_size_per_condition)
    for (d in d_val) {
      print(d)
      #assuming MEAN_2 is bigger
      MEAN_2 <- MEAN_1 + d * SD
      sig_counter = 0
      non_sig_counter = 0
      cur_est_d_list_sigonly = c()
      cur_est_d_list_all = c()
      cur_est_d_list_perc = c()
      for (i in 1:num_samples) {
        data_1 <- rnorm(sample_size_per_condition,mean=MEAN_1,sd = SD)
        data_2 <- rnorm(sample_size_per_condition,mean=MEAN_2,sd = SD)
        p_val <- t.test(data_2,data_1)$p.value
        d_est <- cohen.d(data_2,data_1)$estimate
        cur_est_d_list_all <- c(cur_est_d_list_all,d_est)
        if (p_val < 0.05) {
          sig_counter <- sig_counter + 1
          cur_est_d_list_sigonly <-  c(cur_est_d_list_sigonly,d_est)
          cur_est_d_list_perc <- c(cur_est_d_list_perc,d_est)
          }
        if (p_val >= 0.05) {
          non_sig_counter <- non_sig_counter + 1
          if (runif(1) <= cur_percent_ns) {
            cur_est_d_list_perc <- c(cur_est_d_list_perc,d_est)
          }
        }
      }
      cur_results <- data.frame(
      d = d,
      num_samples = num_samples,
      sample_size_per_condition = sample_size_per_condition,
      percent_ns = cur_percent_ns,
      mean_1 = MEAN_1,
      mean_2 = MEAN_2,
      sd = SD,
      true_d = abs(MEAN_1 - MEAN_2)/SD,
      sig_counter = sig_counter,
      mean_d_sigonly = mean(cur_est_d_list_sigonly),
      mean_d_sig_percent=mean(cur_est_d_list_perc),
      mean_d_all = mean(cur_est_d_list_all)
    )
  
    results <- results %>%
      bind_rows(cur_results)
    }
  }
}

ggplot(results,aes(true_d,mean_d_sigonly, color=as.factor(2*sample_size_per_condition)))+
  #geom_line()+
  geom_smooth(se=F,method="loess",span=0.2,size=1.5)+
  geom_line(data=data.frame(x=seq(0,2,0.1),y=seq(0,2,0.1)), aes(x,y),size=1.5,color="black")+
  xlab("True Effect Size (Cohen's d)")+
  ylab("Average Effect Size \nEstimated from Significant Results Only")+
  scale_color_viridis_d(
    name = "SAMPLE SIZE"
  )+
  theme(legend.position = c(0.7,0.3))+
  xlim(0,1.5)+
  ylim(0,1.7)
ggsave("winners_curse_simulation.jpeg")
ggsave("winners_curse_simulation.tiff")
ggsave("winners_curse_simulation.pdf")

ggplot(results,aes(true_d,mean_d_sig_percent, color=as.factor(2*sample_size_per_condition)))+
  #geom_line()+
  geom_smooth(se=F,method="loess",span=0.2,size=1.5)+
  geom_line(data=data.frame(x=seq(0,2,0.1),y=seq(0,2,0.1)), aes(x,y),size=1.5,color="black")+
  xlab("True Effect Size (Cohen's d)")+
  ylab("Average Effect Size \nEstimated from Significant Results Only")+
  scale_color_viridis_d(
    name = "SAMPLE SIZE"
  )+
  theme(legend.position = c(0.7,0.3))+
  xlim(0,1.5)+
  ylim(0,1.7)+
  facet_wrap(~percent_ns)
ggsave("winners_curse_simulation_percent_ns.jpeg")

write_csv(results,"simulation_results.csv")

#hacky solution to the slight (rounding error-related?) imprecision in the 100% reporting condition
results <- results %>%
  mutate(
    mean_d_sig_percent_mod = case_when(
      percent_ns==1~true_d,
      TRUE ~mean_d_sig_percent
    ),
    total_sample = 2* sample_size_per_condition,
    non_sig_reported = case_when(
      percent_ns == 1 ~ "100% ns results",
      percent_ns == 2/3 ~ "67% ns results",
      percent_ns == 1/3 ~ "33% ns results",
      percent_ns == 0 ~ "0% ns results"
    ))

results$non_sig_reported = factor(results$non_sig_reported,levels=c(
  "100% ns results",
  "67% ns results",
  "33% ns results",
  "0% ns results"
))


ggplot(filter(results,total_sample %in% c(20,30,40,60,100)),aes(true_d,mean_d_sig_percent_mod, color=as.factor(total_sample)))+
  #geom_line()+
  geom_smooth(se=F,method="loess",span=0.2,size=1.5)+
  geom_line(data=data.frame(x=seq(0,2,0.1),y=seq(0,2,0.1)), aes(x,y),size=1.5,color="black")+
  xlab("True Effect Size (Cohen's d)")+
  ylab("Average Effect Size \nFrom Selectively Reported Studies")+
  scale_color_viridis_d(
    name = "SAMPLE SIZE"
  )+
  theme(legend.position = c(0.05,0.8),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        legend.text = element_text(size=20),
        legend.title=element_text(size=24),
        strip.text=element_text(size = 22))+
  xlim(0,1.5)+
  ylim(0,1.7)+
  facet_wrap(~non_sig_reported,nrow=1)
ggsave("winners_curse_simulation_percent_ns.jpeg",width=14,height=7)


ggplot(results,aes(true_d,mean_d_all, color=as.factor(sample_size_per_condition)))+
  geom_line()+
  #geom_smooth(se=F,method="loess",span=0.2,size=1.5)+
  geom_line(data=data.frame(x=seq(0,2,0.1),y=seq(0,2,0.1)), aes(x,y),size=1.5,color="black")+
  xlab("True Effect Size (Cohen's d)")+
  ylab("Average Effect Size Estimated")+
  scale_color_viridis_d(
    name = "SAMPLE SIZE"
  )+
  theme(legend.position = c(0.7,0.3))+
  xlim(0,1.5)+
  ylim(0,1.7)

beep(2)


