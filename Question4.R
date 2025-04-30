#Question4:Are there any positions that are facing a headcount crisis?
#————————————————————————————————————————————————————————————————————————————————————
#Analysis 1: Current status of all positions
Q4_A1_1 <-  employee_attrition %>%
  group_by(job_title) %>%
  select(job_title )%>%
  summarise(totala = n(),.groups = 'drop')

Q4_A1_12  <- employee_attrition %>%
  filter(STATUS == "TERMINATED") %>%
  group_by(job_title) %>%
  select(job_title)%>%
  summarise(totalb = n(),.groups = 'drop')

Q4_A1 <- merge(Q4_A1_1,Q4_A1_12,by="job_title", all = TRUE)
Q4_A1$totalb[is.na(Q4_A1$totalb)==TRUE]=0
Q4_A1 <- Q4_A1 %>%
  mutate(total = totala-totalb)

Q4_A1 %>%
  ggplot( aes(x=job_title, y=total)) +
  geom_segment( aes(xend=job_title, yend=0)) +
  geom_point( size=5, color=ifelse(Q4_A1$total ==0, "red","orange")) +
  xlab("Position") + 
  ylab("Total of employees")+
  ggtitle("Status of all current positions")+
  geom_text(aes(label = total)) +
  coord_flip() +
  theme_bw()

#————————————————————————————————————————————————————————————————————————————————————
#Analysis 2: The mean of terminated employee's service length in each position
Q4_A2_1 <-  employee_attrition %>%
  filter(STATUS == "TERMINATED") %>%
  group_by(job_title, length_of_service) %>%
  select(job_title, length_of_service)%>%
  summarise(total = n(),.groups = 'drop')

Q4_A2_12 <- Q4_A2_1%>%
  group_by(job_title)%>%
  mutate(X=sum(total))%>%
  mutate(Y = sum(length_of_service*total))%>%
  mutate(avg=Y/X)

Q4_A2_12 %>%
  ggplot( aes(x=job_title, y=avg)) +
  geom_segment( aes(xend=job_title, yend=0)) +
  geom_point( size=5,color = "#EDF6E5") +
  xlab("Position") + 
  ylab("Mean of Service Length")+
  ggtitle("The Mean of Terminated Employee's Service Length in Each Position")+
  geom_text(aes(label = round(avg))) +
  coord_flip() +
  theme_clean()


#So why is the average age of cashier's departing staff only 3 years of service? 
#Analyse their age range of terminated cashiers
Q4_A2_2<-  employee_attrition %>%
  filter(STATUS == "TERMINATED" & job_title=="Cashier") %>%
  group_by(age_range) %>%
  select(age_range)%>%
  summarise(total = n(),.groups = 'drop')
Q4_A2_2<-Q4_A2_2%>%
  mutate(pro = total / sum(Q4_A2_2$total))

ggplot(Q4_A2_2, aes(x=age_range, y=total,fill = age_range)) + geom_bar(stat = "identity")+
  geom_text(aes(label = scales::percent(pro, accuracy = .2)),vjust = -0.5)+
  xlab("Age Range")+theme_base() + 
  ylab("Total of Terminated Cashiers")+
  ggtitle("The Age Range of Terminated Cashiers")+
  theme(legend.position = "none")


#Analyse their age range of active cashiers
Q4_A2_3<-  employee_attrition %>%
  filter(STATUS == "ACTIVE" & job_title=="Cashier") %>%
  group_by(age_range) %>%
  select(age_range)%>%
  summarise(total = n(),.groups = 'drop')
Q4_A2_3<-Q4_A2_3%>%
  mutate(pro = total / sum(Q4_A2_3$total))
  
ggplot(Q4_A2_3, aes(x=age_range, y=total,fill = age_range)) + geom_bar(stat = "identity")+
  geom_text(aes(label = scales::percent(pro, accuracy = .2)),vjust = -0.5)+theme_hc()+
  xlab("Age Range") + 
  ylab("Total of Active Cashiers")+
  ggtitle("The Age Range of Active Cashiers")+
  theme(legend.position = "none")


#————————————————————————————————————————————————————————————————————————————————————
#Analysis 3: The total of active employee(s) in the position
Q4_A3_1 <-  employee_attrition %>%
  filter(STATUS == "ACTIVE") %>%
  group_by(job_title) %>%
  select(job_title)%>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q4_A3_1, aes( "",total,fill = job_title) ) + 
  geom_bar( stat = "identity" ) +theme_few()+ 
  facet_wrap( ~ job_title ) + 
  xlab("Position") + 
  ylab("Total of Active Employee(s)")+
  ggtitle("The Total of Active Employee(s) In The Position")+
  geom_text( aes( label =  total), position = position_stack(vjust = 0.5))+
  theme(legend.position = "none")


#————————————————————————————————————————————————————————————————————————————————————
#Analysis 4: The average age of active and terminated employees in each position

#active
employee_attrition %>% 
  filter(STATUS == "ACTIVE") %>%
  group_by(job_title) %>% 
  summarise(avgage = mean(age,accuracy = .0)) %>% 
  ggplot(aes("", avgage, label=round(avgage,0) ,fill=factor(job_title))) +
  geom_col()+theme_dark() +
  geom_text(position =position_stack(vjust = 0.5)) + 
  ggtitle("The Average Age of Active Employees in Each Position")+
  xlab("Average Age of Active Employees")+
  ylab("Position")+
  facet_wrap(~job_title)+
  theme(legend.position = "none")+coord_flip()

#TERMINATED
employee_attrition %>% 
  filter(STATUS == "TERMINATED") %>%
  group_by(job_title) %>% 
  summarise(avgage = mean(age,accuracy = .0)) %>% 
  ggplot(aes("", avgage, label=round(avgage,0) ,fill=factor(job_title))) +
  geom_col()+theme_few() +
  geom_text(position =position_stack(vjust = 0.5)) + 
  ggtitle("The Average Age of Terminated Employees in Each Position")+
  xlab("Average Age of Terminated Employees")+
  ylab("Position")+
  geom_text(position =position_stack(vjust = 0.5)) + 
  facet_wrap(~job_title)+
  theme(legend.position = "none")+coord_flip()












