#Question 5: Does the difference in the era in which they were born have anything to do 
#            with how they view their jobs and how high their turnover rate is?
#————————————————————————————————————————————————————————————————————————————————————
#Analysis 1: Total number of employees born in each era
Q5_A1_1 <- employee_attrition %>% 
  select(range)%>%
  group_by(range )%>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q5_A1_1, aes(range,total,fill=range)) + geom_col() +theme(legend.position = "none")+
  geom_text(aes(label = total),position = position_dodge(width = 1),
            vjust = -1.5) +
  labs(x="Era of Birth",
       y="Total of Employees",
       title = "Total Number of Employees Born In Each Era", 
       subtitle="Era: 1940s ~ 1990s")


#Are the 1980s born in the highest numbers because they will 
#leave more quickly so companies need to keep recruiting this era of employees?
#analyse Total Number of Terminated Employees Born In Each Era
Q5_A1_2 <- employee_attrition %>% 
  filter(STATUS == "TERMINATED")%>% 
  select(range)%>%
  group_by(range )%>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q5_A1_2, aes(range,total,fill=range)) + geom_col() +theme_bw()+theme(legend.position = "none")+
  geom_text(aes(label = total),position = position_dodge(width = 1),
            vjust = -1.5) +
  labs(x="Era of Birth",
       y="Total of Employees",
       title = "Total Number of Terminated Employees Born In Each Era", 
       subtitle="Era: 1940s ~ 1990s")



#So what exactly is the era with the highest number of people in the company now?
Q5_A1_3 <- employee_attrition %>% 
  filter(STATUS == "ACTIVE")%>% 
  select(range)%>%
  group_by(range )%>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q5_A1_3, aes(range,total,fill=range)) + geom_col() +theme_bw()+theme(legend.position = "none")+
  geom_text(aes(label = total),position = position_dodge(width = 1),
            vjust = -1.5) +
  labs(x="Era of Birth",
       y="Total of Employees",
       title = "Total Number of Active Employees Born In Each Era", 
       subtitle="Era: 1940s ~ 1990s")


#————————————————————————————————————————————————————————————————————————————————————
#Analysis 2: At what age did the employees of each era enter the company?
Q5_A2_1 <- employee_attrition %>% 
  select(range,orighireYear,birthYear)%>%  
  mutate(AgeEntered=orighireYear - birthYear)

ggplot(Q5_A2_1, aes(AgeEntered, "", color = range, shape = range)) + 
  geom_point(size = 8) + facet_grid(rows = vars(range))+
  labs(x="Age of entering company",
       y="Era of Birth",
       title = "Relationship Between Age of Employee Entering Company and Their Era of Birth", 
       subtitle="Era: 1940s ~ 1990s")+theme_gray()+theme(legend.position = "none")



#————————————————————————————————————————————————————————————————————————————————————
#Analysis 3:The general length of service for employees born in each era
employee_attrition%>% 
  filter(STATUS == "TERMINATED")%>% 
  ggplot(aes(length_of_service, "", color = range, shape = range)) + 
  geom_point(size = 5) + facet_grid(rows = vars(range))+
  geom_text(aes(label = length_of_service),position = position_dodge(width = 1),
            vjust = -1.5) +
  labs(x="Length of Service",
       y="Era of Birth",
       title = "Relationship Between Length of Terminated Employee's Service and Their Era of Birth", 
       subtitle="Era: 1940s ~ 1990s")+theme_bw()+theme(legend.position = "none")





