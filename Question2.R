#Question 2: Why do employees leave voluntarily?
#————————————————————————————————————————————————————————————————————————————————————
#Analysis 1: Voluntary separation (percentage of resignations and retirements)
Q2_A1_1 <- employee_attrition %>%
  filter(STATUS == "TERMINATED" & termtype_desc == "Voluntary") %>%
  select(termreason_desc)%>%
  group_by(termreason_desc) %>%
  summarise(total = n(),.groups = 'drop')
Q2_A1_1 <-Q2_A1_1%>%
  mutate(pro = total / sum(Q2_A1_1$total))

ggplot(Q2_A1_1, aes(x="", y=total, fill=termreason_desc))+coord_polar("y", start=0)+geom_col(color = "white")+
  geom_text(aes(label = total),position = position_stack(vjust = 0.5))+theme_void()+
  scale_fill_manual(values = c("#FCF8E8", "#ECB390"))+
  ggtitle("The Ratio of Voluntary Resignation and Retirement")+
  geom_text(aes(x = 1.6, label = scales::percent(pro, accuracy = .1)), position = position_stack(vjust = .5))+
  guides(fill=guide_legend(title="Termination Reason"))+
  theme(plot.title = element_text(hjust = 0.5,vjust = -5, size=20,face="bold", color = "#A27B5C"))


#What is the ratio of the number of male and female employees who voluntarily left the company? 
#female
Q2_A1_2 <- employee_attrition %>%
  filter(STATUS == "TERMINATED" & termtype_desc == "Voluntary" & Gender =="Female") %>%
  select(termreason_desc)%>%
  group_by(termreason_desc) %>%
  summarise(total = n(),.groups = 'drop')
Q2_A1_2 <-Q2_A1_2 %>%
  mutate(pro = total / sum(Q2_A1_2$total))

ggplot(Q2_A1_2, aes(x="", y=total, fill=termreason_desc))+coord_polar("y", start=0)+
  geom_col()+
  geom_text(aes(label = total),position = position_stack(vjust = 0.5))+theme_void()+
  scale_fill_manual(values = c("#FFC3C3", "#FF8C8C"))+
  ggtitle("The Ratio of Voluntary Resignation and Retirement in Female")+
  geom_text(aes(x = 1.6, label = scales::percent(pro, accuracy = .1)), 
            position = position_stack(vjust = .5))+
  guides(fill=guide_legend(title="Termination Reason"))+
  theme(plot.title = element_text(hjust = 0.5,vjust = -5, size=18,face="bold"),legend.position = "bottom")


#male
Q2_A1_3 <- employee_attrition %>%
  filter(STATUS == "TERMINATED" & termtype_desc == "Voluntary" & Gender =="Male") %>%
  select(termreason_desc)%>%
  group_by(termreason_desc) %>%
  summarise(total = n(),.groups = 'drop')
Q2_A1_3 <-Q2_A1_3 %>%
  mutate(pro = total / sum(Q2_A1_3$total))

ggplot(Q2_A1_3, aes(x="", y=total, fill=termreason_desc))+coord_polar("y", start=0)+
  geom_col()+
  geom_text(aes(label = total),position = position_stack(vjust = 0.5))+theme_void()+
  scale_fill_manual(values = c("#DDF3F5", "#A6DCEF"))+
  ggtitle("The Ratio of Voluntary Resignation and Retirement in Male")+
  geom_text(aes(x = 1.6, label = scales::percent(pro, accuracy = .1)), 
            position = position_stack(vjust = .5))+
  guides(fill=guide_legend(title="Termination Reason"))+
  theme(plot.title = element_text(hjust = 0.5,vjust = -5, size=18,face="bold"),legend.position = "bottom")



#————————————————————————————————————————————————————————————————————————————————————
#Analysis2: Total number of voluntary terminations per department
Q2_A2_1  <- employee_attrition %>%
  filter(STATUS == "TERMINATED" & termtype_desc == "Voluntary") %>%
  group_by(department_name,Gender) %>%
  select(department_name,Gender)%>%
  summarise(total = n(),.groups = 'drop') 

ggplot(Q2_A2_1, aes(x=department_name, y=total, fill=Gender))+
  geom_col()+
  scale_fill_manual(values = c("#FF869E", "#9DD6DF"),labels=c("Female","Male"))+
  facet_share(~Gender,dir="h", scales = "free",reverse_num=FALSE)+
  coord_flip()+
  theme(legend.position = "none")+
  labs(x = NULL, y = NULL)+  
  xlab("Department") + 
  ylab("Total of employees")+
  ggtitle("Total Number of Voluntary Resignations and Retirements in Department")+
  geom_text(aes(label = total), position = position_stack(vjust = 0.5))


#Do they choose to leave voluntarily because they are getting older? 
Q2_A2_2<- employee_attrition %>%
  filter(termtype_desc == "Voluntary")%>%
  select(age_range,department_name) %>%
  group_by(age_range,department_name) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q2_A2_2, aes(age_range, department_name, color = department_name)) + 
  geom_point(size=8,alpha = 0.35)+  geom_text(aes(label = total), color ="black")+
  ggtitle("The Age Range of Voluntary Separation and The Relationship \nBetween Departments")+
  xlab("Age Range")+
  ylab("Department")+theme_bw()+theme(legend.position = "none")


#————————————————————————————————————————————————————————————————————————————————————
#Analysis 3: Total number of voluntary terminations in each position
Q2_A3_1  <- employee_attrition %>%
  filter(termtype_desc == "Voluntary") %>%
  group_by(job_title) %>%
  select(job_title)%>%
  summarise(total = n(),.groups = 'drop')

Q2_A3_1 %>%
  arrange(total) %>% 
  mutate(job_title=factor(job_title, levels=job_title)) %>%
  ggplot( aes(x=job_title, y=total)) +
  geom_segment( aes(xend=job_title, yend=0)) +  
  xlab("Position") + 
  ylab("Total of Employees")+
  ggtitle("Total Number of Voluntary Resignations and Retirements In Position")+
  geom_point( size=6.5, color="orange") +
  geom_text(aes(label = total)) +
  coord_flip() +
  theme_minimal()



#————————————————————————————————————————————————————————————————————————————————————
#Analysis 4:Total Number of Voluntary Terminated in Each City
#analyze whether these voluntary employees are related to the city where they work and 
#cause them to want to leave.

Q2_A4_1<- employee_attrition %>%
  filter(termtype_desc == "Voluntary")%>%
  select(city_name) %>%
  group_by(city_name) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q2_A4_1, aes(x=city_name, y=total, fill = city_name)) + 
  geom_col()+ coord_flip()+
  ggtitle("Total Number of Voluntary Terminated in Each City")+
  xlab("City")+theme_light()+
  geom_text(aes(label = total),position = position_stack(vjust = 0.5))+
  ylab("Total Number of Employees")+
  geom_hline(yintercept = mean(Q2_A4_1$total),linetype = "dashed")+
  theme(legend.position = "none")








