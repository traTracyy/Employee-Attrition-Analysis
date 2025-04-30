#Question3:Are there any departments that are facing a headcount crisis?
#————————————————————————————————————————————————————————————————————————————————————
#Analysis 1: Total of Employees in All Departments
Q3_A1_1 <-  employee_attrition %>%
  group_by(department_name) %>%
  select(department_name )%>%
  summarise(totala = n(),.groups = 'drop')

Q3_A1_12  <- employee_attrition %>%
  filter(STATUS == "TERMINATED") %>%
  group_by(department_name) %>%
  select(department_name)%>%
  summarise(totalb = n(),.groups = 'drop')

Q3_A1 <- merge(Q3_A1_1,Q3_A1_12,by="department_name", all = TRUE)
Q3_A1$totalb[is.na(Q3_A1$totalb)==TRUE]=0
Q3_A1 <- Q3_A1 %>%
  mutate(total = totala-totalb)

Q3_A1 %>%
  ggplot( aes(x=department_name, y=total)) +
  geom_segment( aes(xend=department_name, yend=0)) +
  geom_point( size=8, color=ifelse(Q3_A1$total ==0, "red","#CEE5D0")) +
  xlab("Department") + 
  ylab("Total of Employees")+
  ggtitle("Total of Employees in All Departments")+
  geom_text(aes(label = total)) +
  coord_flip() +
  theme_bw()



#————————————————————————————————————————————————————————————————————————————————————
#Analysis 2: Departments Currently Unemployed and Their Cities
#So why is the number of employees in these departments at 0? 
#What is the reason for the under staffing situation? 
#Is it because the cities in these departments have low population densities, so they can't recruit people?
Q3_A2_1 <-  employee_attrition %>%
  group_by(department_name,city_name) %>%
  select(department_name ,city_name)%>%
  summarise(totala = n(),.groups = 'drop')

Q3_A2_12  <- employee_attrition %>%
  filter(STATUS == "TERMINATED") %>%
  group_by(department_name,city_name) %>%
  select(department_name,city_name)%>%
  summarise(totalb = n(),.groups = 'drop')

Q3_A2 <- merge(Q3_A2_1,Q3_A2_12,by=c("city_name","department_name"), all = TRUE)
Q3_A2$totalb[is.na(Q3_A2$totalb)==TRUE]=0
Q3_A2 <- Q3_A2 %>%
  mutate(total = totala-totalb)

Q3_A2 %>%
  filter(total == 0)%>%
  ggplot( aes(x=city_name, y=department_name, color =department_name )) + geom_point(size = 6)+
  xlab("City") + 
  ylab("Department")+
  ggtitle("Departments Currently No Employee and Their Cities")+
  coord_flip() +theme_calc()+
  theme(axis.text.x = element_text(angle=65, vjust=0.6),legend.position = "none")


#So what is the reason why the store management department has no one in many cities? 
#What's the reason for the fact that the city of Vancouver has 12 departments with zero headcount? 
#Could it be that the employee has reached retirement age and left the company? 
#analyze the age of departing employees in the store management sector 
Q3_A2_2  <- employee_attrition %>%
  filter(STATUS == "TERMINATED" & department_name =="Store Management") %>%
  group_by(age,Gender) %>%
  select(age,Gender)%>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q3_A2_2, aes(age, total, color= Gender, shape = Gender)) +geom_line()+geom_point(size=8)+
  geom_text(aes(label = total), color ="black")+theme_bw()+
  xlab("Age") + 
  ylab("Total of Employees")+
  ggtitle("The Age of Employees in Store Managment\nDepartment Who Terminated")+
  theme(legend.position = "top")




#analyse the age of percentage of departing employees in the city of Vancouver 
Q3_A2_3  <- employee_attrition %>%
  filter(STATUS == "TERMINATED" & city_name =="Vancouver") %>%
  group_by(age) %>%
  select(age)%>%
  summarise(total = n(),.groups = 'drop')
Q3_A2_3  <- Q3_A2_3  %>%
  mutate(pro = total / sum(Q3_A2_3$total))


ggplot(Q3_A2_3, aes(age, total)) +geom_col(fill = "#C4DFAA")+
  geom_text(aes(label = scales::percent(pro, accuracy = .1)), position = position_stack(vjust =.5))+
  xlab("Age") + scale_fill_manual(values = "green")+
  ylab("Total of Employees")+ theme_calc()+
  ggtitle("Percentage of The Age of Terminated Employees in Vancouver")+
  theme(legend.position = "top")+coord_flip()


#————————————————————————————————————————————————————————————————————————————————————
#Analysis 3: Total number of people laid off in each department
Q3_A3_1 <-  employee_attrition %>%
  filter(termtype_desc == "Involuntary") %>%
  group_by(department_name) %>%
  select(department_name )%>%
  summarise(total = n(),.groups = 'drop')

Q3_A3_1 %>%
  arrange(total) %>% 
  mutate(department_name=factor(department_name, levels=department_name)) %>%
  ggplot( aes(x=department_name, y=total, color = department_name)) +
  geom_segment( aes(xend=department_name, yend=0)) +
  geom_point( size=8) +
  xlab("Department") + 
  ylab("Total of Employees Laid Off")+theme_classic()+
  ggtitle("Total of Employees Laid Off in Each Department")+
  geom_text(aes(label = total),position = position_dodge(width = 1),
            vjust = -1.5) +
  theme(legend.position = "none")



#Age of Employees Who Laid Off in The Department
Q3_A3_2 <-  employee_attrition %>%
  filter(termtype_desc == "Involuntary") %>%
  group_by(department_name, age) %>%
  select(department_name, age)%>%
  summarise(total = n(),.groups = 'drop')
Q3_A3_2 <- Q3_A3_2%>%
  group_by(department_name)%>%
  mutate(X=sum(total))%>%
  mutate(Y = sum(age*total))%>%
  mutate(avgage=Y/X)

ggplot(Q3_A3_2, aes(x=age, y=total,color =department_name)) + 
  geom_point(size = 2)+geom_line()+
  facet_grid(rows = vars(department_name))+
  ggtitle("Age of Employees Who Laid Off in The Department")+
  xlab("Age")+
  ylab("Total of Employees")+ 
  theme(legend.position = "none")+geom_vline(aes(xintercept = avgage),linetype = "dashed") 



#————————————————————————————————————————————————————————————————————————————————————
#Analysis 4: The average age of each department,analyzing whether there is an aging population in any department
employee_attrition %>% 
  filter(STATUS == "ACTIVE") %>%
  group_by(department_name) %>% 
  summarise(avgage = mean(age,accuracy = .0)) %>% 
  ggplot(aes("", avgage, label=round(avgage,0) ,fill=factor(department_name))) +
  geom_col()+theme_bw() +
  geom_text(position =position_stack(vjust = 0.5)) + 
  ggtitle("The Average Age in Each Department Which Have Employees")+
  xlab("Department")+
  ylab("Average Age")+
  facet_wrap(~department_name)+
  theme(legend.position = "none")+coord_flip()


#TERMINATED
employee_attrition %>% 
  filter(STATUS == "TERMINATED") %>%
  group_by(department_name) %>% 
  summarise(avgage = mean(age,accuracy = .0)) %>% 
  ggplot(aes("", avgage, label=round(avgage,0) ,fill=factor(department_name))) +
  geom_col()+theme_bw() +
  geom_text(position =position_stack(vjust = 0.5)) + 
  ggtitle("The Average Age of Terminated Employees in Each Department")+
  xlab("Department")+
  ylab("Average Age")+
  geom_text(position =position_stack(vjust = 0.5)) + 
  facet_wrap(~department_name)+
  theme(legend.position = "none")+coord_flip()


