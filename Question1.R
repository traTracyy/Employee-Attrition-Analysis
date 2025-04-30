#Question1: What gender has potential strengths or weaknesses in this company and 
#           does the gender ratio have an impact on the company's growth?

#————————————————————————————————————————————————————————————————————————————————————
#Analysis1: Total Employees by Gender from Past to Present
Q1_A1_1 <- table(subset(employee_attrition, select = Gender))
Q1_A1_1
barplot(Q1_A1_1, 
        xlab = "Total of Employees",
        main='Total Employees by Gender from Past to Present',
        horiz=T, las=1,
        density=c(10,10) , angle=c(135,45) , col=c("brown2", "blue")  )
abline(v= min(Q1_A1_1), col = "blue",lwd=2, lty=2)


#analyze the current ratio of females to males in the company and the ratio of 
#females to males leaving the company
Q1_A1_2<- with(employee_attrition, table(Gender, STATUS))

barplot(Q1_A1_2, beside=TRUE, legend=TRUE,
        main='Gender Distribution with Status', width=0.6,col=rainbow(2))


#Why do female employees leave in higher numbers than men?
#What is the termination reason for female employees?
Q1_A1_3<- employee_attrition %>%
  filter(Gender =="Female" & STATUS == "TERMINATED")%>%
  select(termreason_desc) %>%
  group_by(termreason_desc) %>%
  summarise(total = n(),.groups = 'drop') %>%
  mutate(perc = total / sum(total)) %>% 
  mutate(total_perc = scales::percent(perc))

ggplot(Q1_A1_3, aes(x = "", y = total_perc, fill = termreason_desc)) +
  geom_col() + scale_fill_brewer(palette="OrRd")+
  geom_label(aes(label = total_perc),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  ggtitle("The Reason for Termination of Female Employees Who Terminated")+
  guides(fill = guide_legend(title = "Reason for termination")) +
  coord_polar(theta = "y") + 
  theme_void()


#Retirement has such a high percentage of female employees, but on the other hand, are men the same?
Q1_A1_4<- employee_attrition %>%
  filter(Gender =="Male" & STATUS == "TERMINATED")%>%
  select(termreason_desc) %>%
  group_by(termreason_desc) %>%
  summarise(total = n(),.groups = 'drop') %>%
  mutate(perc = total / sum(total)) %>% 
  mutate(total_perc = scales::percent(perc))

ggplot(Q1_A1_4, aes(x = "", y = total_perc, fill = termreason_desc)) +
  geom_col() + scale_fill_brewer(palette="Blues")+
  geom_label(aes(label = total_perc),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  ggtitle("The Reason for Termination of Male Employees Who Terminated")+
  guides(fill = guide_legend(title = "Reason for termination")) +
  coord_polar(theta = "y") + 
  theme_void()

#The highest reason for employees to leave is retirement, so at what age do they generally retire?
Q1_A1_5<- employee_attrition %>%
  filter (termreason_desc == "Retirement")%>%
  select(age,Gender) %>%
  group_by(age,Gender) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q1_A1_5, aes(age, total, color = Gender)) + 
  geom_point(size=8) + 
  geom_segment(aes(x=age, 
                   xend=age, 
                   y=0, 
                   yend=total)) + 
  facet_grid(rows = vars(Gender))+
  ggtitle("Retirement Age And Number of Male and Female Employees")+
  xlab("Age of Retirement")+
  ylab("Total of Employee")+theme_bw()+ 
  theme(axis.text.x = element_text(angle=55, vjust=0.6),legend.position = "none")+
  geom_text(aes(label = total), color ="black")



#————————————————————————————————————————————————————————————————————————————————————
#Analysis2: Male and female employees distribution in each department
Q1_A2_1<- employee_attrition %>%
  select(department_name, Gender) %>%
  group_by(department_name,Gender) %>%
  summarise(total = n(),.groups = 'drop') %>%
  arrange(desc(total)) %>%
  slice(1:12) 
Q1_A2_12<- employee_attrition %>%
  select(department_name,Gender) %>%
  group_by(department_name,Gender) %>%
  summarise(total = n(),.groups = 'drop') %>%
  arrange(desc(total))  %>%
  slice(13:40) 

ggplot(Q1_A2_1,
       mapping = aes(x = department_name,y = total, fill=Gender)) + geom_col()+
  labs(title = "Gender Distribution in Each Department",
       subtitle = "Department: Bakery, Customer Service, Dairy, Meats, Processed Foods, Produce")+
  xlab("Department") + 
  ylab("Total of Employees")+
  guides(fill=guide_legend(title="Gender"))+
  geom_text(aes(label = total), position = position_stack(vjust = 0.5))+
  theme(legend.position = "top")

ggplot(Q1_A2_12,
       mapping = aes(x = department_name,y = total, fill=Gender)) + geom_col()+
  labs(title = "Gender Distribution in Each Department",
       subtitle = "Department: Traning, Store Management, Recruitment, Legal, Labor Relations, Investment, IT,
       \nHR Technology, Executive, Employee Records, Compensation, Audit, Accounts Receiveable, \nAccounts Payable, 
       Accounting")+
  xlab("Department")+
  ylab("Total of Employees")+
  guides(fill=guide_legend(title="Gender"))+
  geom_text(aes(label = total), position = position_stack(vjust = 0.5))+
  theme(legend.position = "bottom") +coord_flip()



#Distribution of Active Female and Male Currently Working in Each Department
Q1_A2_2<- employee_attrition %>%
  filter(STATUS == "ACTIVE")%>%
  select(department_name, Gender) %>%
  group_by(department_name,Gender) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q1_A2_2, aes(x=department_name, y=total, color = Gender)) + 
  geom_point(size=8) + 
  geom_segment(aes(x=department_name, 
                   xend=department_name, 
                   y=0, 
                   yend=total)) + 
  facet_grid(rows = vars(Gender))+
  ggtitle("Distribution of Active Female and Male Currently Working in Each Department")+
  xlab("Department")+
  ylab("Total of Employees")+theme_pubclean()+ 
  theme(axis.text.x = element_text(angle=55, vjust=0.6),legend.position = "none")+
  geom_text(aes(label = total), color ="black")+
  geom_hline(data = Q1_A2_2 %>% filter(Gender == "Female"),
             aes(yintercept =mean(total)), col = "red", linetype = 2)+
  geom_hline(data = Q1_A2_2 %>% filter(Gender == "Male"),
             aes(yintercept =mean(total)), col = "blue", linetype = 2)


#————————————————————————————————————————————————————————————————————————————————————
#Analysis3: Relationship Between Employee Working Age and Number of Employees Who Is Active
Q1_A3_1<- employee_attrition %>%
  filter(STATUS == "ACTIVE")%>%
  select(age,Gender) %>%
  group_by(age,Gender) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q1_A3_1, aes(age, total, group = Gender, color = Gender)) + 
  geom_line(size=1.3) + geom_point()+  geom_vline(xintercept = mean(Q1_A3_1$age),linetype = "dashed")+
  ggtitle("Relationship Between Employee Working Age and Number of Employees Who Is Active")+
  xlab("Age")+
  ylab("Total of Employees")+theme_bw()+theme(legend.position = "top")

#What is the reason for the large gap between male and female employees between the ages of 25 and 35? 
#analyze the reasons for leaving all male and female employees between the ages of 25 and 35.
Q1_A3_2<- employee_attrition %>%
  filter(STATUS == "TERMINATED" & age >= 25 & age<= 35)%>%
  select(age,termreason_desc,Gender) %>%
  group_by(age,termreason_desc,Gender) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q1_A3_2, aes(x=age, y=total, color = Gender)) + 
  geom_point(size=8)+theme_hc() + 
  geom_segment(aes(x=age, 
                   xend=age, 
                   y=0, 
                   yend=total)) + 
  facet_grid(Gender ~ termreason_desc,  scales = "free_y")+
  ggtitle("Reason for Terminated Between Age 25 Until 35")+
  xlab("Age")+
  ylab("Total of Employees Who Terminated")+
  theme(axis.text.x = element_text(angle=55, vjust=0.6),legend.position = "none")+
  geom_text(aes(label = total), color ="black")



#————————————————————————————————————————————————————————————————————————————————————
#Analysis4:Comparison of length of service and number of male and female employees
#Currently Working
Q1_A4_1 <- employee_attrition %>%
  filter(STATUS=="ACTIVE")%>%
  select(length_of_service,Gender) %>%
  group_by(length_of_service,Gender) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q1_A4_1, aes(x=length_of_service, y=total, color = Gender)) + 
  geom_point(size = 6)+geom_line()+
  facet_grid(rows = vars(Gender))+
  ggtitle("Comparison of Service Length and Number of Employees\nCurrently Working")+
  xlab("Length of Service")+
  ylab("Total of Employees")+theme_pubclean()+ 
  theme(legend.position = "none")+
  geom_text(aes(label = length_of_service),color = "black") 


#Comparison of length of service and number of male and female employees
#Terminated
Q1_A4_2 <- employee_attrition %>%
  filter(STATUS=="TERMINATED")%>%
  select(length_of_service, Gender) %>%
  group_by(length_of_service,Gender) %>%
  summarise(total = n(),.groups = 'drop')

library(ggpmisc)
ggplot(Q1_A4_2, aes(x=length_of_service, y=total, fill = Gender, color = Gender)) + 
  geom_line()+
  facet_grid(rows = vars(Gender))+
  ggtitle("Comparison of Service Length and Number of Terminated Employees")+
  xlab("Length of Service")+
  ylab("Total of Employees")+geom_area( alpha=0.6)+
  theme(legend.position = "none") +stat_peaks(geom = "text", colour = "black", vjust = 2, 
                                              check_overlap = TRUE, span = NULL) 



#What causes female to quit their jobs after 13 years of work? 
#To learn more about whether employees quit their jobs because of their position
#analyze below the positions where female worked for 13 years and quit 
Q1_A4_3 <- employee_attrition %>%
  filter(Gender == "Female" & length_of_service == 13 & STATUS == "TERMINATED")%>%
  select(job_title) %>%
  group_by(job_title)%>%
  summarise(total = n(),.groups = 'drop')


ggplot(Q1_A4_3,aes(job_title,total, fill = job_title)) + geom_col()+
  ggtitle("Relationship Between Female Employees With 13 Years of Employment and Their Job Title")+
  xlab("Job Title") + 
  ylab("Total of Female Employees")+ 
  geom_text(aes(label = total), position = position_stack(vjust = 0.5)) +theme(legend.position = "none")



#male who worked for 8 years and quit
Q1_A4_4 <- employee_attrition %>%
  filter(Gender == "Male" & length_of_service == 8 & STATUS == "TERMINATED")%>%
  select(job_title) %>%
  group_by(job_title)%>%
  summarise(total = n(),.groups = 'drop')


ggplot(Q1_A4_4,aes(job_title,total, fill = job_title)) + geom_col()+
  ggtitle("Relationship Between Male Employees With 8 Years of Employment and Their Job Title")+
  xlab("Job Title") + 
  ylab("Total of Male Employees")+ 
  geom_text(aes(label = total), position = position_stack(vjust = 0.5)) +theme(legend.position = "none")


