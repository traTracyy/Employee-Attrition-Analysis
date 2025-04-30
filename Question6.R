#Question 6: What are the company's annual trends and the impact they bring?
#————————————————————————————————————————————————————————————————————————————————————
#Analysis 1: Number of employees entering the company in each year
Q6_A1_1 <- employee_attrition %>% 
  group_by(orighireYear) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q6_A1_1, aes(x=orighireYear,y=total)) + 
  geom_line(size=1.2) + 
  labs(title="Yearly Series with Number of Employees Hired",
       x = "Year",
       y="Total of Employees") + 
  geom_label_repel(aes(label = orighireYear),
                   box.padding   = 0.6, 
                   point.padding = 0.1,
                   segment.color = 'black') +theme_light()+theme(axis.text.x=element_blank())

#Why is 1998 the year with the highest number of employees entering the company?
#Does it have any relevance to the city they are in?
Q6_A1_2 <- employee_attrition %>% 
  filter(orighireYear == "1998") %>% 
  group_by(city_name) %>%
  summarise(total = n(),.groups = 'drop')
ggplot(Q6_A1_2, aes(city_name,total,fill=city_name)) + geom_col() +theme_bw()+
  theme(legend.position = "none")+
  geom_text(aes(label = total),position = position_stack(vjust = 0.5)) +
  labs(x="City",
       y="Total of Employees",
       title = "The City Where the Employee Was Hired In 1998")+coord_flip()




#————————————————————————————————————————————————————————————————————————————————————
#Analysis 2: Number of employees leaving the company each year
Q6_A2_1 <- employee_attrition %>% 
  filter(terminationYear > 1900)%>% 
  group_by(terminationYear) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q6_A2_1, aes(x=terminationYear,y=total)) + 
  geom_line(size=1.2,color = "white") + 
  labs(title="Yearly Series with Number of Employees Terminated",
       x = "Year",
       y="Total of Employees") + 
  geom_label_repel(aes(label = terminationYear),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_dark()+theme(axis.text.x=element_blank())



#Why did the number of departures suddenly increase in 2014?
#Analyse the reasons for leaving
Q6_A2_2 <- employee_attrition %>% 
  filter(terminationYear == '2014' & termreason_desc !="Not Applicable")%>% 
  group_by(termreason_desc) %>%
  summarise(total = n(),.groups = 'drop')
Q6_A2_2 <-Q6_A2_2%>%
  mutate(pro = total / sum(Q6_A2_2$total))

ggplot(Q6_A2_2, aes(x="", y=total, fill=termreason_desc))+coord_polar("y", start=0)+
  geom_col()+
  scale_fill_manual(values = c("#C2B8A3","#FEF7DC", "#E6DDC6"))+
  geom_text(aes(label = total),position = position_stack(vjust = 0.5))+theme_void()+
  ggtitle("Reasons for Terminated Employees Leaving in 2014")+
  geom_text(aes(x = 1.6, label = scales::percent(pro, accuracy = .1)), 
            position = position_stack(vjust = .5))+
  guides(fill=guide_legend(title="Termination Reason"))+
  theme(plot.title = element_text(hjust = 0.5,vjust = -5, size=18,face="bold"),legend.position = "bottom")




#Analyse Age And Reasons for Terminated Employees In 2014
Q6_A2_3 <- employee_attrition %>% 
  filter(terminationYear == '2014' & termreason_desc !="Not Applicable")%>% 
  group_by(termreason_desc,age) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q6_A2_3, aes(x=age, y=total,color =termreason_desc)) + 
  geom_point(size = 2)+ 
  geom_segment(aes(x=age, 
                   xend=age, 
                   y=0, 
                   yend=total))+theme_cleveland()+
  facet_grid(rows = vars(termreason_desc))+
  ggtitle("Age And Reasons for Terminated Employees In 2014")+
  xlab("Age")+
  ylab("Total of EmployeeS")+ 
  theme(legend.position = "none")




#————————————————————————————————————————————————————————————————————————————————————
#Analysis 3:Number of Employees Voluntary Terminated Each Year
Q6_A3_1 <- employee_attrition %>% 
  filter(terminationYear > 1900, termtype_desc =="Voluntary")%>% 
  group_by(terminationYear) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q6_A3_1, aes(x=terminationYear,y=total)) + 
  geom_line(size=1.2,color = "red") + 
  labs(title="Yearly Series with Number of Employees Voluntary Terminated",
       x = "Year",
       y="Total of Employees") + 
  geom_label_repel(aes(label = terminationYear),
                   box.padding   = 0.9, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   fill = alpha(c("white"),.1)) +theme(axis.text.x=element_blank())


#Years and Reasons for Voluntary Terminated of Employees
Q6_A3_2 <- employee_attrition %>% 
  filter(terminationYear > 1900, termtype_desc =="Voluntary")%>% 
  group_by(terminationYear,termreason_desc) %>%
  summarise(total = n(),.groups = 'drop')

ggplot(Q6_A3_2, aes(x=terminationYear,y=total,color = termreason_desc))+
  geom_line()+geom_point()+ 
  facet_grid(rows = vars(termreason_desc))+theme_clean()+
  ggtitle("Years and Reasons for Voluntary Terminated of Employees")+
  xlab("Year")+
  ylab("Total of Employees")+ 
  theme(legend.position = "none")+
  geom_label_repel(aes(label = terminationYear),
                   box.padding   = 0.8, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   fill = alpha(c("white"),.1)) +theme(axis.text.x=element_blank())







