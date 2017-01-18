#This report takes Utilization and Forecast Data and creates a summary stats per tech stack demonstrating 
#demand spikes and unuzed resources

#Report rules
# 1. Change the date in Knitr 
# 2. Add amount of hours to the month (date_100 variable)
# 3. Check the list of skills in PreSales and Make sure cleanign works properly.
# CHANGE STATT AND END DATES
# 4. Download Utilization
# 5. Downlpoad Team Composition (Forecast)
# 6. Download project probabilities (Yulia Onishenko)

setwd("C:/Users/lyt/Desktop/Reporting")

require("ggplot2")
require("dplyr")
require("reshape2")
require(gridExtra)
require(grid)
require(knitr)
require(lubridate)
require(googlesheets)
require(xtable)
require(pander)
require(stringr)
require(readxl)

###Projects probability
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
probabiity_df <- read.csv(file=paste0("C:/Users/lyt/Desktop/Reporting/Probabilities_jan16.csv"), 
                                          stringsAsFactors = FALSE, colClasses = "character", na.strings = "")

probabiity_df<-probabiity_df %>%
  filter(Status %in% c("Assign PM", "First Contact", "First Contact", "Proposal Sent")) %>%
  select(Project.title, X..to.win) %>% 
  rename(Probability = X..to.win) %>%
  filter(!is.na(Project.title)) %>%
  mutate(Probability=as.numeric(Probability)/100)

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Salaries

salaries_df <- read.csv(file=paste0("C:/Users/lyt/Desktop/Reporting/salaries-jan10.csv"), stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
salaries_df <- salaries_df[, c("Employee.Name", "Salary.amount")]

q<-3

  
  if (q==1){
  start_date <- as.Date("2017/01/01" , "%Y/%m/%d")
  end_date <- as.Date("2017/01/31" , "%Y/%m/%d")
  
  filename<-"utilization_01-01-2017_31-01-2017_BA"
  data_100<-as.numeric(160)
  
  } else if (q==2){
    start_date <- as.Date("2017/02/01" , "%Y/%m/%d")
    end_date <- as.Date("2017/02/28" , "%Y/%m/%d")
    
    filename<-"utilization_01-02-2017_28-02-2017_BA"
    data_100<-as.numeric(160)
  } else if (q==3){
    start_date <- as.Date("2017/03/01" , "%Y/%m/%d")
    end_date <- as.Date("2017/03/31" , "%Y/%m/%d")
    
    filename<-"utilization_01-03-2017_31-03-2017_BA"
    data_100<-as.numeric(176)
    
    
  }

  ##########################################
  ##########################################
  ########Forecast data
  
  #
#  fc_df <- read.csv(file=paste0("C:/Users/lyt/Desktop/Reporting/team_composition_jan16-2017.csv"), stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
  
  fc_df<-read_excel("team_composition_jan17.xlsx")
  #leaving only forecast
  fc_df<-fc_df[fc_df$`Project status` %in% c("Ass
                                           gn PM", "First Contact", "In Estimation", "Proposal Sent"),]
  
  
  fc_df<-fc_df[which((fc_df$`Request status`=="Forecast") | (fc_df$`Request status`=="Requested")), ]
  
  ## Data cleaning
  fc_df<-fc_df[, which(names(fc_df) %in% c("id", "Project title", "Project.status", "PM", "Location", "Technology",
                                           "Seniority", "Involvement", "Profile created", "Profile start", "Profile end",
                                           "Request status","Request start", "Request end","Resource name", "Request created"))]
  
  #set date types
  fc_df$`Profile start`<-as.Date(fc_df$`Profile start`, "%Y-%m-%d")
  fc_df$`Profile end`<-as.Date(fc_df$`Profile end`, "%Y-%m-%d")
  fc_df$`Request created`<-strptime(fc_df$`Request created`, "%Y-%m-%d %H:%M:%S")

  #Involvment perentag
  fc_df$Stavka <- round(as.numeric(fc_df$Involvement)/8,2)
  
  #Similar Tech stacks
  
  fc_df[ which(fc_df$Technology=="Quality Assurance (QA)"),"Technology"] <- "QA"
  fc_df[ which(fc_df$Technology=="Quality Assurance (QA)"),"Automated Testing"] <- "QA"
  
  
  fc_df[ which(fc_df$Technology=="Design/1"),"Technology"] <- "UX/UI"
  fc_df[ which(fc_df$Technology=="User Experience"),"Technology"] <- "UX/UI"
  fc_df[ which(fc_df$Technology=="Game Designer"),"Technology"] <- "UX/UI"
  fc_df[ which(fc_df$Technology=="GUI Designer"),"Technology"] <- "Unity 3D Gaming"
  fc_df[ which(fc_df$Technology=="A/UX"),"Technology"] <- "UX/UI"
  fc_df[ which(fc_df$Technology=="HTML"),"Technology"] <- "UX/UI"
  
  fc_df[ which(fc_df$Technology=="Business Analysis"),"Technology"] <- "PMO BA"
  
    # Bridge tech stacks with the utilization techn lists
  fc_df[ which(fc_df$Technology=="JavaScript"),"Technology"] <- "JS"
  fc_df[ which(fc_df$Technology=="Angular.JS"),"Technology"] <- "JS"
  fc_df[ which(fc_df$Technology=="HTML"),"Technology"] <- "JS"
  fc_df[ which(fc_df$Technology=="User Interface Development"),"Technology"] <- "JS"
  

      
  #fc_df[ which(fc_df$Technology=="Microsoft Dynamics Nav"),"Technology"] <- "Dynamics"
  #fc_df[ which(fc_df$Technology=="Microsoft Dynamics Ax"),"Technology"] <- "Dynamics"
  #fc_df[ which(fc_df$Technology=="Microsoft Dynamics CRM"),"Technology"] <- "Dynamics"
  
  
  fc_df[ which(fc_df$Technology=="ASP.NET"),"Technology"] <- ".NET"
  fc_df[ which(fc_df$Technology=="ASP.NET MVC (Model View Controller)"),"Technology"] <- ".NET"
  
   
  
  fc_df[ which(fc_df$Technology=="Unity 3D"),"Technology"] <- "Unity 3D Gaming"
  
  #fc_df[ which(fc_df$Technology==""),"Technology"] <- ""
  
  
  #variable to store days of overlap
  fc_df$overlapdays<-0
  
  l<-length(fc_df$id)
  i<-1
  
  for (i in seq_len(l)){
    
    if (!is.na(fc_df$`Profile start`) && !is.na(fc_df$`Profile end`)){
      ##############################
      ### Profile_range bigger then Month_range ==> overlap = 100%
      ###      |---|      Month range
      ### |-------------| Profile_range
      #############################
      
      if ((fc_df$`Profile start`[i] <= start_date) && (fc_df$`Profile end`[i] >= end_date)) {
        fc_df$overlapdays[i]<- difftime(end_date, start_date, units = "days") 
      } else 
        ##############################
      ### Profile_range smaller then Month_range ==> overlap= 100%
      ###   |------------|      Month range    
      ###        |----|         Profile_range
      #############################
      if ((fc_df$`Profile start`[i] >= start_date) && (fc_df$`Profile end`[i] <= end_date)) {
        fc_df$overlapdays[i]<- difftime(fc_df$`Profile end`[i], fc_df$`Profile start`[i], units = "days") 
      } else
        ##############################
      ### Profile_range goes beyone Month_range end date ==> overlap = month_end - profile_start
      ###   |-----------|       Month range    
      ###         |---------|   Profile_range
      #############################
      if (( (fc_df$`Profile start`[i] >= start_date) && (fc_df$`Profile start`[i] <= end_date) ) && 
          ( (fc_df$`Profile end`[i] >= end_date) && (fc_df$`Profile end`[i] >= start_date) )) {
        fc_df$overlapdays[i]<- difftime(end_date, fc_df$`Profile start`[i],  units = "days") 
      } else
        ##############################
      ### Profile_range goes beyone Month_range end date ==> overlap = month_end - profile_start
      ###     |------------|      Month range    
      ### |----------|            Profile_range
      #############################
      if ( (fc_df$`Profile start`[i] <= start_date ) && 
           ( (fc_df$`Profile end`[i] >= start_date) && (fc_df$`Profile end`[i] <= end_date) )) {
        fc_df$overlapdays[i]<- difftime(fc_df$`Profile end`[i], start_date, units = "days") 
      }
      
      
      
    }
  }
  
  period<- as.numeric(difftime(end_date, start_date, units = "days"))
  fc_df$Forecast_FTE <- round(((fc_df$overlapdays * fc_df$Stavka )/ period),2)
  

  ## Fiind duplicated ids 
  n_occur <- data.frame(table(fc_df$id))
  id_oc<-n_occur[n_occur$Freq > 1, ]
  
  
  l<-length(id_oc)
  if (l>=3){
    h<-1
    for (h in seq_len(l)){
    
      #id_oc$Var1 <- list of ids that has duplicates 
      #id_oc$Freq <- amount of duplicates per id
     
      
        ## DELEATING ROWS THAT ARE NOT EQUAL TO MAX IN THIS SUBSET
        mmax<-fc_df[(fc_df$id==id_oc$Var1[h]), "Request.created"]
        fc_df<-subset(fc_df, !((fc_df$id==id_oc$Var1[h]) & !(fc_df$`Request created`==mmax)))
      
      
    }
  }
  
  ##########################################
  ##########################################
  
  
  ##########################################
  ##########################################
  #########WEEKLY UTILIZATION
  
  

  mydata <- read.csv(file=paste0(filename, ".csv"), stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
  
  wip_df <- read.csv(file=paste0("WIP.csv"), stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
  
  ## Define the list for data cleaning
  depts_pmo<-c("PMO Custom","PMO Enterprise","PMO eCommerce","PMO Nearshore","PMO Nearshore","PMO Nearshore","PMO Social & Gaming")
  depts_unit_delete<-c("Big Data & Analytics","BPA","Ciklum Support Center","Ciklum Support Center","Consulting Services",
                       "DevOps","Pakistan Consulting","Pakistan Location","PPMC","Productivity Suite","R&D Engineering",
                       "Service Delivery","Solutions Onshore UK","Solutions Onshore US", "Peak&Relocation", "TBC team",
                       "TCoE Admin", "PMO Admin", "Solutions Admin", "PMO Nordic", "TCoE Admin")
  managers_nonbillable<-c("Benyukh Vlada","Chernyshov Leonid","Litvin Irina","Mykhailenko Ievgeniia",
                          "Zhardel Anna", "Zherdel Anna", "Nosenko Andrey", "Kurtanidze Georgii","Lugovaya Natalia","Prymakov Volodymyr",
                          "Voitsekhovskyi Konstantin")
  
  # QA 
  # add to billable ? Benyukh Vlada, Voitsekhovskyi Konstantin, Litvin Irina -  TL 
  # add to billable ? Chernyshov Leonid -  consultant 
  
  # Kurtanidze Georgii Lugovaya Natalia","Prymakov Volodymyr, - head of unit
  # Mykhailenko Ievgeniia, maternetiy leave
  # 
  
  projects_nonbillable<-c("PMO__Bench__Internal project","PMO__Bench__Internal project BAO", "PMO__Bench__Internal Project E&P",
                          "PMO__Education Training__Internal eCommerce","PMO__Education Training__Internal project",
                          "PMO__Education Training__Internal project BAO",
                          "PMO__Education Training__Internal project E&P", 
                          "PMO__Internal project__Parser for Taleo recruitment system",
                          "PMO__Internal Tasks__Internal eCommerce",
                          "PMO__Internal tasks__Internal project", 
                          "PMO__Internal tasks__Internal project BAO",
                          "PMO__Internal Tasks__Internal Project E&P",
                          "TCOE__TCoE__InternalProject", 
                          "SOL__Solutions Team__Bench",
                          "SOL__Solutions Team__Education")
  
  projects_beforeWIP<-wip_df$A
  
  unit_pmo<-c("PMO BA", "PMO Custom", "PMO eCommerce", "PMO Enterprise", "PMO Social & Gaming", "Unity 3D Gaming","UX/UI","Xamarin")
  unit_sol<-c(".NET", "Android", "Architecture", "Dynamics", "iOS", "Java","JS", "Magento", "PHP")
  unit_tcoe<-c("TCoE Automation QA", "TCoE Manual QA", "TCoE Performance QA", "TCoE Security QA")
  
  mydata <- select(mydata, Delivery.unit, Employee.Name, Seniority, Project.name, Hours.booked)
  mydata$Hours.booked<-as.numeric(mydata$Hours.booked)
  mydata$Load<-as.numeric(mydata$Hours.booked)
  
  
  ##### Data Cleaning 
  ## Selecting right rows(departments, excluding non billable managers)
  smydata<-subset(                                                   #Leavers
    mydata, is.na(mydata$Delivery.unit)=="FALSE" &     
      #Non relevant departments
      !(mydata$Delivery.unit %in% depts_unit_delete) &
      #Managers in QA team 
      !(mydata$Employee.Name %in% managers_nonbillable) 
    
    
  )
  
  
  ## Non billable Load set to zero
  i<-numeric()   
  k<-1
  l<-numeric()
  l<-length(smydata$Delivery.unit)
  for (k in seq_len(l)){
    
    
    #Non billable Load set to zero
    
    if (!is.na(smydata$Project.name[k])){    
      if(smydata$Project.name[k] %in% projects_nonbillable) { smydata$Load[k]<-0}
      #if(smydata$Project.name[k] %in% projects_beforeWIP) { smydata$Load[k]<-0}
      
      
      
    }
  }
  
  #DATA CLEANING FOR BRIDGING WITH FC DATA 
  #smydata[which(smydata$Delivery.unit == "PMO BA"), "Delivery.unit"] <- "Project Management"
  smydata[which(smydata$Delivery.unit == "PMO Custom"), "Delivery.unit"] <- "Project Management"
  smydata[which(smydata$Delivery.unit == "PMO eCommerce"), "Delivery.unit"] <- "Project Management"
  smydata[which(smydata$Delivery.unit == "PMO Enterprise"), "Delivery.unit"] <- "Project Management"
  smydata[which(smydata$Delivery.unit == "PMO Social & Gaming"), "Delivery.unit"] <- "Project Management"
  
  smydata[which(smydata$Delivery.unit == "TCoE Automation QA"), "Delivery.unit"] <- "QA"
  smydata[which(smydata$Delivery.unit == "TCoE Manual QA"), "Delivery.unit"] <- "QA"
  smydata[which(smydata$Delivery.unit == "TCoE Performance QA"), "Delivery.unit"] <- "QA"
  smydata[which(smydata$Delivery.unit == "TCoE Security QA"), "Delivery.unit"] <- "QA"
  #smydata[which(smydata$Delivery.unit == ""), "Delivery.unit"] <- ""
  
  smydata$WIP<-0
  
  smydata$Utilization<-round(smydata$Load/data_100,1)

  smydata[smydata$Project.name %in% projects_beforeWIP, "WIP"]<-"Before work in Progress"
  
  
  #d<-subset(smydata, (smydata$Project.name %in% projects_beforeWIP))
  #l1<-length(d)
  #for (i in seq_len(l1)) {
  #  df <- rbind(fc_df-, data.frame(x = i, y = toString(i)))
  #}
  #
  #d<-rename(d, Technology = Delivery.unit)
  #d<-rename(d, Project.title = Project.name)
  #d<-rename(d, Forecast_FTE = Utilization)
  
  #fc_df<-fc_df[, -which(names(fc_df) %in% c("Profile.created", "Comment", "Check", "Engagement.type", 
                                            
  #d<-d[ , -which(names(d) %in% c("Employee.Name", "Hours.booked", "Load", "WIP"))]
  
  #fc_df<-rbind(fc_df, d[1], deparse.level = 1)
  #do.call(rbind, lapply(fc_df, function(x) x[match(names(fc_df[[1]]), names(x))]))
  
  #smydata<-merge(smydata, salaries_df, by="Employee.Name")
  
  dsmydata<-smydata
  write.csv(dsmydata, file= paste0("!Report_", filename, ".csv"))

  
  u1<-1
  l1<-length(probabiity_df$`Project title`)
  fc_df$Project.probability<-0
  for (u1 in length(l1)){
    #if (fc_df$`Project title` %in% probabiity_df$`Project title`[u1])
      
    
    fc_df[fc_df$`Project title` %in% probabiity_df$`Project title`[u1], "Forecast_FTE"]<-probabiity_df$Probability[u1]*fc_df[fc_df$`Project title` %in% probabiity_df$`Project title`[u1], "Forecast_FTE"]
    
  }
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ##Pivot talbe by employee name 
  
  shorten<-select(smydata, Delivery.unit, Employee.Name, Seniority, Load)
  molten<-melt(shorten, id=c("Employee.Name", "Seniority", "Delivery.unit") )
  dcasted<-dcast(molten, Employee.Name + Seniority + Delivery.unit ~ variable, sum )
  
  #Exclue overtime payments
  dcasted[dcasted$Load > data_100, "Load"]<-data_100
  
  dcasted$Utilized_FTE<-round(dcasted$Load/data_100,2)
  dcasted$Bench_FTE<- 1 - dcasted$Utilized_FTE
  
  ### Group by the list of technologies 
  
  util_util<-select(dcasted, Delivery.unit,Utilized_FTE)
  util_bench<-select(dcasted, Delivery.unit,Bench_FTE)
  
  util_util_dcast<-dcast(util_util, Delivery.unit ~ . , sum)
  util_util_dcast<-rename(util_util_dcast, Utilized_FTE=.)
  
  util_bench_dcast<-dcast(util_bench, Delivery.unit  ~ . , sum)
  util_bench_dcast <- rename(util_bench_dcast, Bench_FTE=.)
  
  ## extract Forecast data 
  
  fc_select<-select(fc_df, Technology, Forecast_FTE)    
  fc_select<-rename(fc_select, Delivery.unit=Technology)
  
  fc_dcast<-dcast(fc_select, Delivery.unit ~ ., sum)
  fc_dcast<-rename(fc_dcast, Forecast_FTE=.)
  #merge two data frames by Delivery.unit
  
  merged_bench_util<-merge(util_bench_dcast, util_util_dcast)
  merged_all<-merge(merged_bench_util, fc_dcast)
  
  melt_all<- melt(merged_all, id="Delivery.unit")
  
  melt_all$value <- round(melt_all$value, 1)
  
  
  ##########################################
#Totlas
# total_df<-dcasted
# total_df$Salary.amount<-as.numeric(total_df$Salary.amount)
# total_df$Bench_budget<-total_df$Salary.amount*total_df$Bench_FTE
# total_df$Utilization_budget<-total_df$Salary.amount*total_df$Utilized_FTE
  
  # total_df$Bench_budget<-round(total_df$Bench_budget,0)
  # total_df$Utilization_budget<-round(total_df$Utilization_budget,0)
  # 
  # total_df<-select(total_df,Delivery.unit, Bench_budget, Utilization_budget)
  # total<-aggregate(total_df[,2:3], by=total_df["Delivery.unit"], FUN=sum)
  # 
  # a<-sum(total$Bench_budget)
  # b<-sum(total$Utilization_budget)
  # total<-rbind(total, c("Grand Total", as.numeric(a), as.numeric(b)))
  # 
  # total$Bench_budget<-as.numeric(total$Bench_budget)
  # total$Utilization_budget<-as.numeric(total$Utilization_budget)
  # 
  # total$Benc_perc<-round(total$Bench_budget/(total$Bench_budget+total$Utilization_budget),1)
  # total$Util_perc<-round(total$Utilization_budget/(total$Bench_budget+total$Utilization_budget),1)
  # 
  # total$Bench_budget<-prettyNum(total$Bench_budget, big.mark = ",", scientific = FALSE)
  # total$Utilization_budget<-prettyNum(total$Utilization_budget, big.mark = ",", scientific = FALSE)
  # 
  # total$Benc_perc<-paste0(total$Benc_perc*100, "%")
  # total$Util_perc<-paste0(total$Util_perc*100, "%")
  # 
  # if (q==1) total1<-total else if (q==2) total2<-total
  
  #p <- ggplot(x, aes(month, data))+
  #  stat_summary(fun.y = sum, geom = "bar")
  

  
  # Demand & Capacity plot listing all technologies for particular period 
  
  #p1 - demand, bench, utilization  
   p1<- ggplot(melt_all, aes(x=melt_all$Delivery.unit, y=melt_all$value, fill=factor(melt_all$variable), 
                            label=melt_all$value)) +
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() + 
    ylab("FTE")+xlab("Technology")+
    scale_fill_manual(values=c("#F8766D","#619CFF","#00BA38"), breaks=c("Forecast_FTE","Utilized_FTE", "Bench_FTE"),name="")+
    geom_text(size = 3, position = position_dodge(width = 0.7))+
    labs(title= paste("Period", strftime(start_date, "%d-%b-%y"), ":", strftime(end_date, "%d-%b-%y")))
  
  ###########################
  #p2 - demand, bench, delta
  delta_df <- merged_all
  delta_df$Delta<- - delta_df$Forecast_FTE + delta_df$Bench_FTE
  delta_df$Utilized_FTE<-NULL
  delta_df<-melt(delta_df, id="Delivery.unit")
  delta_df$value<-round(delta_df$value,1)
  
  bigpl<-delta_df
  
  p2<-ggplot(delta_df, aes(x=delta_df$Delivery.unit, y=delta_df$value, fill=factor(delta_df$variable), 
                           label=delta_df$value)) +
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() + 
    ylab("FTE")+xlab("Technology")+
    ##00BA38 green, #F8766D red, #619CFF blue
    scale_fill_manual(values=c("#00BA38","#F8766D","#619CFF"), 
                      breaks=c("Delta","Forecast_FTE", "Bench_FTE"),name="")+
    geom_text(size = 3, position = position_dodge(width = 0.7))+
    labs(title= paste("Period", strftime(start_date, "%d-%b-%y"), ":", strftime(end_date, "%d-%b-%y")))
     
  
  
  j<-1
  tech_list<-unique(melt_all$Delivery.unit)
  l<-length(tech_list)
  
  graph_list<-list()
  subs_list<-list()
  
  for (j in seq_len(l)){
    
    #j goes over every delivery unit 
    
    subs_graph<-subset(melt_all, melt_all$Delivery.unit==tech_list[j])
   
    # D&C graph for tech_list[j] skill 
    pi<- ggplot(subs_graph, aes(x=subs_graph$`Delivery unit`, y=subs_graph$value, fill=factor(subs_graph$variable), 
                              label=subs_graph$value)) +
      geom_bar(stat="identity", position=position_dodge()) +
      coord_flip() + 
      ylab("FTE")+xlab("")+
      scale_fill_manual(values=c("#F8766D","#619CFF","#00BA38"), breaks=c("Forecast_FTE","Utilized_FTE", "Bench_FTE"),name="")+
      geom_text(size = 3, position = position_dodge(width = 0.7))+
      labs(title= paste("Period", strftime(start_date, "%d-%b-%y"), ":", strftime(end_date, "%d-%b-%y"), tech_list[j])) 
    
    #Delta per skill 
    
    subs_delta<-subset(delta_df, melt_all$Delivery.unit==tech_list[j])
    
    graph_list[[j]]<-subs_delta
    
    pd<- ggplot(subs_delta, aes(x=subs_delta$Delivery.unit, y=subs_delta$value, fill=factor(subs_delta$variable), 
                                label=subs_graph$value)) +
      geom_bar(stat="identity", position=position_dodge()) +
      coord_flip() + 
      ylab("FTE")+xlab("")+
      scale_fill_manual(values=c("#00BA38","#F8766D","#619CFF"), 
                        breaks=c("Delta","Forecast_FTE", "Bench_FTE"),name="")+
      geom_text(size = 3, position = position_dodge(width = 0.9))+
      labs(title= tech_list[j]) 
    
    
    ##Utilization
    subs_smydata<-smydata[smydata$Delivery.unit==tech_list[j], c("Delivery.unit","Employee.Name", 
                                                                   "Seniority", "Project.name", "Utilization")]
    
    subs_dc_smydata<-dcast(subs_smydata, Employee.Name +  Project.name+Seniority + Delivery.unit  ~ ., sum)
    subs_dc_smydata<-rename(subs_dc_smydata, Utilization=.)
    subs_dc_smydata<-subs_dc_smydata[order(subs_dc_smydata$Employee.Name, subs_dc_smydata$Utilization),]
    
    # only 50% bench 
    subs_dc_smydata<-dcast(subs_dc_smydata, Employee.Name + Seniority + Delivery.unit ~ ., sum)
    subs_dc_smydata<-rename(subs_dc_smydata, Utilization=.)
    subs_dc_smydata<-subs_dc_smydata[subs_dc_smydata$Utilization <= 0.5, ]
    
    
    ## Forecast 
    subs_fc<-fc_df[fc_df$Technology==tech_list[j], c("id", "Project title","PM", "Location","Seniority", "Profile start",
                                                     "Involvement","Project.probability","Forecast_FTE")]
    #remove ids
    subs_fc<-subs_fc[,-which(names(subs_fc) %in% ("id"))]
    subs_fc<-subs_fc %>% 
      rename(FC=Forecast_FTE, 
             Snrty=Seniority,
             City=Location,
             Inv=Involvement,
             Prob = Project.probability)
    
    subs_fc<-plyr::rename(subs_fc, replace = c("Profile start" = "Start"))
    
    subs_fc$FC<-round(subs_fc$FC, 1)
    
    subs_fc$`Project title`<-gsub("PMO__","", subs_fc$`Project title`)
    subs_fc$Start<-format(subs_fc$Start, "%d%b%y")
    
    subs_fc[subs_fc$Snrty=="senior","Snrty"]<-"Snr"
    subs_fc[subs_fc$Snrty=="middle","Snrty"]<-"Mid"
    subs_fc[subs_fc$Snrty=="junior","Snrty"]<-"Jun"
    subs_fc[subs_fc$Snrty=="expert","Snrty"]<-"Exp"
    
    # subs_fc[subs_fc$Status=="In Estimation","Status"]<-"Estim"
    # subs_fc[subs_fc$Status=="On Hold","Status"]<-"Hold"
    # subs_fc[subs_fc$Status=="Proposal Sent","Status"]<-"Prp Sent"
    # subs_fc[subs_fc$Status=="Assign PM","Status"]<-"Asgn PM"
    # subs_fc[subs_fc$Status=="First Contact","Status"]<-"1st Cont"
    #subs_fc$Project.title<-strtrim(subs_fc$Project.title, 32)
     

    subs_fc$PM<-paste0(word(subs_fc$PM, start=1, sep=" "), " ", substr(word(subs_fc$PM, start=2, sep=" "), 1,1 ), ".")
    
    #subs_fc<-subs_fc[order(subs_fc$Project.title),]
    
    subs_list[[j]]<-subs_fc
    

  }
#}
  

  

#}## enf of cycle



