#This file creates a new data frame MenuCH_DPA, ...
#... in which each individual is only one row and the food info is summarized by...
# category, subcategory and nutrient, and this for the 2 food recalls separately 

####################Data import ---------- The file is encoded with German characters in ISO-8859-1##################
MenuCH <- read.csv("/Volumes/AbtII-Krebs-Epi/08_MenuCH/Microdata files/Data from FoodCase/res_consumption_data_V02_2017_04.csv",sep=";",fileEncoding="ISO-8859-1")
MenuCH_Quest <- read.csv("/Volumes/AbtII-Krebs-Epi/08_MenuCH/Microdata files/Data from the dietary behavior and physical activity/res_anthropometry_questionnaire_V02_2017_04.csv",sep=";")
MenuCH_Stat <- read.csv("/Volumes/AbtII-Krebs-Epi/08_MenuCH/Microdata files/Data from FSO_Gross sample/res_federal_statistical_office_sample_V02_2017_04.csv",sep=";")

MenuCH_weights <- read.csv("/Volumes/AbtII-Krebs-Epi/08_MenuCH//Microdata files/Data from statistical weights/res_weights_V02_2017_04.csv",sep=";")

########################sort raw data by id number and type of interview####################
MenuCH <- MenuCH[order(MenuCH$id_num,MenuCH$Interview.type),]


################################finds out all the id_num to loop through and build the new dataframe####################
all_id_num<-as.factor(as.integer(MenuCH$id_num))
id_num_vector<-levels(all_id_num)

#initializes a counter just to see where we stand in the loop
j=0 

for (i in id_num_vector){

  #here is a working data frame in which we isolate one individual's data (intermediate step also protects the raw data from wrong manip)
  MenuCH_working1<-subset(MenuCH,MenuCH$id_num==i & MenuCH$Interview.type==1)
  MenuCH_working2<-subset(MenuCH,MenuCH$id_num==i & MenuCH$Interview.type==2)
  
  #we store the info common to an individual...
  linecommon<-MenuCH_working1[1,"id_num"] #adding common data to both interview
  # ... and the info common to one 24h recall (day1/day2)
  line_int1<-MenuCH_working1[1,c("Recall.date","Recall.day.number","Diet_recoded_a","Day.type","Interview.date","Interview.day.number","Height","Weight","Wakeup","Wakeup.next.day","Moment","Time","Place")]  #adding the data for interview 1
  line_int2<-MenuCH_working2[1,c("Recall.date","Recall.day.number","Diet_recoded_a","Day.type","Interview.date","Interview.day.number","Height","Weight","Wakeup","Wakeup.next.day","Moment","Time","Place")] #adding the data for interview 2
  
  #computing the food category information: how much food was consumed by category of food? In both 24h recall (only if the food contains calories)
  food_int1<-as.data.frame(tapply(MenuCH_working1$Amount,MenuCH_working1$Category.German,FUN=sum))
  food_int2<-as.data.frame(tapply(MenuCH_working2$Amount,MenuCH_working2$Category.German,FUN=sum))
  
  #computing the food subcategory information (only if the food contains calories)
  foodSc_int1<-as.data.frame(tapply(MenuCH_working1$Amount,MenuCH_working1$Subcategory.German,FUN=sum))
  foodSc_int2<-as.data.frame(tapply(MenuCH_working2$Amount,MenuCH_working2$Subcategory.German,FUN=sum))
  

  
  #computing the nutrient values information (!!!! energy and nutrient are given / 100g !!!!) And recepts should not be taken into account
  nut_int1<-as.data.frame(lapply(MenuCH_working1[MenuCH_working1$Category.German!="Rezepte",44:80], function(x) if(is.numeric(x)) sum(x*MenuCH_working1[MenuCH_working1$Category.German!="Rezepte","Amount"]/100, na.rm=T)))
  nut_int2<-as.data.frame(lapply(MenuCH_working2[MenuCH_working2$Category.German!="Rezepte",44:80], function(x) if(is.numeric(x)) sum(x*MenuCH_working2[MenuCH_working2$Category.German!="Rezepte","Amount"]/100, na.rm=T)))

  #TRANSPOSES AND REPLACES NAs by ZEROS.
  
transposeandzero <- function(df) {
    df<-t(df)
    df[is.na(df)]<-0
    return(df)
}

food_int1<-transposeandzero(food_int1)
food_int2<-transposeandzero(food_int2)
foodSc_int1<-transposeandzero(foodSc_int1)
foodSc_int2<-transposeandzero(foodSc_int2)

  #if this is the first person, we start with this data, if this is not, we add the new data to the table
  if (i==422118){
    MenuCH_DPA<-cbind(linecommon,line_int1,food_int1,foodSc_int1,nut_int1,line_int2,food_int2,foodSc_int2,nut_int2)
    } else {
    newline<-cbind(linecommon,line_int1,food_int1,foodSc_int1,nut_int1,line_int2,food_int2,foodSc_int2,nut_int2)
    MenuCH_DPA<-rbind(MenuCH_DPA,newline)  
  }

#some counters to see where we are
print(i)
j=j+1
print(j)
}

EnglishNames <- read.csv("/Volumes/AbtII-Krebs-Epi/08_MenuCH/VarNames/VarNames_EnglishNames.csv")

#We rename the columns, including short/comprehensible English names for the food categories
colnames(MenuCH_DPA)<-EnglishNames$x_eng.1


####################################To avoid restarting the process all the time######################
write.csv(MenuCH_DPA,file="MenuCH_DPA_provisory.csv",row.names=FALSE)

MenuCH_DPA<-read.csv(file="MenuCH_DPA_provisory.csv")


#####################################Then we link it to the questionnaire data##############################################

MenuCH_DPA <- merge(MenuCH_DPA,MenuCH_Quest,by="id_num")
MenuCH_DPA <- merge(MenuCH_DPA,MenuCH_Stat[,c("id_num","bigregion","reportingcanton")],by="id_num")

####################################Here come all the choices that we make related to the food groups##########################


#############separating the meat category into white and red+processed and removing Meat substitutes##############
  #meat is a sum of the following subcategories
  #Chicken_sc_i1 > white
  #Offal_sc_i1 > red+processed (liver, tongue, ...)
  #NS_meat_sc_i1 > red+processed (steak, minced meat, boiled meat, ..)
  #Meat_Mammals_sc_i1 > red+processed (neef, pork, veal,...)
  #BasedOnMeat_sc_i1 > red+processed (also it is mixed)
  #Wild_meat_sc_i1 > red+processed (Deer, ...)
  #Processed_meat_sc_i1 > red+processed (sausages, etc)
  #Meat_substitute_sc_i1 > not meat (tofu, veggi ersatz)


MenuCH_DPA$Meat_white_i1<-MenuCH_DPA$Chicken_sc_i1
MenuCH_DPA$Meat_white_i2<-MenuCH_DPA$Chicken_sc_i2

MenuCH_DPA$Meat_redpro_i1<-MenuCH_DPA$Meat_i1-MenuCH_DPA$Chicken_sc_i1-MenuCH_DPA$Meat_substitute_sc_i1
MenuCH_DPA$Meat_redpro_i2<-MenuCH_DPA$Meat_i2-MenuCH_DPA$Chicken_sc_i2-MenuCH_DPA$Meat_substitute_sc_i2

#we reorganize variable order to have the new meat variables within the category variables
MenuCH_DPA <- MenuCH_DPA[ ,c(1:22,454,456,23:175,455,457,176:453)]

#we drop the original meat variables
MenuCH_DPA$Meat_i1<-NULL
MenuCH_DPA$Meat_i2<-NULL


############################removing milk substitutes####################
MenuCH_DPA$Milk_i1<-MenuCH_DPA$Milk_i1 - MenuCH_DPA$Milk_substitutes_sc_i1
MenuCH_DPA$Milk_i2<-MenuCH_DPA$Milk_i2 - MenuCH_DPA$Milk_substitutes_sc_i2


############################combining cereals and starchy################
MenuCH_DPA$Cereals_Starchy_i1<-MenuCH_DPA$Cereals_i1+MenuCH_DPA$Starchy_i1
MenuCH_DPA$Cereals_Starchy_i2<-MenuCH_DPA$Cereals_i2+MenuCH_DPA$Starchy_i2

#we drop the original variables
MenuCH_DPA$Cereals_i1<-NULL
MenuCH_DPA$Starchy_i1<-NULL
MenuCH_DPA$Cereals_i2<-NULL
MenuCH_DPA$Starchy_i2<-NULL

MenuCH_DPA <- MenuCH_DPA[ ,c(1:25,452,26:177,453,178:451)]


###########################combining legumes and vegetables###################

MenuCH_DPA$Vegetables_i1<-MenuCH_DPA$Vegetables_i1+MenuCH_DPA$Legumes_i1
MenuCH_DPA$Vegetables_i2<-MenuCH_DPA$Vegetables_i2+MenuCH_DPA$Legumes_i2

#we drop the original variables
MenuCH_DPA$Legumes_i1<-NULL
MenuCH_DPA$Legumes_i2<-NULL


###########################taking into account only non alcoholic beverages with calories###################

MenuCH_DPA$NonAlcoholic_Bev_i1 <-MenuCH_DPA$Soft_drinks_sc_i1+MenuCH_DPA$Juices_sc_i1
MenuCH_DPA$NonAlcoholic_Bev_i2 <-MenuCH_DPA$Soft_drinks_sc_i2+MenuCH_DPA$Juices_sc_i2



###########################adding the statistical weights (season and week adjusted for 24 HR) ###########################
MenuCH_DPA <- merge(MenuCH_DPA,MenuCH_weights[,c("id_num","calibrated_w","calibrated_w_2rec","sw_calibrated_w_2rec")],by="id_num")


###########################putting id_num as column names###########################
rownames(MenuCH_DPA)<-MenuCH_DPA[,"id_num"]
MenuCH_DPA$id_num<-NULL


##########Reorganizing the table to have coherent variable groups (coherent in terms of types of var cat/cont and in terms of topic)############

#print the current variable names in a csv file, with the class of each variables
#cat<-as.data.frame(lapply(MenuCH_DPA,class))
#nam<-as.data.frame(names(MenuCH_DPA))
#cat<-t(cat)
#namcat<-cbind(nam,cat)
#write.csv(namcat,file="MenuCH_DPA_provisory_2.csv",row.names=FALSE)

#we call an excel file in which variables were reorganized by hand!!
Reorga<-read.csv(file="/Volumes/AbtII-Krebs-Epi/08_MenuCH/VarNames/VarGrouping-forFormatting.csv")
rownames(Reorga)<-Reorga$names.MenuCH_DPA.
MenuCH_DPA <- MenuCH_DPA[,rownames(Reorga)[c(1:453)]]

#these are the indices that can be useful
# 1-5 : info recall 1 (numeric)
# 6-11: info recall 1 (cat)
# 12-29 : food groups recall 1 (numeric)
# 30-113 : food subcategories recall 1 (numeric)
# 114-150 : nutrients recall 1 (numeric)
# 151-155 : info recall 2 (numeric)
# 156-161: info recall 2 (cat)
# 162-179 : food groups recall 2 (numeric)
# 180-263 : food subcategories recall 2 (numeric)
# 264-300 : nutrients recall 2 (numeric)
# 301 : date of the questionnaire (numeric)
# 302-307: general info about the participant (numeric)
# 308-336: general info about the participant (cat)
# 337-353: morphology of the participant (numeric)
# 354-355: morphology of the participant (cat)
# 356-357: smoking and health (cat)
# 358-431: eating habits (cat)
# 432-434: food literacy (cat)
# 435-446: physical activity (numeric)
# 447-450: physical activity (cat)
# 451-452: statistical weights (normal calibration for population table calibrated_w_2rec and season and week calibrated for 24 HR sw_calibrated_w_2rec


#######################EXCLUSION CRITERIA###########################

######### Remove those who have only 1 24HR #######
#28 participants with only a single 24h recall are now excluded

#Let's remove the people that have no full first or full second 24h recall
# 12-29 : food groups recall 1 (numeric)
foodvar1<-names(MenuCH_DPA[,12:29])
# 162-179 : food groups recall 2 (numeric)
foodvar2<-names(MenuCH_DPA[,162:179])

z<-nrow(MenuCH_DPA)

MenuCH_DPA<-MenuCH_DPA[rowSums(MenuCH_DPA[,foodvar1]) !=0,]
MenuCH_DPA<-MenuCH_DPA[rowSums(MenuCH_DPA[,foodvar2]) !=0,]

#number of cases lost
nlost<-z-nrow(MenuCH_DPA)
print(nlost)






##############WRITING IN A FILE#####################


write.csv(MenuCH_DPA,file="MenuCH_treated_forDPA.csv",row.names=TRUE)


############BONUS: printing all categories and belonging subcategories of the original file##################

#finds the max number of subcategories
#nsubcat<-c()
#for (i in levels(MenuCH$Category.German)){
#  nsubcat<-c(nsubcat,nlevels(droplevels(MenuCH[MenuCH$Category.German==i,"Subcategory.German"])))
#}
#maxsubcat<-max(nsubcat)

#lists all subcategories by category
#tablesubcat<-data.frame(matrix(nrow=10))
#for (i in levels(MenuCH$Category.German)){
#  subcat<-levels(droplevels(MenuCH[MenuCH$Category.German==i,"Subcategory.German"]))
#  length(subcat)<-maxsubcat
#  tablesubcat<-cbind(tablesubcat,subcat)
#}#

#tablesubcat$matrix.nrow...10.<-NULL
#colnames(tablesubcat)<-paste("Cat.",levels(MenuCH$Category.German))

#saves the table
#write.csv(tablesubcat,file="/Volumes/AbtII-Krebs-Epi/08_MenuCH/VarNames/CatAndSubcat_OriginalFile.csv",row.names=FALSE)


#MenuCH[MenuCH$Subcategory.German=="Suppen",c("Category.German","id_num")]

