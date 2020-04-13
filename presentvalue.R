#Net present value calculator
#Assumes wages paid at end of year and school/investment paid at begining of year
#Last updated 4/13/2020
#contact gwhitesitt@gmail.com for any corrections or suggestions
#@instructions
#set the parameters in the user entered parameters section according to the situation
#you wish to investigate. After these are set just run the code seqentially. The
#variable NPV_for_x_years_worked will be a vector coresponding the the net present value
#for the element number of years worked. Therefore the 5th entry corresponds to the net
#present value for the situation if you plan to work 5 years and the 6th entry corresponds
#to the net present value if you plan to work 6 years and so on. 

#@User entered parameters
old_wage = 14.80
new_wage = 24.50
hours_per_year = 2000
years = 25
yearly_cost_of_school = 10764 / 2
years_in_school = 2
interest_rate = .05

#@MAIN

#@initilize outside look objects
NPV_for_x_years_worked = c()
iter = years

for (j in 1:iter){
#@Initilize objects
PV = c()
years = j
#@Calculations
old_salary = old_wage * hours_per_year
new_salary = new_wage * hours_per_year

#Cost of education
for(i in 1:years_in_school){
  PV_i = -yearly_cost_of_school / (1 + interest_rate)^(i-1)
  PV = append(PV,PV_i)
}
education_cost_pv = sum(PV)
PV = c()

#Loss in Earnings
for(i in 1:years_in_school){
  PV_i = -old_salary / (1 + interest_rate)^i
  PV = append(PV,PV_i)
}
loss_in_earnings_pv = sum(PV)
PV = c()

#Increased Earnings
PV = (new_salary - old_salary) * ((1 - (1 + interest_rate)^-(years - years_in_school)) / interest_rate)
#Discount for years in school
PV = PV / (1 + interest_rate)^years_in_school
increase_in_earnings_pv = PV
PV = c()

#Net present value of school
NPV = education_cost_pv + loss_in_earnings_pv + increase_in_earnings_pv
NPV_for_x_years_worked = append(NPV_for_x_years_worked, NPV)                               
}

#Get rid of NPV entries during school because the calculations will be wrong due to negatives. 
NPV_for_x_years_worked[1:years_in_school] = NA
NPV_for_x_years_worked
