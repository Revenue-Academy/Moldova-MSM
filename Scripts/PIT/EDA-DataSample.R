'Descriptive statistcs of sample'



bus_pit_cols <- c("ai_17_r8c2",# Form AI-17
                  "cet18_f5",
                  "daj17_control",
                  "dass19_r130",
                  "ven12_r150",
                  "unif21_t1r120",
                  "taxi18_t1c9_cur"
                  
                  )




# I.  ---------------------------------------------------------------------

test_data<-dt%>%
  select(all_of(bus_pit_cols))


sum(test_data$ai_17_r8c2,na.rm = TRUE)/1E06
sum(test_data$cet18_f5,na.rm = TRUE)/1E06
sum(test_data$daj17_control,na.rm = TRUE)/1E06
sum(test_data$dass19_r130,na.rm = TRUE)/1E06
sum(test_data$ven12_r150,na.rm = TRUE)/1E06
sum(test_data$unif21_t1r120,na.rm = TRUE)/1E06
sum(test_data$taxi18_t1c9_cur,na.rm = TRUE)/1E06



# II.  IALS ---------------------------------------------------------------------


test_data<-dt%>%
  select(starts_with("ials21_sumimp_cur_"))%>%
  data.table()


# Convert to data.table if not already
test_data <- as.data.table(dt)

# Sum each numerical column starting with "ials21_sumimp_cur_" and divide by 1e6
sum_data <- test_data[, lapply(.SD, function(x) sum(x, na.rm = TRUE) / 1e6), .SDcols = patterns("^ials21_sumimp_cur_")]

# View the result
print(sum_data)


# 1.Salaries ---------------------------------------------------------------


test_data<-pit_data$ials21%>%
 
  select(sumven_cur_SAL,sumimp_cur_SAL)%>%
  
  data.table()


View(test_data)



