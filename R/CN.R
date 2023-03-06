#CURVE NUMBERS (98 FOR CON 61 FOR ECO)

#WRITE FUNCTION TO FIND CURVE NUMBER THE NEW WAY 
get_curve_number_new <- function(curve_number,catch_size){
  curve_number = 98
  S = (1000/curve_number)-10
  I = (1.33*S^1.15)
  catch_size = 1468
  
  test<-rain$storms%>%
    group_by(storm_id)%>%
    summarise(depth_mm = sum(depth_mm))%>%
    mutate(Q_mm = ifelse(depth_mm <= I, 
                         0, 
                         (depth_mm - (0.05 * I))^2)/(depth_mm +(0.95*I)))%>%
    mutate(runoff_predict_l = Q_mm*catch_size)
  
}

#CREATE CURVE NUMBER THE OLD WAY TO SEE WHAT'S BETTER 
get_curve_number_old <- function(curve_number,catch_size){
  curve_number = 98
  S= (1000/curve_number)-10
  I = 0.2*S
  catch_size = 1468
  
  test<-rain$storms%>%
    mutate(Q_mm = ((depth_mm -I)^2)/(depth_mm - I + S))%>%
    mutate(runoff_predict_l = Q_mm*catch_size)
  
}

#USE FUNCTIONS TO CREATE NEW DATA FRAME FOR CON ROOF 
test_new = get_curve_number_new(curve_number = 61,
                        catch_size = 1200)

test_old = get_curve_number_old(curve_number = 61,
                            catch_size = 1200)

#AGGREGATE TO STORMS 
test_new = test_new%>%
  group_by(storm_id)%>%
  summarize(predict_new = sum(runoff_predict_l))

test_old = test_old%>%
  group_by(storm_id)%>%
  summarize(predict_old = sum(runoff_predict_l))

#TEST AGAINST REAL DATA 
a= flow$storms

a= a%>%
  filter(roof == "eco")%>%
  group_by(storm_id)%>%
  filter(flow_l_s > 0.04)%>%
  summarise(real = sum(flow_l))

b = merge(test_new, test_old, by= "storm_id")

b = merge(b,a,by="storm_id")%>%
  mutate(error_new = round(abs((real-predict_new)/predict_new*100),0),
         error_old = round(abs((real-predict_new)/predict_old*100),0))

b

ggplot(b,aes(x=storm_id))+
  geom_line(aes(y=real))+
  geom_point(aes(y=real))+
  geom_point(aes(y=predict_new,color="new"))+
  geom_point(aes(y=predict_old,color="old"))+
  theme_bw()

#TOTAL RAIN VOLUME
sum(rain$storms$depth_mm*1200)
#TOTAL PREDICTED DISCHARGE 
sum(b$predict_new)
#ACTUAL DISCHARGE
flow$storms%>%filter(roof == "eco")%>%summarise(sum(flow_l))


