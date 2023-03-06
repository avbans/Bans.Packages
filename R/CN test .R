curve_number = 61
S = (1000/curve_number)-10
I = (1.33*S^1.15)
catch_size = 1200
p= 16.3

Q= ((p - (0.05 * I))^2)/(p+(0.95 * I))
V_out = Q *catch_size
V_in = p*catch_size

ret= (V_in-V_out)/V_in*100

get_curve_number <- function(curve_number,catch_size){
  curve_number
  S = (1000/curve_number)-10
  I = (1.33*S^1.15)
  catch_size = 1468

  test<-rain$storms%>%
    group_by(storm_id)%>%
    summarise(depth_mm = sum(depth_mm))%>%
    mutate(Q_mm = ifelse(depth_mm <= I,
                         0,
                         (depth_mm - (0.05 * I))^2)/(depth_mm +(0.95*I)))

}

a = rain$storms
a = get_curve_number(curve_number = 98,
                                catch_size = 1468)
sum(a$depth_mm)*1468
sum(a$Q_mm*1468)
