data_model<- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(april_bill = sum(ifelse(month(created_at_bill) == 4,1,0)),
            may_bill = sum(ifelse(month(created_at_bill) == 5,1,0)),
            june_bill = sum(ifelse(month(created_at_bill) == 6,1,0)),
            july_bill = sum(ifelse(month(created_at_bill) == 7,1,0)), 
            Aug_bill = sum(ifelse(month(created_at_bill) == 8,1,0)),
            Sep_bill = sum(ifelse(month(created_at_bill) == 9,1,0)))

dummy<- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_spent = total_spend_bill)

dummy<- aggregate(total_spent~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0


data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_ethical_drugs = quantity_ethical)
dummy<- aggregate(total_ethical_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_generic_drugs = quantity_generic)
dummy<- aggregate(total_generic_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_surgical_drugs = quantity_surgical)
dummy<- aggregate(total_surgical_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_ayurvedic_drugs = quantity_ayurvedic)
dummy<- aggregate(total_ayurvedic_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_general_drugs = quantity_general)
dummy<- aggregate(total_general_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_otc_drugs = quantity_otc)
dummy<- aggregate(total_otc_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_chronic_drugs = quantity_chronic)
dummy<- aggregate(total_chronic_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_acute_drugs = quantity_acute)
dummy<- aggregate(total_acute_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")

dummy <- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
  summarise(month = month(created_at_bill),
            total_h1_drugs = quantity_h1)
dummy<- aggregate(total_h1_drugs~month+customer_ref_id, data = dummy, FUN = sum)

dummy<- reshape(data = dummy, idvar = 'customer_ref_id', timevar = 'month', direction = 'wide')

dummy[is.na(dummy)]<- 0

data_model<- full_join(data_model, dummy, by = "customer_ref_id")


data_model_train<- 