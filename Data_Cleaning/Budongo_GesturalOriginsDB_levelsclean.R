library(dplyr)
budongo<-as.data.frame(budongo%>%
                         mutate(Goal=ifelse(Goal=="AffilationContact", "Aff_con",
                                            ifelse(Goal=="AffilationUnclear","Aff_unc", 
                                                   ifelse(Goal =="ClimbOnMe", "Cl_sgn", 
                                                          ifelse(Goal == "ClimbOnYou", "Cl_rcp", 
                                                                 ifelse(Goal =="DirectAttention", "Dir_att", 
                                                                        ifelse(Goal =="GiveMeNurse", "Gv_objfd_ns", 
                                                                               ifelse(Goal =="Groom", "Grm", 
                                                                                      ifelse(Goal =="GroomMe", "Grm_sgn", 
                                                                                             ifelse(Goal =="GroomYou", "Grm_rcp", 
                                                                                                    ifelse(Goal =="MoveAway", "Move_away", 
                                                                                                           ifelse(Goal =="MoveClose","Move_close",
                                                                                                                  ifelse(Goal=="SexualAttention", "Sex_att", 
                                                                                                                         ifelse(Goal=="SexualAttentionMe", "Sex_att_sgn", 
                                                                                                                                ifelse(Goal =="SocioSexualAttention", "Socio_sex_att",
                                                                                                                                       ifelse(Goal=="SocioSexualAttentionMe", "Socio_sex_att_sgn", 
                                                                                                                                              ifelse(Goal=="StaySame", "Stay_same", 
                                                                                                                                                     ifelse(Goal=="StopBehaviour", "Stp_bh",
                                                                                                                                                            ifelse(Goal=="TakeObject", "Tk_objfd",
                                                                                                                                                                   ifelse(Goal=="TakeObjectNurse", "Tk_objfd_ns",
                                                                                                                                                                          ifelse(Goal=="TravelMe", "Trv_sgn",
                                                                                                                                                                                 ifelse(Goal=="TravelYou", "Trv_rcp", 
                                                                                                                                                                                        ifelse(Goal=="Unknown", "Unk", as.character(Goal)))))))))))))))))))))))))

budongo<-(as.data.frame(budongo%>%
                          mutate(Rcp_post_context=ifelse(Rcp_post_context=="PlaySocial", "Play_soc",
                                                         ifelse(Rcp_post_context=="PlaySolitary", "Play_sol",
                                                                ifelse(Rcp_post_context=="RestContact", "Rest_con",
                                                                       ifelse(Rcp_post_context=="RestUnknown", "Rest_unk",
                                                                              ifelse(Rcp_post_context=="SocioSex","Socio_sex",
                                                                                     ifelse(Rcp_post_context=="TravelCarry","Travel_carry",
                                                                                            ifelse(Rcp_post_context=="TravelCarryMove","Travel_carry_move",
                                                                                                   ifelse(Rcp_post_context=="TravelMove","Travel_move",
                                                                                                          ifelse(Rcp_post_context=="GroomSocial","Groom_soc",
                                                                                                                 ifelse(Rcp_post_context=="GroomSolitary","Groom_sol",
                                                                                                                        ifelse(Rcp_post_context=="Unknown","Unk",as.character(Rcp_post_context)))))))))))))))

budongo<-(as.data.frame(budongo%>%
                          mutate(Rcp_prior_context=ifelse(Rcp_prior_context=="PlaySocial", "Play_soc",
                                                          ifelse(Rcp_prior_context=="PlaySolitary", "Play_sol",
                                                                 ifelse(Rcp_prior_context=="RestContact", "Rest_con",
                                                                        ifelse(Rcp_prior_context=="RestUnknown", "Rest_unk",
                                                                               ifelse(Rcp_prior_context=="SocioSex","Socio_sex",
                                                                                      ifelse(Rcp_prior_context=="TravelCarry","Travel_carry",
                                                                                             ifelse(Rcp_prior_context=="TravelCarryMove","Travel_carry_move",
                                                                                                    ifelse(Rcp_prior_context=="TravelMove","Travel_move",
                                                                                                           ifelse(Rcp_prior_context=="GroomSocial","Groom_soc",
                                                                                                                  ifelse(Rcp_prior_context=="GroomSolitary","Groom_sol",
                                                                                                                         ifelse(Rcp_prior_context=="Unknown","Unk",as.character(Rcp_prior_context)))))))))))))))

budongo<-(as.data.frame(budongo%>%
                          mutate(Sgn_post_context=ifelse(Sgn_post_context=="PlaySocial", "Play_soc",
                                                         ifelse(Sgn_post_context=="PlaySolitary", "Play_sol",
                                                                ifelse(Sgn_post_context=="RestContact", "Rest_con",
                                                                       ifelse(Sgn_post_context=="RestUnknown", "Rest_unk",
                                                                              ifelse(Sgn_post_context=="SocioSex","Socio_sex",
                                                                                     ifelse(Sgn_post_context=="TravelCarry","Travel_carry",
                                                                                            ifelse(Sgn_post_context=="TravelCarryMove","Travel_carry_move",
                                                                                                   ifelse(Sgn_post_context=="TravelMove","Travel_move",
                                                                                                          ifelse(Sgn_post_context=="GroomSocial","Groom_soc",
                                                                                                                 ifelse(Sgn_post_context=="GroomSolitary","Groom_sol",
                                                                                                                        ifelse(Sgn_post_context=="Unknown","Unk",as.character(Sgn_post_context)))))))))))))))

budongo<-(as.data.frame(budongo%>%
                          mutate(Sgn_prior_context=ifelse(Sgn_prior_context=="PlaySocial", "Play_soc",
                                                          ifelse(Sgn_prior_context=="PlaySolitary", "Play_sol",
                                                                 ifelse(Sgn_prior_context=="RestContact", "Rest_con",
                                                                        ifelse(Sgn_prior_context=="RestUnknown", "Rest_unk",
                                                                               ifelse(Sgn_prior_context=="SocioSex","Socio_sex",
                                                                                      ifelse(Sgn_prior_context=="TravelCarry","Travel_carry",
                                                                                             ifelse(Sgn_prior_context=="TravelCarryMove","Travel_carry_move",
                                                                                                    ifelse(Sgn_prior_context=="TravelMove","Travel_move",
                                                                                                           ifelse(Sgn_prior_context=="GroomSocial","Groom_soc",
                                                                                                                  ifelse(Sgn_prior_context=="GroomSolitary","Groom_sol",
                                                                                                                         ifelse(Sgn_prior_context=="Unknown","Unk",as.character(Sgn_prior_context)))))))))))))))

budongo<-as.data.frame(budongo%>%
                         mutate(Gesture_record=ifelse(Gesture_record=="BigLoudScratch","Bg_ld_scr",
                                                      ifelse(Gesture_record=="BiteKiss","Bite_ks",
                                                             ifelse(Gesture_record=="GrabHold","Grab_hold",
                                                                    ifelse(Gesture_record=="ObjectMoveUnattached","Obj_mv_unatt",
                                                                           ifelse(Gesture_record=="PresentDirected","Prs_dir",
                                                                                  ifelse(Gesture_record=="Present","Prs",
                                                                                         ifelse(Gesture_record=="ObjectShake","Obj_shk",
                                                                                                ifelse(Gesture_record=="PresentGenitalsBackwards","Prs_gn_bwd",
                                                                                                       ifelse(Gesture_record=="PullDirected","Pull_dir",
                                                                                                              ifelse(Gesture_record=="PushDirected","Push_dir",
                                                                                                                     ifelse(Gesture_record=="HitOther","Hit_rcp",
                                                                                                                            ifelse(Gesture_record=="HitOtherSoft","Hit_rcp_sft",
                                                                                                                                   ifelse(Gesture_record=="SwingDirected","Swing_dir",
                                                                                                                                          ifelse(Gesture_record=="TouchLongOther","Tch_lg_rcp", as.character(Gesture_record)))))))))))))))))


budongo<-as.data.frame(budongo%>%
                         mutate(Goal_met=ifelse(Goal_met=="YesForced","Yes_forced",
                                                ifelse(Goal_met=="Unknown","Unk", as.character(Goal_met)))))

budongo<-as.data.frame(budongo%>%
                         mutate(Distance_rcp=ifelse(Distance_rcp%in%c("o_3","O_3"),"o_3",
                                ifelse(Distance_rcp%in%c("over_5","Over_5"),"over_5",
                                       ifelse(Distance_rcp%in%c("u_3","U_3"),"u_3",as.character(Distance_rcp))))))
budongo<-droplevels(budongo)
levels(as.factor(budongo$Gesture_record))