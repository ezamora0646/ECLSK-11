
######## LOGISTIC REGRESSION - COMPLETE CASE ANALYSIS - AERA SUBMISSION ######
rm(comp.kndr)

comp.kndr <- na.omit(dll.atl.kndr)


dll.atl.kndr$x2povty <- factor(dll.atl.kndr$x2povty, ordered=T)
dll.atl.kndr$a1hghstd <- factor(dll.atl.kndr$a1hghstd, ordered=T)
dll.atl.kndr$a1yrborn <- factor(dll.atl.kndr$a1yrborn, ordered = T)

no.wgt.comp.kndr <- comp.kndr[, setdiff(names(comp.kndr), c("w12t1", "w12t2", "w12t3", "w12t4", "w12t5", 
                                                     "w12t6", "w12t7", "w12t8", "w12t9", "w12t10", 
                                                     "w12t11", "w12t12", "w12t13", "w12t14", "w12t15", 
                                                     "w12t16", "w12t17", "w12t18", "w12t19", "w12t20", 
                                                     "w12t21", "w12t22", "w12t23", "w12t24", "w12t25", 
                                                     "w12t26", "w12t27", "w12t28", "w12t29", "w12t30", 
                                                     "w12t31", "w12t32", "w12t33", "w12t34", "w12t35", 
                                                     "w12t36", "w12t37", "w12t38", "w12t39", "w12t40", 
                                                     "w12t41", "w12t42", "w12t43", "w12t44", "w12t45", 
                                                     "w12t46", "w12t47", "w12t48", "w12t49", "w12t50", 
                                                     "w12t51", "w12t52", "w12t53", "w12t54", "w12t55", 
                                                     "w12t56", "w12t57", "w12t58", "w12t59", "w12t60", 
                                                     "w12t61", "w12t62", "w12t63", "w12t64", "w12t65", 
                                                     "w12t66", "w12t67", "w12t68", "w12t69", "w12t70", 
                                                     "w12t71", "w12t72", "w12t73", "w12t74", "w12t75", 
                                                     "w12t76", "w12t77", "w12t78", "w12t79", "w12t80", 
                                                     "w12t0"))]

comp.fst <- dll.atl.frst[,setdiff(names(dll.atl.frst), c("w4cf4p21", "w4cf4p22", "w4cf4p23", "w4cf4p24", 
                                                            "w4cf4p25", "w4cf4p26", "w4cf4p27", "w4cf4p28", 
                                                            "w4cf4p29", "w4cf4p210", "w4cf4p211", "w4cf4p212", 
                                                            "w4cf4p213", "w4cf4p214", "w4cf4p215", "w4cf4p216", 
                                                            "w4cf4p217", "w4cf4p218", "w4cf4p219", "w4cf4p220", 
                                                            "w4cf4p221", "w4cf4p222", "w4cf4p223", "w4cf4p224", 
                                                            "w4cf4p225", "w4cf4p226", "w4cf4p227", "w4cf4p228", 
                                                            "w4cf4p229", "w4cf4p230", "w4cf4p231", "w4cf4p232", 
                                                            "w4cf4p233", "w4cf4p234", "w4cf4p235", "w4cf4p236", 
                                                            "w4cf4p237", "w4cf4p238", "w4cf4p239", "w4cf4p240", 
                                                            "w4cf4p20"))]
comp.fst <- na.omit(comp.fst)
comp.fst <- as.data.frame(comp.fst)
rm(comp.fst)

wght.comp.fst <- dll.atl.frst
wght.comp.fst <- na.omit(wght.comp.fst)

dll.atl.frst$x4povty_i <- factor(dll.atl.frst$x4povty_i, ordered = T)
dll.atl.frst$a4yrborn <- factor(dll.atl.frst$a4yrborn, ordered = T)
dll.atl.frst$a4hghstd <- factor(dll.atl.frst$a4hghstd, ordered = T)


dll.atl.kndr <- rebindAttributes(dll.atl.kndr, attributeData = eclsk11)
frst.atl <- rebindAttributes(frst.atl, attributeData = eclsk11)

comp.kndr$w12t0 <- large.df$w12t0


#closeness
kndr.model <- glm.cluster(I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
                        x1tchapp+x2prnapp+x2inbcnt+x2attnfs+
                        a1yrstch+a1hghstd+
                        x2pltot+x2clsnss+x2cnflct+x2pltot:x2clsnss, 
                        cluster="s2_id",
                      data=comp.kndr)
summary(kndr.model)
exp(coefficients(kndr.model))
confint.default(kndr.model)


kndr.model.2 <- glm(I(cat.atl=="Low") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
                    x1tchapp+x2prnapp+x2inbcnt+x2attnfs+x2tchext+
                    a1yrstch+a1hghstd+
                    x2pltot+x2clsnss+x2pltot:x2clsnss, 
                  data=comp.kndr)
summary(kndr.model.2)
exp(coefficients(kndr.model.2))

#conflict Measure
kndr.model.3 <- glm(I(cat.atl=="High") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
                      x1tchapp+x2prnapp+x2inbcnt+x2attnfs+x2tchext+
                      a1yrstch+a1hghstd+
                      x2pltot+x2cnflct+x2pltot:x2cnflct, 
                    data=comp.kndr)

summary(kndr.model.3)
exp(coefficients(kndr.model.3))

kndr.model.4 <- glm(I(cat.atl=="Low") ~ x1tchapp+x2kage_r+x2povty+kndr_par_ed+
                      x1tchapp+x2prnapp+x2inbcnt+x2attnfs+x2tchext+
                      a1yrstch+a1hghstd+
                      x2pltot+x2cnflct+x2pltot:x2cnflct, 
                    data=comp.kndr)
summary(kndr.model.4)
exp(coefficients(kndr.model.4))


#closeness measure - First Grade 
frst.model <- glm(I(fst.cat.atl=="High") ~ x3tchapp+x4age+x4povty_i+fst_par_ed+
                    +x4prnapp+x4inbcnt+x4attnfs+x4tchext+
                    a4yrstch+a4hghstd+
                    frst.prof+x4clsnss+frst.prof:x4clsnss,
                  data= comp.frst)
summary(frst.model)
exp(coefficients(frst.model))

frst.model.2 <- glm(I(fst.cat.atl=="Low") ~ x3tchapp+x4age+x4povty_i+fst_par_ed+
                    x3tchapp+x4prnapp+x4inbcnt+x4attnfs+x4tchext+
                    a4yrstch+a4hghstd+
                    frst.prof+x4clsnss+frst.prof:x4clsnss,
                  data= comp.frst)

summary(frst.model.2)
exp(coefficients(frst.model.2))

#Conflict Measure 

frst.model.3 <- glm(I(fst.cat.atl=="High") ~ x3tchapp+x4age+x4povty_i+fst_par_ed+
                    x3tchapp+x4prnapp+x4inbcnt+x4attnfs+x4tchext+
                    a4yrstch+a4hghstd+
                    frst.prof+x4cnflct+frst.prof:x4cnflct,
                  data= comp.frst)
summary(frst.model.3)
exp(coefficients(frst.model.3))

frst.model.4 <- glm(I(fst.cat.atl=="Low") ~ x3tchapp+x4age+x4povty_i+fst_par_ed+
                      x3tchapp+x4prnapp+x4inbcnt+x4attnfs+x4tchext+
                      a4yrstch+a4hghstd+
                      frst.prof+x4cnflct+frst.prof:x4cnflct,
                    data= comp.frst)
summary(frst.model.4)
exp(coefficients(frst.model.4))
