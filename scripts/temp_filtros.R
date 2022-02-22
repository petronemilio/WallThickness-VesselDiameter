#Regresar a base original
cell <- cell.temp.subseted
cell <- subset(cell, cell$order!="Gnetales")
cell <- subset(cell, cell$ite.type!="fiber tracheids")
cell <- subset(cell, cell$vd > 0.1)
cell <- subset(cell, cell$vwt > 0.1)
cell <- subset(cell, cell$vd < 90)
cell <- subset(cell, cell$simp.scal=="simple"|cell$simp.scal=="scalariform")


length(cell$dwt.conditional)

boxplot(log10(cell$dwt.conditional)~cell$conductive)
t.test(log10(cell$dwt.conditional)~cell$conductive)

boxplot(log10(cell$dwt.conditional)~cell$simp.scal)
t.test(log10(cell$dwt.conditional)~cell$simp.scal)


boxplot(log10(cell$vwt)~cell$conductive)
t.test(log10(cell$vwt)~cell$conductive)

boxplot(log10(cell$vwt)~cell$simp.scal)
t.test(log10(cell$vwt)~cell$simp.scal)

boxplot(log10(cell$ite.wt)~cell$conductive)
t.test(log10(cell$ite.wt)~cell$conductive)

boxplot(log10(cell$ite.wt)~cell$simp.scal)
t.test(log10(cell$ite.wt)~cell$simp.scal)
