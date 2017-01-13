#{Blueberry Tart,Apricot Croissant} => {Hot Coffee}
#{Apple Pie,Almond Twist} => {Hot Coffee}
#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
#{Chocolate Tart,Walnut Cookie} => {Vanilla Frappuccino}
tran1rules <- apriori(tran1,parameter = list(sup =  0.01 ,conf =  0.6 ,minlen= 2 ,target='rules'))
tran1rules <- apriori(tran1,parameter = list(sup =  0.01 ,conf =  0.7 ,minlen= 2 ,target='rules'))
tran1rules <- apriori(tran1,parameter = list(sup =  0.01 ,conf =  0.8 ,minlen= 2 ,target='rules'))
tran1rules <- apriori(tran1,parameter = list(sup =  0.01 ,conf =  0.9 ,minlen= 2 ,target='rules'))

#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
#{Apple Pie,Almond Twist} => {Hot Coffee}
#{Almond Twist,Coffee Eclair} => {Hot Coffee}
#{Apple Pie,Coffee Eclair} => {Hot Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.02 ,conf =  0.6 ,minlen= 2 ,target='rules'))

#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
#{Blueberry Tart,Apricot Croissant} => {Hot Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.03 ,conf =  0.6 ,minlen= 2 ,target='rules'))

#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
#{Blueberry Tart,Apricot Croissant} => {Hot Coffee}
#{Apple Pie,Almond Twist} => {Hot Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.02 ,conf =  0.7 ,minlen= 2 ,target='rules'))

#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
#{Blueberry Tart,Apricot Croissant} => {Hot Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.03 ,conf =  0.7 ,minlen= 2 ,target='rules'))

#{Apple Pie,Almond Twist} => {Hot Coffee}
#{Almond Twist,Coffee Eclair} => {Hot Coffee}
#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
#{Blueberry Tart,Apricot Croissant} => {Hot Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.02 ,conf =  0.8 ,minlen= 2 ,target='rules'))

#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
#{Blueberry Tart,Apricot Croissant} => {Hot Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.03 ,conf =  0.8 ,minlen= 2 ,target='rules'))

#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.02 ,conf =  0.9 ,minlen= 2 ,target='rules'))

#{Chocolate Cake,Casino Cake} => {Chocolate Coffee}
tran1rules <- apriori(tran1,parameter = list(sup =  0.03 ,conf =  0.9 ,minlen= 2 ,target='rules'))