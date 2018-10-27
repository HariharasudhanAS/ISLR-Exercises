Power = function(){
  print(I(2^3))
}
Power2 = function(x,a){
  print(I(x^a))  
}

Power2(10,3)
Power2(8,17)
Power2(131,3)

Power3 = function(x,a){
  return(I(x^a))
}

y = 1:10
plot(y, Power3(y,2), log="xy", type="b")

PlotPower = function(x,a){
  plot(x, I(x^a), log="xy", type="b")
}

PlotPower(1:20,20)
