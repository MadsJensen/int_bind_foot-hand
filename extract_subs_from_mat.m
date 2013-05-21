

for j = 1:16
  
  if j < 10
    eval(sprintf(' fName = sub0%d', j));
  else
    eval(sprintf('fName = sub%d', j));
  end
  
  eval('data = [fName.M_hand.errorTime, fName.W_hand.errorTime, fName.M_foot.errorTime, fName.W_foot.errorTime]');
  
  output_name = sprintf('libet_sub_%d.csv', j);
  eval('csvwrite(output_name, data)');
 
end