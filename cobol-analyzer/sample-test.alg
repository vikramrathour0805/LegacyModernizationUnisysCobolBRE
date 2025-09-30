comment Sample ALGOL program for testing;
begin
  integer i, sum, n;
  real average;
  
  procedure calculate_sum(x, y);
  integer x, y;
  begin
    sum := x + y;
  end;
  
  procedure calculate_average;
  begin
    if sum > 0 then
      average := sum / n
    else
      average := 0;
  end;
  
  n := 10;
  sum := 0;
  
  for i := 1 step 1 until n do
  begin
    sum := sum + i;
  end;
  
  calculate_average;
  
  write(sum, average);
end 