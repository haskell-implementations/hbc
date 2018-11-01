module
export fac;
rec fac 0 = 1
||  fac n = n*fac(n-1)
end
