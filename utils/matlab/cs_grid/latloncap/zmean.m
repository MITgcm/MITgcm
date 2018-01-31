function bz =zmean(var,Zl)
% Sum a field in the vertical according to spacing Zl
% and divide by total spacing.
%

bz=var(:,:,1).*0;
d=0.;
for k=1:size(var,3)
 dd=(Zl(k)-Zl(k+1));
 bz=bz+var(:,:,k).*dd;
 d=d+dd;
end
bz=bz./d;
return 
end
