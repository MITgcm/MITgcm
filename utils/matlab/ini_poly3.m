function [poly3] = ini_poly3()
% P=INI_POLY3
%
% Reads the file 'POLY3.COEFFS' and returns coefficients in P

fid=fopen('POLY3.COEFFS','r');

n=fscanf(fid,'%i',1);

for k=1:n,
 a=fscanf(fid,'%g',3);
 T(k)=a(1);
 S(k)=a(2);
 D(k)=a(3);
 poly3(k).t=a(1);
 poly3(k).s=a(2);
 poly3(k).dens=a(3);
end
for k=1:n,
 a=fscanf(fid,'%g',9);
 P(k,:)=a';
 poly3(k).coeffs=a';
end
P(:,10)=T';
P(:,11)=S';
P(:,12)=D';
