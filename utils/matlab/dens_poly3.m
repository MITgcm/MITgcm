function [dens] = dens_poly3(poly3,t,s)
% D=DENS_POLY3(P,T,S)
%
% Calculates in-situ density as approximated by the POLY3 method
% used in the MITgcm.
%  P - coefficients read from file 'POLY3.COEFFS' using INI_POLY3
%  T - potential temperature
%  S - salinity
%
% eg.
% >> P=ini_poly3;
% >> T=rdmds('T',100);
% >> S=rdmds('S',100);
% >> D=dens_poly3(P,T,S);
%
% or to work within a single model level
% >> D=dens_poly3(P(3),T(:,:,3),S(:,:,3));

if size(t) ~= size(s)
 error('T and S must be the same shape and size')
end
%if size(t,ndims(t)) ~= size(poly3,2)
% error('Last dimension of T and S must be the number of levels in P')
%end

n=size(t);
nz=size(poly3,2);

t=reshape(t,[prod(size(t))/nz nz]);
s=reshape(s,[prod(size(t))/nz nz]);

for k=1:nz,
 tRef=poly3(k).t;
 sRef=poly3(k).s;
 dRef=poly3(k).dens;
 tp=t(:,k)-tRef;
 sp=s(:,k)-sRef;

 deltaSig=                                    ...
  poly3(k).coeffs(1) .*tp                     ...
 +poly3(k).coeffs(2)             .*sp         ...
 +poly3(k).coeffs(3) .*tp.*tp                 ...
 +poly3(k).coeffs(4) .*tp        .*sp         ...
 +poly3(k).coeffs(5)             .*sp.*sp     ...
 +poly3(k).coeffs(6) .*tp.*tp.*tp             ...
 +poly3(k).coeffs(7) .*tp.*tp    .*sp         ...
 +poly3(k).coeffs(8) .*tp        .*sp.*sp     ...
 +poly3(k).coeffs(9)             .*sp.*sp.*sp ...
  ;
 dens(:,k)=deltaSig+dRef;
end

dens=reshape(dens,n);
dens( find(t==0 & s==0) )=0;
