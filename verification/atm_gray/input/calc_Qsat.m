function [qsat,dQdT]=calc_Qsat(kpot,p,t);
% [qsat,dQdT]=calc_Qsat(kpot,p,t);
%  kpot=0 : t = T = absolute Temp ; kpot=1 : t = theta = pot.temp
%  p = normalized pressure = p/1000.mb
%  output: qsat (g/kg) ; d.qsat/d.T
%  ktyp=1 : p in mb ; t = absolute Temp ;

g=9.81;
kappa=2./7.;

np=prod(size(p));
nt=prod(size(t));
nx=nt/np;
if nx ~= fix(nx) | nx < 1 
 fprintf(' Pb in dimensions: dim_p,dim_t= %i , %i ; ratio= %f \n',np,nt,nx);
 qsat=0.; dQdT=0.;
 return
end

T=reshape(t,nx,np);
P=reshape(p,1,np);
if kpot == 1,
 factp=P.^kappa;
 fp=ones(nx,1)*factp;
 size(fp);
 T=fp.*T;
end

e0=6.108e-3 ; c1=17.269 ; c2=21.875 ; t0c=273.16 ; ct1=35.86 ; ct2=7.66 ;
c0q=622. ; c1q=0.378 ;

qsat=zeros(nx,np);
dQdT=zeros(nx,np);

for k=1:np,
 for j=1:nx, 
  if T(j,k) >= t0c
    qE=e0*exp(c1*(T(j,k)-t0c)/(T(j,k)-ct1)) ;
    qsat(j,k)=c0q*qE/(P(k)-c1q*qE);
    dQdT(j,k)=c1*P(k)*qsat(j,k)/(P(k)-c1q*qE)*(t0c-ct1)/(T(j,k)-ct1)^2 ;
  elseif T(j,k) > ct2
    qE=e0*exp(c2*(T(j,k)-t0c)/(T(j,k)-ct2)) ;
    qsat(j,k)=c0q*qE/(P(k)-c1q*qE);
    dQdT(j,k)=c2*P(k)*qsat(j,k)/(P(k)-c1q*qE)*(t0c-ct2)/(T(j,k)-ct2)^2 ;
  end
 end
end

return
