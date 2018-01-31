
dzL=20;
hup=200; kL=round(hup/dzL);
N=15;
k=[1:N];
for k=1:N,
 dd(k)=1.20436^k;
end
dd=dd*dzL;
dd=round(dd);
tot=cumsum(dd)/100;
fprintf(' %5.2f',tot(end-5:end));fprintf('\n');
fprintf(' %2.0f.,',dd);fprintf('\n');

%- uniform dz=20 m down to 200m, then increasing exponentially
dz=[ones(1,kL)*dzL dd]; nz=size(dz,2);
zF=[0 -cumsum(dz)]; zC=(zF(1:nz)+zF(2:nz+1))/2;
dy=1.e+5; ny=32;
yF=[0:ny]-ny/2; yF=yF*dy; yC=(yF(1:ny)+yF(2:ny+1))/2;

%- set initial temperature:
N2=4.e-6;
alphaT=2.e-4;
gravity=9.81;
fo=1.e-4;
%- N2= db/dz = g alphaT dT/dz
dTdz=N2/gravity/alphaT;

slope=1e-3;
dTdy=-slope*dTdz;

fprintf(' N= %9.3e ; g= %5.3f ; alphaT= %9.3e ; dTdz= %e\n', ...
         sqrt(N2),gravity,alphaT,dTdz);

Ly=24.*dy; yM=0; Ho=2205;
T=zeros(ny,nz);
km=6; kmx=6;
for k=km:nz,
 T(:,k)=10 +dTdz*zC(k) ...
           +dTdy*Ly*(tanh((yC-yM-200.e3)/200.e3)-tanh((yC-yM+200.e3)/200.e3))/2 ...
           *exp(-(3*zC(k)/Ho)^2);
end
%Create a ML
for k=1:km
 T(:,k)=T(:,km);
end
TML=mean(T(:,1:kmx),2);
for k=1:kmx
 T(:,k)=TML;
end

%-- Thermal wind
alphaT=2.e-4;
dbdy=zeros(ny+1,nz);
dbdy(2:ny,:)=gravity*alphaT*(T([2:ny],:)-T([1:ny-1],:))/dy;
ug=zeros(ny+1,nz+1);
for k=nz:-1:1;
 ug(:,k) = ug(:,k+1) - dbdy(:,k)*dz(k)/fo ;
end
uCg=ug([1:ny],[1:nz])+ug([2:ny+1],[2:nz+1])  ...
   +ug([2:ny+1],[1:nz])+ug([1:ny],[2:nz+1]);
uCg=uCg/4;
%size(uCg)

% Salt (passive tracer)
S=zeros(ny,nz);
for k=1:nz,
%S(:,k)=ones(ny,1)+zC(k)/Ho;  % Linear with z
%S(:,k)=exp(2*zC(k)/Ho);      % Exponential with z
 S(:,k)=exp(-(2*yC/Ly).^2);   % Exponential with y
end
S=10+2*S;

%- plot to check:

figure(1);clf;
subplot(211);
var=squeeze(T); var(find(var==0))=NaN;
%pcolor(yC,zC,var'); colorbar;
[cs,h]=contour(yC,zC,var',[6:.5:13 13.3]);clabel(cs);
grid
title('t\_ini');

subplot(223);
var=T(1,:);
plot(var,zC,'b-');
grid
axis([9.4 10 -300 0]);
subplot(224);
plot(var,zC,'b-');
var=T(ny/2,:);
hold on;
plot(var,zC,'r-');
i1=ny/2-3; i2=1+ny-i1;
var=T(i1,:);
plot(var,zC,'g-');
var=T(i2,:);
plot(var,zC,'c-');
hold off;
grid;

figure(2);clf;
subplot(211);
var=squeeze(uCg);
%pcolor(yc,zc,var'); colorbar;
cl=[-10:10]/100;
[cs,h]=contour(yC(1:ny),zC,var',cl);clabel(cs);
grid
title('u\_ini');

subplot(223);
plot(yC,S(:,1),'k-'); axis([yF(1) yF(end) 10 12.1]);
%var=squeeze(S); cl=[-10:10]/10;
%[cs,h]=contour(yC(1:ny),zC,var',cl);clabel(cs);
grid
title('s\_ini');

hSlp=[1:ny]*8/(ny-2); hSlp=hSlp-mean(hSlp);
hSlp=zF(end)+300*(1+tanh(hSlp)); hSlp(1)=0; hSlp(ny)=0;
subplot(224);
plot(yC,hSlp,'b-');
AA=axis;
axis([yF(1) yF(end) AA(3:4)]);
grid
title('topo\_slp');

%return

H=zF(end)*ones(ny,1);
H(1)=0; H(ny)=0;
fid=fopen('topog.bin','w','b'); fwrite(fid,H,'real*8'); fclose(fid);

fid=fopen('topo_slp.bin','w','b'); fwrite(fid,hSlp,'real*8'); fclose(fid);

fid=fopen('t_ini.bin','w','b'); fwrite(fid,T,'real*8'); fclose(fid);

fid=fopen('u_ini.bin','w','b'); fwrite(fid,uCg,'real*8'); fclose(fid);

fid=fopen('s_ini.bin','w','b'); fwrite(fid,S,'real*8'); fclose(fid);

return
