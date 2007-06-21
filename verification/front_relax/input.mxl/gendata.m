
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
dx=1.e+5; nx=32; 
xF=[0:nx]-nx/2; xF=xF*dx; xC=(xF(1:nx)+xF(2:nx+1))/2;

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

Ly=24.*dx; yc=0; Ho=2205; 
T=zeros(nx,nz); 
km=6; kmx=6;
for k=km:nz,
 T(:,k)=10 +dTdz*zC(k) ...
           +dTdy*Ly*(tanh((xC-yc-200.e3)/200.e3)-tanh((xC-yc+200.e3)/200.e3))/2 ...
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
dbdx=zeros(nx+1,nz);
dbdx(2:nx,:)=gravity*alphaT*(T([2:nx],:)-T([1:nx-1],:))/dx;
vg=zeros(nx+1,nz+1);
for k=nz:-1:1;
 vg(:,k) = vg(:,k+1) - dbdx(:,k)*dz(k)/fo ;
end
vCg=vg([1:nx],[1:nz])+vg([2:nx+1],[2:nz+1])  ...
   +vg([2:nx+1],[1:nz])+vg([1:nx],[2:nz+1]);
vCg=vCg/4;
%size(vCg)

% Salt (passive tracer)
S=zeros(nx,nz);
for k=1:nz,
%S(:,k)=ones(nx,1)+zC(k)/Ho;  % Linear with z
%S(:,k)=exp(2*zC(k)/Ho);      % Exponential with z
 S(:,k)=exp(-(2*xC/Ly).^2);   % Exponential with y
end
S=10+2*S;

%- plot to check:

figure(1);clf;
subplot(211);
var=squeeze(T); var(find(var==0))=NaN;
%pcolor(yc,zc,var'); colorbar;
[cs,h]=contour(xC,zC,var',[6:.5:13 13.3]);clabel(cs);
grid

subplot(223);
var=T(1,:);
plot(var,zC,'b-');
grid
axis([9.4 10 -300 0]);
subplot(224);
plot(var,zC,'b-');
var=T(nx/2,:);
hold on;
plot(var,zC,'r-');
i1=nx/2-3; i2=1+nx-i1;
var=T(i1,:);
plot(var,zC,'g-');
var=T(i2,:);
plot(var,zC,'c-');
hold off;
grid;

figure(2);clf;
subplot(211);
var=squeeze(vCg);
%pcolor(yc,zc,var'); colorbar;
cl=[-10:10]/100;
[cs,h]=contour(xC(1:nx),zC,var',cl);clabel(cs);
grid
subplot(313);
plot(xC,S(:,1),'k-'); axis([xF(1) xF(end) 10 12.1]);
%var=squeeze(S);
%cl=[-10:10]/10;
%[cs,h]=contour(xC(1:nx),zC,var',cl);clabel(cs);
grid

return

H=zF(end)*ones(nx,1);
H(1)=0; H(nx)=0;
fid=fopen('topog.bin','w','b'); fwrite(fid,H,'real*8'); fclose(fid);

fid=fopen('t_ini.bin','w','b'); fwrite(fid,T,'real*8'); fclose(fid);

fid=fopen('u_ini.bin','w','b'); fwrite(fid,vCg,'real*8'); fclose(fid);

fid=fopen('s_ini.bin','w','b'); fwrite(fid,S,'real*8'); fclose(fid);

return
