
kwr=0;
%kwr=1;

%gDir='../run_26l/';
%gDir='../cs_grid/';
gDir='../run/';
G=load_grid(gDir,0);

nx=G.dims(1); ny=G.dims(2); nc=ny;
xc=G.xC; yc=G.yC; xg=G.xG; yg=G.yG;

ccB=[0 0]; shift=-1; cbV=1; AxBx=[-180 180 -90 90]; kEnv=0;
y1d=[-89.5:1:90];
%figure(1);clf;
%imagesc(msk'); set(gca,'YDir','normal');
%colorbar

%-- make SST field (cos shape, no noise) from grid-output file YC:
yy=yc*pi/90; sst1=9+19*cos(yy);
%sst=273.15+sst1;
%fname='SST_cos0.bin';
yy=y1d*pi/90; sst1_1d=9+19*cos(yy); %- just for doing a plot

%-- follow APE profile:
yy=yc*pi/180;
var=sin(1.5*yy); var=1.-var.*var;
var(find(abs(yc) > 60.))=0.; sst2=27*var;
%sst=273.15+sst2;
%fname='SST_APE_1.bin';
%- just for doing a plot
yy=y1d*pi/180;
var=sin(1.5*yy); var=1.-var.*var;
var(find(abs(y1d) > 60.))=0.; sst2_1d=27*var;

%-- close to Symetric zonally-average current SST:
yyp=abs(yc/90); phi=yyp.^3.;
sst3=27.8*exp(-7*phi) - 1 ;
sst=273.15+sst3;
fname='SST_symEx3.bin';
%- just for doing a plot
yyp=abs(y1d/90); phi=yyp.^3.;
sst3_1d=27.8*exp(-7*phi) - 1 ;

if kwr > 0,
 fid=fopen(fname,'w','b'); fwrite(fid,sst,'real*8'); fclose(fid);
 fprintf(['write file: ',fname,'\n']);
end

figure(1);clf;
subplot(211)
var=sst;
grph_CS(var,xc,yc,xg,yg,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);
title('SST');

subplot(212)
plot(y1d,sst1_1d,'g-'); hold on;
plot(y1d,sst2_1d,'b-');
plot(y1d,sst3_1d,'r-');
hold off;
axis([-90 90 -5 29]); grid
legend('cos','ape','sym')
title('SST ^oC');
%return

%-- make Q-flux file (similar to the one used by Paul, hard coded
%                     in original version of mixed_layer.f90)
qflx_ampl=50. ;
qflx_width=90.;
yy=yc/qflx_width;
yy=yy.*yy;
qflx=qflx_ampl*(1-2*yy);
qflx=qflx.*exp(-yy);

figure(2);clf;
var=qflx;
grph_CS(var,xc,yc,xg,yg,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);
title('Q-flux [W/m^2]');

if kwr > 0,
 fname='Qflux_w90.bin';
 fid=fopen(fname,'w','b'); fwrite(fid,qflx,'real*8'); fclose(fid);
 fprintf(['write file: ',fname,'\n']);
end
%return

%-- make initial pot-temp field by adding noise to T(iter=0) output file:

rDir=gDir;
namf='T'; it=0;
tini=rdmds([rDir,namf],it);
nr=size(tini,3);

var=rand([nx,ny]); var=var-mean(var(:));
yy=yc*pi/90;
var=var.*(2+cos(yy))/3;
figure(3);clf;
%var=sst0;
grph_CS(var,xc,yc,xg,yg,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);

noise=1.e-3;
tini1=tini+noise*reshape(reshape(var,[nx*ny 1])*ones(1,nr),[nx ny nr]);
%size(tini1)

if kwr > 0,
 fname='ini_theta.bin';
 fid=fopen(fname,'w','b'); fwrite(fid,tini1,'real*8'); fclose(fid);
 fprintf(['write file: ',fname,'\n']);
end

%- spec-humid : put constant Rel-Humid in the lowest troposphere
relhum=0.8 ; pHum=800.e+2;
relhum=0.4 ; pHum=700.e+2;
khum=max(find(G.rC > pHum));

%- taken from AIM -> qsat in g/kg, pIn = normalised Pressure
P0=1.e+5; pIn=G.rC/P0;
qsat=calc_Qsat(1,pIn,tini);
qsat=reshape(qsat,[nx ny nr]);
qini=qsat*1.e-3*relhum;
qini(:,:,khum+1:end)=0;

figure(4);clf;
pax=G.rC/100; %- in mb
i1=1; j1=1;
var=squeeze(qini(i1,j1,:));
plot(var,pax,'k-'); hold on;
i1=nc/2; j1=nc/2;
var=squeeze(qini(i1,j1,:));
plot(var,pax,'r-');
i1=nc*2.5; j1=nc*0.5;
var=squeeze(qini(i1,j1,:));
plot(var,pax,'b-');
hold off
set(gca,'YDir','reverse');
grid
legend('mid','eq','pol');
title('Q-ini profile');

if kwr > 0,
 fname=['ini_specQ_',int2str(nr),'l.bin'];
 fid=fopen(fname,'w','b'); fwrite(fid,qini,'real*8'); fclose(fid);
 fprintf(['write file: ',fname,'\n']);
end

var=reshape(tini,[nx*ny nr]);
var=mean(var);
for n=1:ceil(nr/10)
  is=1+(n-1)*10; ie=min(nr,n*10);
  if n == 1, fprintf(' tRef='); else fprintf('      '); end
  fprintf(' %5.1f,',round(10*var(is:ie))/10);
  fprintf('\n')
end

return
