nx=1 ; ny=56 ; 
dx=3 ; dy=3 ; yyM=84 ;
yc=-yyM+dy/2:dy:yyM; xc=dx/2:dx:nx*dx;
kg=3;

%-- Bathy :
if kg == 1

hh=ones(nx,ny);
hh(:,1)=zeros(nx,1);
hh(:,ny)=zeros(nx,1);
hh=-6000*hh;

imagesc(xc,yc,hh');set(gca,'YDir','normal');
scalHV_colbar([1. 1. 0.5 0.7],1);
grid;

file_name='topo_sl.bin' ;
fid=fopen(file_name,'w','b');
fwrite(fid,hh,'real*8'); fclose(fid);

end
%---

%-- SST :
if kg == 2

yyp=abs(yc/90);
phi=yyp.^3.;
phi=-9*phi;
ts1=exp(phi); ts1=28*ts1 - 1 ;
subplot(211); 
%plot(y2c,sst2zav,'k-',y2c,sst2sym,'b-',yc,ts1,'r-');
plot(yc,ts1,'k-');
AA=axis ; axis([-81 81 AA(3:4)]);
grid;

ts=zeros(nx,ny); for j=1:ny, ts(:,j)=ts1(j) ; end
file_name='sst_sl.bin' ;
fid=fopen(file_name,'w','b');
fwrite(fid,ts,'real*8'); fclose(fid);

end
%---

%-- Tau-X :
if kg == 3

%- exponential damping if y > 60 (or y > 55) :
yyp=abs(yc/60);
yyp=max(yyp,1);
yyp=yyp.^5;
yye=1-yyp; yye=exp(yye);

rad=pi/180.;
phi=4.0*rad*yc;
tx0=cos(phi); tx0=-.06*tx0 ;
phi=7.2*rad*yc;
tx1=cos(phi); tx1=.043*tx1 ;
tx3=tx0+tx1 ; fc=90;
tx3=tx3.*yye;
%- tx4 = tx3 but only positive part in high lat.
tx4=tx3; 
nn=min( find(yc >= -60) ) ; tx4(1:nn)=max(tx4(1:nn),0) ;
nn=max( find(yc <= 60) ) ; tx4(nn:ny)=max(tx4(nn:ny),0) ;
subplot(212); 
plot(y2c,tx2zav,'k-',y2c,tx2sym,'b-',yc,tx4,'g-',yc,tx3,'r-');
%plot(yc,tx3,'k-',yc,tx4,'r-');
AA=axis ; axis([-81 81 AA(3:4)]);
grid;

tx=zeros(nx,ny); for j=1:ny, tx(:,j)=tx3(j) ; end
file_name='taux_sl.bin' ;
%tx=zeros(nx,ny); for j=1:ny, tx(:,j)=tx4(j) ; end
%file_name='taux2sl.bin' ;
fid=fopen(file_name,'w','b');
fwrite(fid,tx,'real*8'); fclose(fid);

end
%---
