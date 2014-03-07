
kwr=1; kprt=0;
nx=80; ny=42; nr=3; nt=1;

xc=[1:nx]; xc=xc-mean(xc);
yc=[1:ny]-.5;

%------------------------------------------------------

windx=10.;
H0=-100.;

namf='channel.bin';
depth=H0*ones(nx,ny); depth(:,1)=0.;
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,depth,'real*8'); fclose(fid);
end

namf='bathy_3c.bin';
msk=abs(xc)'*ones(1,ny)+ones(nx,1)*yc;
depth=H0*ones(nx,ny); depth(:,1)=0.;
depth(find(msk < 24))=0.;
y2d=ones(nx,1)*yc;
depth(find(y2d > ny/2))=H0;
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,depth,'real*8'); fclose(fid);
end

namf='windx.bin';
wnd=windx*ones(nx,ny,nt);
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,wnd,'real*8'); fclose(fid);
end

%- file name convention: "const_{xx}.bin" <-> uniform value = xx (in percent)
namf='const_00.bin';
fld=0*ones(nx,ny,nt);
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,fld,'real*8'); fclose(fid);
end

namf='const100.bin'; w0=1.;
var=w0*ones(nx,ny);
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
end

namf='const+20.bin'; w0=0.2;
var=w0*ones(nx,ny);
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
end

%namf='const+40.bin';
%u0=0.4;
%var=u0*ones(nx,ny);
%if kwr > 0,
% fprintf('write to file: %s\n',namf);
% fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
%end

%namf='const-10.bin';
%v0=-0.1;
%var=v0*ones(nx,ny);
%if kwr > 0,
% fprintf('write to file: %s\n',namf);
% fid=fopen(namf,'w','b'); fwrite(fid,var,'real*8'); fclose(fid);
%end
%------------------------------------------------------

namf='ice0_area.bin'; iceC0=1.;
iceConc=iceC0*ones(nx,ny); iceConc(:,1)=0;
iceConc(:,2)=0.00*iceC0;
iceConc(:,3)=0.10*iceC0;
iceConc(:,end)  =0.00*iceC0;
iceConc(:,end-1)=0.01*iceC0;
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,iceConc,'real*8'); fclose(fid);
end

namf='ice0_heff.bin'; iceH0=0.2;
iceVol=iceConc*iceH0;
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,iceVol,'real*8'); fclose(fid);
end

%------------------------------------------------------

dsw0=100;
namf=['dsw_',int2str(dsw0),'.bin'];
fld=dsw0*ones(nx,ny,nt);
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,fld,'real*8'); fclose(fid);
end

dlw0=250;
namf=['dlw_',int2str(dlw0),'.bin'];
fld=dlw0*ones(nx,ny,nt);
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,fld,'real*8'); fclose(fid);
end

cel2K=273.15; dtx=4; %- dtx = amplitude of air temp variations in X-dir
ta_x=cel2K + dtx*sin(pi*(1+2*xc'/nx));
ta=repmat(ta_x,[1 ny nt]);
namf=['tair_',int2str(dtx),'x.bin'];
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,ta,'real*8'); fclose(fid);
end;

cvapor_fac     =   640380.000  ;
cvapor_exp     =     5107.400  ;
atmrho         =        1.200  ;
rh=70; %- specific humid <--> 70.% relative humid
tmpbulk = cvapor_fac*exp(-cvapor_exp./ta_x);
qa_x = (rh/100.)*tmpbulk/atmrho ;
qa=repmat(qa_x,[1 ny nt]);
namf=['qa',int2str(rh),'_',int2str(dtx),'x.bin'];
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,qa,'real*8'); fclose(fid);
end;

%- salinity
sCst=30;
so=sCst*ones(nx,ny,nt);
namf='socn.bin';
%if kwr > 0,
% fprintf('write to file: %s\n',namf);
% fid=fopen(namf,'w','b'); fwrite(fid,so,'real*8'); fclose(fid);
%end;

muTf = 5.4e-2;
tfreeze=-muTf*sCst;
fprintf('T-freeze = %10.6f\n',tfreeze);
%- parabolic profile in Y, max @ j=4, min @ j=ny, amplitude=1.K
to_y=(yc-3.5)/(ny-4);
to_y=tfreeze+0.5-to_y.*to_y;
  mnV=min(to_y); MxV=max(to_y); Avr=mean(to_y(2:end));
  fprintf(' SST* av,mn,Mx: %9.6f , %9.6f , %9.6f , %9.6f\n',Avr,mnV,MxV,MxV-mnV);
to=repmat(to_y,[nx 1 nt]);
namf='tocn.bin';
if kwr > 0,
 fprintf('write to file: %s\n',namf);
 fid=fopen(namf,'w','b'); fwrite(fid,to,'real*8'); fclose(fid);
end;

%-- make some plots to check: ----------------

hScal=[-1.1 0.1]*abs(H0);
figure(1); clf;
subplot(211);
var=depth;
imagesc(xc,yc,var'); set(gca,'YDir','normal');
%caxis(hScal);
%change_colmap(-1);
colorbar;
title('Depth [m]');

subplot(413);
var=depth;
j1=2;
j2=ny/2;
j3=j2+1;
plot(xc,var(:,j1),'k-')
hold on; j=j+1;
plot(xc,var(:,j2),'ro-')
plot(xc,var(:,j3),'b-')
hold off;
axis([-nx/2 nx/2 hScal]);
grid
legend(int2str(j1),int2str(j2),int2str(j3));
title('Depth @ j= cst');

subplot(414);
i=nx/2;
plot(yc,var(i,:),'k-')
axis([0 ny H0*1.1 -H0*.1]);
grid
title(['Depth @ i=',int2str(i)]);

%--
dewPt=(qa_x*atmrho)/cvapor_fac;
dewPt=-cvapor_exp./log(dewPt);

figure(2);clf;
subplot(311)
P(1)=plot(xc,ta_x-cel2K,'r-'); hold on;
P(2)=plot(xc,dewPt-cel2K,'b-');
P(3)=plot(xc,tfreeze*ones(nx,1),'k-');
set(P,'LineWidth',1);
hold off; AA=axis;
axis([-nx/2 nx/2 AA(3:4)]);
legend('ta','dew');
grid
xlabel('X')
title(['Air Temp (^oC): del-Temp-X = ',int2str(dtx),' , RH= ',int2str(rh)]);
subplot(312)
P(1)=plot(yc,to_y,'b-'); hold on;
P(2)=plot(yc,tfreeze*ones(ny,1),'k-');
set(P,'LineWidth',1);
hold off; AA=axis;
L=line([1 1],AA(3:4)); set(L,'LineWidth',2.,'Color',[0 0 0]);
axis([0 ny AA(3:4)]);
grid
xlabel('Y')
title('Ocean Temp ^oC');

subplot(313)
var=iceConc(1,:);
P(1)=semilogy(yc,var,'b-x'); hold on;
%plot(yc,var,'b-x'); hold on;
var=iceVol(1,:);
P(2)=semilogy(yc,var,'r-x');
%plot(yc,var,'r-x');
set(P,'LineWidth',1);
hold off; AA=axis;
L=line([1 1],AA(3:4)); set(L,'LineWidth',2.,'Color',[0 0 0]);
axis([0 ny [0 2]*iceC0]);
grid
xlabel('Y')
legend('iceC','hEff','Location','South');
title('Initial ice in Channel : y-section');
%-----
if kprt == 1, f=2;
 namfig=sprintf('forcing_%2.2i',f);
 fprintf([' print fig= %2i to file: ',namfig,' '],f);
 set(f,'PaperOrientation','portrait')
%set(f,'PaperPosition',[0.25 1.5 6. 8.]);
 set(f,'PaperPosition',[0.25 1.5 5.25 7.]);
 print(f,'-depsc2',namfig); fprintf('\n');
end
%-----

figure(3);clf;
subplot(311)
var=iceConc; ccB=[-1 12]/10;
imagesc(xc,yc,var'); set(gca,'YDir','normal');
caxis(ccB);
%change_colmap(-1);
colorbar;
title('Ice Concentration in Channel');

subplot(312)
var=iceVol; ccB=[-1 12]/50;
imagesc(xc,yc,var'); set(gca,'YDir','normal');
caxis(ccB);
%change_colmap(-1);
colorbar;
title('Effective ice thickness in Channel');

subplot(313)
var=iceConc(1,:);
%plot(yc,var,'b-x'); hold on;
semilogy(yc,var,'b-x'); hold on;
var=iceVol(1,:);
%plot(yc,var,'r-x'); hold off;
semilogy(yc,var,'r-x'); hold on;
AA=axis; axis([0 ny [0 2]*iceC0]);
grid
legend('iceC','hEff','Location','South');
title('Initial ice in Channel : y-section');
%-----

return
