function [APEprocess2] = APEprocess2(dirsuffix)
% Complete processing of APE second moment decomposition
load mncdirlist
nsegments = size(mncdirlist,1);
eval(['cd ' 'mnc_out_',num2str(mncdirlist(1))])
griddata = rdmnc('grid.*');
 XC = griddata.XC;
 YC = griddata.YC;
 XG = griddata.XG; XG = XG(1:end-1,1:end-1);
 YG = griddata.YG; YG = YG(1:end-1,1:end-1);
 dxC = griddata.dxC;
 dyC = griddata.dyC;
 dxG = griddata.dxG;
 dyG = griddata.dyG;
 RAC = griddata.rA;
 HFacC = griddata.HFacC;
 HFacW = griddata.HFacW;
 HFacS = griddata.HFacS;
 ZC = griddata.RC;
 ZF = griddata.RF;
 drC = griddata.drC;
 drF = griddata.drF;
 undef=1.e15;
eval(['cd ../'])
%
nx = size(HFacC,1);
ny = size(HFacC,2);
%nr = size(HFacC,3);
nr = 17;
nPg = nx*ny;
nlats = 64;
% 
% load COS & SIN of rotation angle:
fid=fopen('/u/molod/proj_cs32_2uEvN.bin','r','b'); uvEN=fread(fid,nx*ny*2,'real*8'); fclose(fid);
uvEN=reshape(uvEN,[nPg 2]);
AngleCS=uvEN(:,1);
AngleSN=uvEN(:,2);
%
tempz=zeros(nlats,nr);
uzav=zeros(nlats,nr);
vzav=zeros(nlats,nr);
wzav=zeros(nlats,nr);
thetazav=zeros(nlats,nr);
saltzav=zeros(nlats,nr);
phizav=zeros(nlats,nr);
%
temp=zeros(nx,ny,nr);
uave=zeros(nx,ny,nr);
vave=zeros(nx,ny,nr);
wave=zeros(nx,ny,nr);
thetaave=zeros(nx,ny,nr);
saltave=zeros(nx,ny,nr);
phiave=zeros(nx,ny,nr);
hfactot=zeros(nx,ny,nr);
%
usqz=zeros(nlats,nr);
vsqz=zeros(nlats,nr);
thetasqz=zeros(nlats,nr);
phisqz=zeros(nlats,nr);
omegasqz=zeros(nlats,nr);
qsqz=zeros(nlats,nr);
uvz=zeros(nlats,nr);
uomegaz=zeros(nlats,nr);
vomegaz=zeros(nlats,nr);
vthetaz=zeros(nlats,nr);
omegathetaz=zeros(nlats,nr);
vqz=zeros(nlats,nr);
omegaqz=zeros(nlats,nr);
vphiz=zeros(nlats,nr);
%
UVtotz=zeros(nlats,nr);
U2totz=zeros(nlats,nr);
V2totz=zeros(nlats,nr);
wvelsqz=zeros(nlats,nr);
potempsqz=zeros(nlats,nr);
phihydsqz=zeros(nlats,nr);
saltsqz=zeros(nlats,nr);
wuEz=zeros(nlats,nr);
wvNz=zeros(nlats,nr);
vNthz=zeros(nlats,nr);
wvelthz=zeros(nlats,nr);
vNsltz=zeros(nlats,nr);
wvelsltz=zeros(nlats,nr);
vNphiz=zeros(nlats,nr);
hfacztot1=zeros(nlats,nr);
hfacztot2=zeros(nlats,nr);
%
% To do while leafing through all the 10-day segments:
for it = 1:nsegments
eval(['cd ' 'mnc_out_',num2str(mncdirlist(it))])
pwd
%
% 1) Stationary Mean: Products of zonal mean, time mean first moments
%    Read in the time mean of zonal mean first moments:
%    Compute running 3-year mean of zonal mean first moments:
%
fid=fopen('hfacztot1.interim','r','b');hfacz1=fread(fid,[nlats nr],'real*8');fclose(fid);hfacztot1 = hfacztot1 + hfacz1;
fid=fopen('thetazon.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);thetazav=thetazav+tempz.*hfacz1;
fid=fopen('uzon.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);uzav=uzav+tempz.*hfacz1;
fid=fopen('vzon.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vzav=vzav+tempz.*hfacz1;
fid=fopen('wzon.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);wzav=wzav+tempz.*hfacz1;
fid=fopen('saltzon.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);saltzav=saltzav+tempz.*hfacz1;
fid=fopen('phizon.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);phizav=phizav+tempz.*hfacz1;
%
% 2) Stationary Eddies: Total Stationary minus Stationary Mean
%                    Need zonal mean of products of 3-year mean first moments
%    Read in time averages of first moments
%    Compute running 3-year mean of first moments:
%
fid=fopen('hfactot1.interim','r','b');temp=fread(fid,[nx*ny nr],'real*8');fclose(fid);hfac=reshape(temp,[nx ny nr]);hfactot = hfactot + hfac;
fid=fopen('thetaave.interim','r','b');temp=fread(fid,[nx*ny nr],'real*8');fclose(fid);temp=reshape(temp,[nx ny nr]);thetaave=thetaave+temp.*hfac;
fid=fopen('uave.interim','r','b');temp=fread(fid,[nx*ny nr],'real*8');fclose(fid);temp=reshape(temp,[nx ny nr]);uave=uave+temp.*hfac;
fid=fopen('vave.interim','r','b');temp=fread(fid,[nx*ny nr],'real*8');fclose(fid);temp=reshape(temp,[nx ny nr]);vave=vave+temp.*hfac;
fid=fopen('wave.interim','r','b');temp=fread(fid,[nx*ny nr],'real*8');fclose(fid);temp=reshape(temp,[nx ny nr]);wave=wave+temp.*hfac;
fid=fopen('saltave.interim','r','b');temp=fread(fid,[nx*ny nr],'real*8');fclose(fid);temp=reshape(temp,[nx ny nr]);saltave=saltave+temp.*hfac;
fid=fopen('phiave.interim','r','b');temp=fread(fid,[nx*ny nr],'real*8');fclose(fid);temp=reshape(temp,[nx ny nr]);phiave=phiave+temp.*hfac;
%
% 3) Transient Mean: Total Mean minus Stationary Mean
%                    Need time mean of products of zonal mean first moments
%    Read in (time averaged) products of zonal means
%    Compute running 3-year mean of products of zonal means
%
fid=fopen('thetasqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);thetasqz=thetasqz+tempz.*hfacz1;
fid=fopen('usqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);usqz=usqz+tempz.*hfacz1;
fid=fopen('vsqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vsqz=vsqz+tempz.*hfacz1;
fid=fopen('phisqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);phisqz=phisqz+tempz.*hfacz1;
fid=fopen('omegasqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);omegasqz=omegasqz+tempz.*hfacz1;
fid=fopen('qsqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);qsqz=qsqz+tempz.*hfacz1;
fid=fopen('uvz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);uvz=uvz+tempz.*hfacz1;
fid=fopen('uomegaz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);uomegaz=uomegaz+tempz.*hfacz1;
fid=fopen('vomegaz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vomegaz=vomegaz+tempz.*hfacz1;
fid=fopen('vthetaz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vthetaz=vthetaz+tempz.*hfacz1;
fid=fopen('omegathetaz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);omegathetaz=omegathetaz+tempz.*hfacz1;
fid=fopen('vqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vqz=vqz+tempz.*hfacz1;
fid=fopen('omegaqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);omegaqz=omegaqz+tempz.*hfacz1;
fid=fopen('vphiz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vphiz=vphiz+tempz.*hfacz1;
%
% 4) Transient Eddy: Total Second Moment minus Total Mean minus Stationary Eddy
%                    Need time mean of zonal mean second moments
%    Read in (time averaged) zonal mean second moments
%    Compute running 3-year mean of zonal mean second moments
%
% Read in time averages of zonal mean second moments
fid=fopen('hfacztot2.interim','r','b');hfacz2=fread(fid,[nlats nr],'real*8');fclose(fid);hfacztot2 = hfacztot2 + hfacz2;
fid=fopen('potempsqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);potempsqz=potempsqz+tempz.*hfacz2;
fid=fopen('uvtotz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);UVtotz=UVtotz+tempz.*hfacz2;
fid=fopen('u2totz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);U2totz=U2totz+tempz.*hfacz2;
fid=fopen('v2totz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);V2totz=V2totz+tempz.*hfacz2;
fid=fopen('wvelsqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);wvelsqz=wvelsqz+tempz.*hfacz2;
fid=fopen('phihydsqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);phihydsqz=phihydsqz+tempz.*hfacz2;
fid=fopen('saltsqz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);saltsqz=saltsqz+tempz.*hfacz2;
fid=fopen('wuEz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);wuEz=wuEz+tempz.*hfacz2;
fid=fopen('wvNz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);wvNz=wvNz+tempz.*hfacz2;
fid=fopen('vNthz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vNthz=vNthz+tempz.*hfacz2;
fid=fopen('wvelthz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);wvelthz=wvelthz+tempz.*hfacz2;
fid=fopen('vNsltz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vNsltz=vNsltz+tempz.*hfacz2;
fid=fopen('wvelsltz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);wvelsltz=wvelsltz+tempz.*hfacz2;
fid=fopen('vNphiz.interim','r','b');tempz=fread(fid,[nlats nr],'real*8');fclose(fid);vNphiz=vNphiz+tempz.*hfacz2;
eval(['cd ../'])
% End segment loop
end
% Divide by the appropriate counter to compute 3-year time average
inds1=find(hfactot~=0);
thetaave(inds1)=thetaave(inds1) ./ hfactot(inds1);
uave(inds1)=uave(inds1) ./ hfactot(inds1);
vave(inds1)=vave(inds1) ./ hfactot(inds1);
wave(inds1)=wave(inds1) ./ hfactot(inds1);
saltave(inds1)=saltave(inds1) ./ hfactot(inds1);
phiave(inds1)=phiave(inds1) ./ hfactot(inds1);
%
inds2=find(hfacztot1~=0);
thetazav(inds2)=thetazav(inds2) ./ hfacztot1(inds2);
uzav(inds2)=uzav(inds2) ./ hfacztot1(inds2);
vzav(inds2)=vzav(inds2) ./ hfacztot1(inds2);
wzav(inds2)=wzav(inds2) ./ hfacztot1(inds2);
saltzav(inds2)=saltzav(inds2) ./ hfacztot1(inds2);
phizav(inds2)=phizav(inds2) ./ hfacztot1(inds2);
usqz(inds2)=usqz(inds2) ./ hfacztot2(inds2);
vsqz(inds2)=vsqz(inds2) ./ hfacztot2(inds2);
thetasqz(inds2)=thetasqz(inds2) ./ hfacztot2(inds2);
phisqz(inds2)=phisqz(inds2) ./ hfacztot2(inds2);
omegasqz(inds2)=omegasqz(inds2) ./ hfacztot2(inds2);
qsqz(inds2)=qsqz(inds2) ./ hfacztot2(inds2);
uvz(inds2)=uvz(inds2) ./ hfacztot2(inds2);
uomegaz(inds2)=uomegaz(inds2) ./ hfacztot2(inds2);
vomegaz(inds2)=vomegaz(inds2) ./ hfacztot2(inds2);
vthetaz(inds2)=vthetaz(inds2) ./ hfacztot2(inds2);
omegathetaz(inds2)=omegathetaz(inds2) ./ hfacztot2(inds2);
vqz(inds2)=vqz(inds2) ./ hfacztot2(inds2);
omegaqz(inds2)=omegaqz(inds2) ./ hfacztot2(inds2);
vphiz(inds2)=vphiz(inds2) ./ hfacztot2(inds2);
%
inds3=find(hfacztot2~=0);
UVtotz(inds3)=UVtotz(inds3) ./ hfacztot2(inds3);
U2totz(inds3)=U2totz(inds3) ./ hfacztot2(inds3);
V2totz(inds3)=V2totz(inds3) ./ hfacztot2(inds3);
wvelsqz(inds3)=wvelsqz(inds3) ./ hfacztot2(inds3);
potempsqz(inds3)=potempsqz(inds3) ./ hfacztot2(inds3);
phihydsqz(inds3)=phihydsqz(inds3) ./ hfacztot2(inds3);
saltsqz(inds3)=saltsqz(inds3) ./ hfacztot2(inds3);
wuEz(inds3)=wuEz(inds3) ./ hfacztot2(inds3);
wvNz(inds3)=wvNz(inds3) ./ hfacztot2(inds3);
vNthz(inds3)=vNthz(inds3) ./ hfacztot2(inds3);
wvelthz(inds3)=wvelthz(inds3) ./ hfacztot2(inds3);
vNsltz(inds3)=vNsltz(inds3) ./ hfacztot2(inds3);
wvelsltz(inds3)=wvelsltz(inds3) ./ hfacztot2(inds3);
vNphiz(inds3)=vNphiz(inds3) ./ hfacztot2(inds3);
%
% To do after leafing through all the 10-day segments:
% Stationary Mean:
% 1a)   Compute products of time mean zonal mean first moments
smusq = uzav.*uzav;
smvsq = vzav.*vzav;
smtsq = thetazav.*thetazav;
smwsq = wzav.*wzav;
smphisq = phizav.*phizav;
smqsq = saltzav.*saltzav;
smuv = uzav.*vzav;
smuw = uzav.*wzav;
smvw = vzav.*wzav;
smvt = vzav.*thetazav;
smwt = wzav.*thetazav;
smvq = vzav.*saltzav;
smwq = wzav.*saltzav;
smvphi = vzav.*phizav;
%
% Transient Mean: Time mean of products of zonal mean first moments minus Stationary Mean
tmusq = usqz - smusq;
tmvsq = vsqz - smvsq;
tmtsq = thetasqz - smtsq;
tmwsq = omegasqz - smwsq;
tmphisq = phisqz - smphisq;
tmqsq = qsqz - smqsq;
tmuv = uvz - smuv;
tmuw = uomegaz - smuw;
tmvw = vomegaz - smvw;
tmvt = vthetaz - smvt;
tmwt = omegathetaz - smwt;
tmvq = vqz - smvq;
tmwq = omegaqz - smwq;
tmvphi = vphiz - smvphi;
%
% Stationary Eddy:
% 2a)   Compute products of time mean first moments
% 2b)   Compute zonal mean of products of time mean first moments
% 2c)   Compute SE as zonal mean of products of time mean first moments minus stationary mean
hfac4calcz = hfactot ./ nsegments;
[uEave,vNave] = rotate_uv2uvEN(uave,vave,AngleCS,AngleSN);
temp=uEave.*uEave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);seusq=tempZ(:,:,1)-smusq;
temp=vNave.*vNave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sevsq=tempZ(:,:,1)-smvsq;
temp=thetaave.*thetaave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);setsq=tempZ(:,:,1)-smtsq;
temp=wave.*wave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sewsq=tempZ(:,:,1)-smwsq;
temp=phiave.*phiave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sephisq=tempZ(:,:,1)-smphisq;
temp=saltave.*saltave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);seqsq=tempZ(:,:,1)-smqsq;
temp=uEave.*vNave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);seuv=tempZ(:,:,1)-smuv;
temp=uEave.*wave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);seuw=tempZ(:,:,1)-smuw;
temp=vNave.*wave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sevw=tempZ(:,:,1)-smvw;
temp=vNave.*thetaave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sevt=tempZ(:,:,1)-smvt;
temp=wave.*thetaave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sewt=tempZ(:,:,1)-smwt;
temp=vNave.*saltave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sevq=tempZ(:,:,1)-smvq;
temp=wave.*saltave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sewq=tempZ(:,:,1)-smwq;
temp=vNave.*phiave;[tempZ,dum1,dum2]=calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);sevphi=tempZ(:,:,1)-smvphi;
%
% Transient Eddy: Time mean of zonal mean second moments minus Transient Mean minus Stationary Eddy
teusq = U2totz - usqz - seusq;
tevsq = V2totz - vsqz - sevsq;
tetsq = potempsqz - thetasqz - setsq;
tewsq = wvelsqz - omegasqz - sewsq;
tephisq = phihydsqz - phisqz - sephisq;
teqsq = saltsqz - qsqz - seqsq;
teuv = UVtotz - uvz - seuv;
teuw = wuEz - uomegaz - seuw;
tevw = wvNz - vomegaz - sevw;
tevt = vNthz - vthetaz - sevt;
tewt = wvelthz - omegathetaz - sewt;
tevq = vNsltz - vqz - sevq;
tewq = wvelsltz - omegaqz - sewq;
tevphi = vNphiz - vphiz - sevphi;
%
% Write out all the terms
eval(['cd mnc_out_processed'])
%
% Total:
fid = fopen('uvtotz.bin','w','b'); UVtotz(find(isnan(UVtotz)))=undef; fwrite(fid,UVtotz,'real*8'); fclose(fid);
fid = fopen('u2totz.bin','w','b'); U2totz(find(isnan(U2totz)))=undef; fwrite(fid,U2totz,'real*8'); fclose(fid);
fid = fopen('v2totz.bin','w','b'); V2totz(find(isnan(V2totz)))=undef; fwrite(fid,V2totz,'real*8'); fclose(fid);
fid = fopen('wvelsqz.bin','w','b'); wvelsqz(find(isnan(wvelsqz)))=undef; fwrite(fid,wvelsqz,'real*8'); fclose(fid);
fid = fopen('potempsqz.bin','w','b'); potempsqz(find(isnan(potempsqz)))=undef; fwrite(fid,potempsqz,'real*8'); fclose(fid);
fid = fopen('phihydsqz.bin','w','b'); phihydsqz(find(isnan(phihydsqz)))=undef; fwrite(fid,phihydsqz,'real*8'); fclose(fid);
fid = fopen('saltsqz.bin','w','b'); saltsqz(find(isnan(saltsqz)))=undef; fwrite(fid,saltsqz,'real*8'); fclose(fid);
fid = fopen('wuEz.bin','w','b'); wuEz(find(isnan(wuEz)))=undef; fwrite(fid,wuEz,'real*8'); fclose(fid);
fid = fopen('wvNz.bin','w','b'); wvNz(find(isnan(wvNz)))=undef; fwrite(fid,wvNz,'real*8'); fclose(fid);
fid = fopen('vNthz.bin','w','b'); vNthz(find(isnan(vNthz)))=undef; fwrite(fid,vNthz,'real*8'); fclose(fid);
fid = fopen('wvelthz.bin','w','b'); wvelthz(find(isnan(wvelthz)))=undef; fwrite(fid,wvelthz,'real*8'); fclose(fid);
fid = fopen('vNsltz.bin','w','b'); vNsltz(find(isnan(vNsltz)))=undef; fwrite(fid,vNsltz,'real*8'); fclose(fid);
fid = fopen('wvelsltz.bin','w','b'); wvelsltz(find(isnan(wvelsltz)))=undef; fwrite(fid,wvelsltz,'real*8'); fclose(fid);
fid = fopen('vNphiz.bin','w','b'); vNphiz(find(isnan(vNphiz)))=undef; fwrite(fid,vNphiz,'real*8'); fclose(fid);
%
% Some additional filtering to set some inconsistent values to undefined
%
indusq = find(smusq>U2totz); smusq(indusq)=undef;seusq(indusq)=undef;tmusq(indusq)=undef;teusq(indusq)=undef;
indvsq = find(smvsq>V2totz); smvsq(indvsq)=undef;sevsq(indvsq)=undef;tmvsq(indvsq)=undef;tevsq(indvsq)=undef;
indwsq = find(smwsq>wvelsqz); smwsq(indwsq)=undef;sewsq(indwsq)=undef;tmwsq(indwsq)=undef;tewsq(indwsq)=undef;
indtsq = find(smtsq>potempsqz); smtsq(indtsq)=undef;setsq(indtsq)=undef;tmtsq(indtsq)=undef;tetsq(indtsq)=undef;
indpsq = find(smphisq>phihydsqz); smphisq(indpsq)=undef;sephisq(indpsq)=undef;tmphisq(indpsq)=undef;tephisq(indpsq)=undef;
indqsq = find(smqsq>saltsqz); smqsq(indqsq)=undef;seqsq(indqsq)=undef;tmqsq(indqsq)=undef;teqsq(indqsq)=undef;
%
% Stationary Mean:
fid = fopen('smusq.bin','w','b'); smusq(find(isnan(smusq)))=undef; fwrite(fid,smusq,'real*8'); fclose(fid);
fid = fopen('smvsq.bin','w','b'); smvsq(find(isnan(smvsq)))=undef; fwrite(fid,smvsq,'real*8'); fclose(fid);
fid = fopen('smtsq.bin','w','b'); smtsq(find(isnan(smtsq)))=undef; fwrite(fid,smtsq,'real*8'); fclose(fid);
fid = fopen('smwsq.bin','w','b'); smwsq(find(isnan(smwsq)))=undef; fwrite(fid,smwsq,'real*8'); fclose(fid);
fid = fopen('smphisq.bin','w','b'); smphisq(find(isnan(smphisq)))=undef; fwrite(fid,smphisq,'real*8'); fclose(fid);
fid = fopen('smqsq.bin','w','b'); smqsq(find(isnan(smqsq)))=undef; fwrite(fid,smqsq,'real*8'); fclose(fid);
fid = fopen('smuv.bin','w','b'); smuv(find(isnan(smuv)))=undef; fwrite(fid,smuv,'real*8'); fclose(fid);
fid = fopen('smuw.bin','w','b'); smuw(find(isnan(smuw)))=undef; fwrite(fid,smuw,'real*8'); fclose(fid);
fid = fopen('smvw.bin','w','b'); smvw(find(isnan(smvw)))=undef; fwrite(fid,smvw,'real*8'); fclose(fid);
fid = fopen('smvt.bin','w','b'); smvt(find(isnan(smvt)))=undef; fwrite(fid,smvt,'real*8'); fclose(fid);
fid = fopen('smwt.bin','w','b'); smwt(find(isnan(smwt)))=undef; fwrite(fid,smwt,'real*8'); fclose(fid);
fid = fopen('smvq.bin','w','b'); smvq(find(isnan(smvq)))=undef; fwrite(fid,smvq,'real*8'); fclose(fid);
fid = fopen('smwq.bin','w','b'); smwq(find(isnan(smwq)))=undef; fwrite(fid,smwq,'real*8'); fclose(fid);
fid = fopen('smvphi.bin','w','b'); smvphi(find(isnan(smvphi)))=undef; fwrite(fid,smvphi,'real*8'); fclose(fid);
%
% Stationary Eddy:
fid = fopen('seusq.bin','w','b'); seusq(find(isnan(seusq)))=undef; fwrite(fid,seusq,'real*8'); fclose(fid);
fid = fopen('sevsq.bin','w','b'); sevsq(find(isnan(sevsq)))=undef; fwrite(fid,sevsq,'real*8'); fclose(fid);
fid = fopen('setsq.bin','w','b'); setsq(find(isnan(setsq)))=undef; fwrite(fid,setsq,'real*8'); fclose(fid);
fid = fopen('sewsq.bin','w','b'); sewsq(find(isnan(sewsq)))=undef; fwrite(fid,sewsq,'real*8'); fclose(fid);
fid = fopen('sephisq.bin','w','b'); sephisq(find(isnan(sephisq)))=undef; fwrite(fid,sephisq,'real*8'); fclose(fid);
fid = fopen('seqsq.bin','w','b'); seqsq(find(isnan(seqsq)))=undef; fwrite(fid,seqsq,'real*8'); fclose(fid);
fid = fopen('seuv.bin','w','b'); seuv(find(isnan(seuv)))=undef; fwrite(fid,seuv,'real*8'); fclose(fid);
fid = fopen('seuw.bin','w','b'); seuw(find(isnan(seuw)))=undef; fwrite(fid,seuw,'real*8'); fclose(fid);
fid = fopen('sevw.bin','w','b'); sevw(find(isnan(sevw)))=undef; fwrite(fid,sevw,'real*8'); fclose(fid);
fid = fopen('sevt.bin','w','b'); sevt(find(isnan(sevt)))=undef; fwrite(fid,sevt,'real*8'); fclose(fid);
fid = fopen('sewt.bin','w','b'); sewt(find(isnan(sewt)))=undef; fwrite(fid,sewt,'real*8'); fclose(fid);
fid = fopen('sevq.bin','w','b'); sevq(find(isnan(sevq)))=undef; fwrite(fid,sevq,'real*8'); fclose(fid);
fid = fopen('sewq.bin','w','b'); sewq(find(isnan(sewq)))=undef; fwrite(fid,sewq,'real*8'); fclose(fid);
fid = fopen('sevphi.bin','w','b'); sevphi(find(isnan(sevphi)))=undef; fwrite(fid,sevphi,'real*8'); fclose(fid);
%
% Transient Mean:
fid = fopen('tmusq.bin','w','b'); tmusq(find(isnan(tmusq)))=undef; fwrite(fid,tmusq,'real*8'); fclose(fid);
fid = fopen('tmvsq.bin','w','b'); tmvsq(find(isnan(tmvsq)))=undef; fwrite(fid,tmvsq,'real*8'); fclose(fid);
fid = fopen('tmtsq.bin','w','b'); tmtsq(find(isnan(tmtsq)))=undef; fwrite(fid,tmtsq,'real*8'); fclose(fid);
fid = fopen('tmwsq.bin','w','b'); tmwsq(find(isnan(tmwsq)))=undef; fwrite(fid,tmwsq,'real*8'); fclose(fid);
fid = fopen('tmphisq.bin','w','b'); tmphisq(find(isnan(tmphisq)))=undef; fwrite(fid,tmphisq,'real*8'); fclose(fid);
fid = fopen('tmqsq.bin','w','b'); tmqsq(find(isnan(tmqsq)))=undef; fwrite(fid,tmqsq,'real*8'); fclose(fid);
fid = fopen('tmuv.bin','w','b'); tmuv(find(isnan(tmuv)))=undef; fwrite(fid,tmuv,'real*8'); fclose(fid);
fid = fopen('tmuw.bin','w','b'); tmuw(find(isnan(tmuw)))=undef; fwrite(fid,tmuw,'real*8'); fclose(fid);
fid = fopen('tmvw.bin','w','b'); tmvw(find(isnan(tmvw)))=undef; fwrite(fid,tmvw,'real*8'); fclose(fid);
fid = fopen('tmvt.bin','w','b'); tmvt(find(isnan(tmvt)))=undef; fwrite(fid,tmvt,'real*8'); fclose(fid);
fid = fopen('tmwt.bin','w','b'); tmwt(find(isnan(tmwt)))=undef; fwrite(fid,tmwt,'real*8'); fclose(fid);
fid = fopen('tmvq.bin','w','b'); tmvq(find(isnan(tmvq)))=undef; fwrite(fid,tmvq,'real*8'); fclose(fid);
fid = fopen('tmwq.bin','w','b'); tmwq(find(isnan(tmwq)))=undef; fwrite(fid,tmwq,'real*8'); fclose(fid);
fid = fopen('tmvphi.bin','w','b'); tmvphi(find(isnan(tmvphi)))=undef; fwrite(fid,tmvphi,'real*8'); fclose(fid);
%
% Transient Eddy:
fid = fopen('teusq.bin','w','b'); teusq(find(isnan(teusq)))=undef; fwrite(fid,teusq,'real*8'); fclose(fid);
fid = fopen('tevsq.bin','w','b'); tevsq(find(isnan(tevsq)))=undef; fwrite(fid,tevsq,'real*8'); fclose(fid);
fid = fopen('tetsq.bin','w','b'); tetsq(find(isnan(tetsq)))=undef; fwrite(fid,tetsq,'real*8'); fclose(fid);
fid = fopen('tewsq.bin','w','b'); tewsq(find(isnan(tewsq)))=undef; fwrite(fid,tewsq,'real*8'); fclose(fid);
fid = fopen('tephisq.bin','w','b'); tephisq(find(isnan(tephisq)))=undef; fwrite(fid,tephisq,'real*8'); fclose(fid);
fid = fopen('teqsq.bin','w','b'); teqsq(find(isnan(teqsq)))=undef; fwrite(fid,teqsq,'real*8'); fclose(fid);
fid = fopen('teuv.bin','w','b'); teuv(find(isnan(teuv)))=undef; fwrite(fid,teuv,'real*8'); fclose(fid);
fid = fopen('teuw.bin','w','b'); teuw(find(isnan(teuw)))=undef; fwrite(fid,teuw,'real*8'); fclose(fid);
fid = fopen('tevw.bin','w','b'); tevw(find(isnan(tevw)))=undef; fwrite(fid,tevw,'real*8'); fclose(fid);
fid = fopen('tevt.bin','w','b'); tevt(find(isnan(tevt)))=undef; fwrite(fid,tevt,'real*8'); fclose(fid);
fid = fopen('tewt.bin','w','b'); tewt(find(isnan(tewt)))=undef; fwrite(fid,tewt,'real*8'); fclose(fid);
fid = fopen('tevq.bin','w','b'); tevq(find(isnan(tevq)))=undef; fwrite(fid,tevq,'real*8'); fclose(fid);
fid = fopen('tewq.bin','w','b'); tewq(find(isnan(tewq)))=undef; fwrite(fid,tewq,'real*8'); fclose(fid);
fid = fopen('tevphi.bin','w','b'); tevphi(find(isnan(tevphi)))=undef; fwrite(fid,tevphi,'real*8'); fclose(fid);
%
