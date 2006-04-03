function [APEprocess1] = APEprocess1(dirsuffix)
datadir=['mnc_out_',dirsuffix];
eval(['cd ' datadir])
% Load grid fields
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

% Load ML fields
mldata = rdmnc('MLfields.*');
ntimes = size(mldata.T,1);
temp = mldata.THETA(:,:,:,1);
nx = size(temp,1);
ny = size(temp,2);
nPg = nx*ny;
nr = size(temp,3);
% Reference geopotential profile for 17 levels:
phiref=9.8.*[ -0.739878953  646.302002  1338.38696  2894.67627  4099.63135  5484.93359 7116.62549  9115.08008  10321.4688  11741.3574  13494.4102  15862.4404 17833.5605  19667.8887  22136.1934  23854.2266  26366.9375];
% load COS & SIN of rotation angle:
fid=fopen('/u/molod/proj_cs32_2uEvN.bin','r','b'); uvEN=fread(fid,nx*ny*2,'real*8'); fclose(fid);
uvEN=reshape(uvEN,[nPg 2]);
AngleCS=uvEN(:,1);
AngleSN=uvEN(:,2);
% Some initialization of arrays
uav=zeros(nx,ny,nr);
vav=zeros(nx,ny,nr);
wav=zeros(nx,ny,nr);
thetaav=zeros(nx,ny,nr);
saltav=zeros(nx,ny,nr);
phiav=zeros(nx,ny,nr);
hfactot=zeros(nx,ny,nr);
uzav=zeros(64,nr);
vzav=zeros(64,nr);
wzav=zeros(64,nr);
thetazav=zeros(64,nr);
saltzav=zeros(64,nr);
phizav=zeros(64,nr);
usqz=zeros(64,nr);
vsqz=zeros(64,nr);
thetasqz=zeros(64,nr);
phisqz=zeros(64,nr);
omegasqz=zeros(64,nr);
qsqz=zeros(64,nr);
uvz=zeros(64,nr);
uomegaz=zeros(64,nr);
vomegaz=zeros(64,nr);
vthetaz=zeros(64,nr);
omegathetaz=zeros(64,nr);
vqz=zeros(64,nr);
omegaqz=zeros(64,nr);
vphiz=zeros(64,nr);
hfacztot=zeros(64,nr);
for it=1:ntimes
% 1) compute zonal mean of scalars (theta,q,phi,omega) using calc_ZonAv_CS:
temp = mldata.THETA(:,:,:,it);
hfac4calcz = ones(nx,ny,nr);
hfac4calcz(find(temp(:,:,:)==0))=0.;
hfactot = hfactot + hfac4calcz;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);
thetaz=tempZ(:,:,1);
hfacz = ones(64,nr);
hfacz(:,1) = dum1(:,1) ./ dum1(:,17);
hfacztot = hfacztot + hfacz;
temp = mldata.SALT(:,:,:,it);
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);
saltz=tempZ(:,:,1);
temp = mldata.PHIHYD(:,:,:,it);
% Add reference geopotential to anomaly
for ilev = 1:nr
ind2d = find(hfac4calcz(:,:,ilev)~=0);
atemp = squeeze(temp(:,:,ilev));
atemp(ind2d)=atemp(ind2d) + phiref(ilev);
temp(:,:,ilev) = atemp;
end
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);
phiz=tempZ(:,:,1);
temp = mldata.WVEL(:,:,:,it);
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);
wz=tempZ(:,:,1);
% 2) rotate u and v then compute zonal mean using calc_ZonAv_CS:
u = mldata.UVEL(:,:,:,it);
u = u(1:nx,1:ny,:);
v = mldata.VVEL(:,:,:,it);
v = v(1:nx,1:ny,:);
[uE,vN] = rotate_uv2uvEN(u,v,AngleCS,AngleSN);
temp = uE;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);
uz=tempZ(:,:,1);
temp = vN;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',hfac4calcz);
vz=tempZ(:,:,1);
% 3)compute the products and accumulate into a time average:
ind00=find(hfacz~=0);
usqz(ind00)=usqz(ind00) + uz(ind00).*uz(ind00).*hfacz(ind00);
vsqz(ind00)=vsqz(ind00) + vz(ind00).*vz(ind00).*hfacz(ind00);
thetasqz(ind00)=thetasqz(ind00) + thetaz(ind00).*thetaz(ind00).*hfacz(ind00);
phisqz(ind00)=phisqz(ind00) + phiz(ind00).*phiz(ind00).*hfacz(ind00);
omegasqz(ind00)=omegasqz(ind00) + wz(ind00).*wz(ind00).*hfacz(ind00);
qsqz(ind00)=qsqz(ind00) + saltz(ind00).*saltz(ind00).*hfacz(ind00);
uvz(ind00)=uvz(ind00) + uz(ind00).*vz(ind00).*hfacz(ind00);
uomegaz(ind00)=uomegaz(ind00) + uz(ind00).*wz(ind00).*hfacz(ind00);
vomegaz(ind00)=vomegaz(ind00) + vz(ind00).*wz(ind00).*hfacz(ind00);
vthetaz(ind00)=vthetaz(ind00) + vz(ind00).*thetaz(ind00).*hfacz(ind00);
omegathetaz(ind00)=omegathetaz(ind00) + wz(ind00).*thetaz(ind00).*hfacz(ind00);
vqz(ind00)=vqz(ind00) + vz(ind00).*saltz(ind00).*hfacz(ind00);
omegaqz(ind00)=omegaqz(ind00) + wz(ind00).*saltz(ind00).*hfacz(ind00);
vphiz(ind00)=vphiz(ind00) + vz(ind00).*phiz(ind00).*hfacz(ind00);
%
% Compute time average of zonal means:
uzav(ind00)=uzav(ind00) + uz(ind00).*hfacz(ind00);
vzav(ind00)=vzav(ind00) + vz(ind00).*hfacz(ind00);
wzav(ind00)=wzav(ind00) + wz(ind00).*hfacz(ind00);
thetazav(ind00)=thetazav(ind00) + thetaz(ind00).*hfacz(ind00);
saltzav(ind00)=saltzav(ind00) + saltz(ind00).*hfacz(ind00);
phizav(ind00)=phizav(ind00) + phiz(ind00).*hfacz(ind00);
%
% 4)Compute time average of u, v, Theta, q, Omega, Phi (full fields)
ind0=find(hfac4calcz~=0);
uav(ind0)=uav(ind0) + u(ind0);
vav(ind0)=vav(ind0) + v(ind0);
ttemp=squeeze(mldata.WVEL(:,:,:,it));
wav(ind0)=wav(ind0) + ttemp(ind0);
ttemp=squeeze(mldata.THETA(:,:,:,it));
thetaav(ind0)=thetaav(ind0) + ttemp(ind0);
ttemp=squeeze(mldata.SALT(:,:,:,it));
saltav(ind0)=saltav(ind0) + ttemp(ind0);
ttemp=squeeze(mldata.PHIHYD(:,:,:,it));
phiav(ind0)=phiav(ind0) + ttemp(ind0);
%
end
% Divide the sums by the appropriate counter to get time average
ind1=find(hfacztot~=0);
usqz(ind1)=usqz(ind1) ./ hfacztot(ind1);
vsqz(ind1)=vsqz(ind1) ./ hfacztot(ind1);
thetasqz(ind1)=thetasqz(ind1) ./ hfacztot(ind1);
phisqz(ind1)=phisqz(ind1) ./ hfacztot(ind1);
omegasqz(ind1)=omegasqz(ind1) ./ hfacztot(ind1);
qsqz(ind1)=qsqz(ind1) ./ hfacztot(ind1);
uvz(ind1)=uvz(ind1) ./ hfacztot(ind1);
uomegaz(ind1)=uomegaz(ind1) ./ hfacztot(ind1);
vomegaz(ind1)=vomegaz(ind1) ./ hfacztot(ind1);
vthetaz(ind1)=vthetaz(ind1) ./ hfacztot(ind1);
omegathetaz(ind1)=omegathetaz(ind1) ./ hfacztot(ind1);
vqz(ind1)=vqz(ind1) ./ hfacztot(ind1);
omegaqz(ind1)=omegaqz(ind1) ./ hfacztot(ind1);
vphiz(ind1)=vphiz(ind1) ./ hfacztot(ind1);
uzav(ind1)=uzav(ind1) ./ hfacztot(ind1);
vzav(ind1)=vzav(ind1) ./ hfacztot(ind1);
wzav(ind1)=wzav(ind1) ./ hfacztot(ind1);
thetazav(ind1)=thetazav(ind1) ./ hfacztot(ind1);
saltzav(ind1)=saltzav(ind1) ./ hfacztot(ind1);
phizav(ind1)=phizav(ind1) ./ hfacztot(ind1);
%
ind2=find(hfactot~=0);
uav(ind2)=uav(ind2) ./ hfactot(ind2);
vav(ind2)=vav(ind2) ./ hfactot(ind2);
wav(ind2)=wav(ind2) ./ hfactot(ind2);
thetaav(ind2)=thetaav(ind2) ./ hfactot(ind2);
saltav(ind2)=saltav(ind2) ./ hfactot(ind2);
phiav(ind2)=phiav(ind2) ./ hfactot(ind2);
%
% Add reference geopotential to anomaly
for ilev = 1:nr
atemp = squeeze(phiav(:,:,ilev));
ind2d2 = find(hfactot(:,:,ilev)~=0);
atemp(ind2d2)=atemp(ind2d2) + phiref(ilev);
phiav(:,:,ilev) = atemp;
end
%
% Release memory of all time-dependant ML quantities
clear mldata
%
% Some steps that have to wait until all 10-day periods are processed
% 5) Compute squared time averages:
% 6) Compute zonal mean of squared time averaged:
%
% Write out (time averaged) zonal means of first moments
%
fid = fopen('uzon.interim','w','b'); fwrite(fid,uzav,'real*8'); fclose(fid);
fid = fopen('vzon.interim','w','b'); fwrite(fid,vzav,'real*8'); fclose(fid);
fid = fopen('wzon.interim','w','b'); fwrite(fid,wzav,'real*8'); fclose(fid);
fid = fopen('thetazon.interim','w','b'); fwrite(fid,thetazav,'real*8'); fclose(fid);
fid = fopen('saltzon.interim','w','b'); fwrite(fid,saltzav,'real*8'); fclose(fid);
fid = fopen('phizon.interim','w','b'); fwrite(fid,phizav,'real*8'); fclose(fid);
weightoutz = hfacztot ./ ntimes;
fid = fopen('hfacztot1.interim','w','b'); fwrite(fid,weightoutz,'real*8'); fclose(fid);
%
% Write out (time averaged) products of zonal means
%
fid = fopen('usqz.interim','w','b'); fwrite(fid,usqz,'real*8'); fclose(fid);
fid = fopen('vsqz.interim','w','b'); fwrite(fid,vsqz,'real*8'); fclose(fid);
fid = fopen('thetasqz.interim','w','b'); fwrite(fid,thetasqz,'real*8'); fclose(fid);
fid = fopen('phisqz.interim','w','b'); fwrite(fid,phisqz,'real*8'); fclose(fid);
fid = fopen('omegasqz.interim','w','b'); fwrite(fid,omegasqz,'real*8'); fclose(fid);
fid = fopen('qsqz.interim','w','b'); fwrite(fid,qsqz,'real*8'); fclose(fid);
fid = fopen('uvz.interim','w','b'); fwrite(fid,uvz,'real*8'); fclose(fid);
fid = fopen('uomegaz.interim','w','b'); fwrite(fid,uomegaz,'real*8'); fclose(fid);
fid = fopen('vomegaz.interim','w','b'); fwrite(fid,vomegaz,'real*8'); fclose(fid);
fid = fopen('vthetaz.interim','w','b'); fwrite(fid,vthetaz,'real*8'); fclose(fid);
fid = fopen('omegathetaz.interim','w','b'); fwrite(fid,omegathetaz,'real*8'); fclose(fid);
fid = fopen('vqz.interim','w','b'); fwrite(fid,vqz,'real*8'); fclose(fid);
fid = fopen('omegaqz.interim','w','b'); fwrite(fid,omegaqz,'real*8'); fclose(fid);
fid = fopen('vphiz.interim','w','b'); fwrite(fid,vphiz,'real*8'); fclose(fid);
%
% Write out time averages of first moments
%
fid = fopen('uave.interim','w','b'); fwrite(fid,uav,'real*8'); fclose(fid);
fid = fopen('vave.interim','w','b'); fwrite(fid,vav,'real*8'); fclose(fid);
fid = fopen('wave.interim','w','b'); fwrite(fid,wav,'real*8'); fclose(fid);
fid = fopen('thetaave.interim','w','b'); fwrite(fid,thetaav,'real*8'); fclose(fid);
fid = fopen('saltave.interim','w','b'); fwrite(fid,saltav,'real*8'); fclose(fid);
fid = fopen('phiave.interim','w','b'); fwrite(fid,phiav,'real*8'); fclose(fid);
weightout = hfactot ./ ntimes;
fid = fopen('hfactot1.interim','w','b'); fwrite(fid,weightout,'real*8'); fclose(fid);
%
%Load MF fields
mfdata = rdmnc('MFfields.*');
ntimes = size(mfdata.T,1);
% allocate some space for time averages
uvelsq = zeros(nx,ny,nr);
vvelsq = zeros(nx,ny,nr);
thetasq = zeros(nx,ny,nr);
wvelsq = zeros(nx,ny,nr);
phihydsq = zeros(nx,ny,nr);
saltsq = zeros(nx,ny,nr);
uvvelc = zeros(nx,ny,nr);
wuvel = zeros(nx,ny,nr);
wvvel = zeros(nx,ny,nr);
uvelth = zeros(nx,ny,nr);
vvelth = zeros(nx,ny,nr);
wvelth = zeros(nx,ny,nr);
uvelslt = zeros(nx,ny,nr);
vvelslt = zeros(nx,ny,nr);
wvelslt = zeros(nx,ny,nr);
uvelphi = zeros(nx,ny,nr);
vvelphi = zeros(nx,ny,nr);
hfacztot=zeros(64,nr);
%
%1)Compute time averages of quantities - cannot use mean function because of undefs
%      also - variances can't be negative (u**2,v**2, w**2, q**2, t**2 and phi**2)
for it=1:ntimes
temp00 = mfdata.THETASQ(1:nx,1:ny,:,it);
hfac4calcz = ones(nx,ny,nr);
hfac4calcz(find(temp00(:,:,:)==0))=0.;
[ind5]=find(hfac4calcz~=0);
temp0 = mfdata.UVELSQ(1:nx,1:ny,:,it);
temp0(find(temp0<0.))=0.;
uvelsq(ind5) = uvelsq(ind5) + temp0(ind5);
temp0 = mfdata.VVELSQ(1:nx,1:ny,:,it);
temp0(find(temp0<0.))=0.;
vvelsq(ind5) = vvelsq(ind5) + temp0(ind5);
temp0 = mfdata.THETASQ(1:nx,1:ny,:,it);
temp0(find(temp0<0.))=0.;
thetasq(ind5) = thetasq(ind5) + temp0(ind5);
temp0 = mfdata.WVELSQ(1:nx,1:ny,:,it);
temp0(find(temp0<0.))=0.;
wvelsq(ind5) = wvelsq(ind5) + temp0(ind5);
temp0 = mfdata.PHIHYDSQ(1:nx,1:ny,:,it);
temp0(find(temp0<0.))=0.;
phihydsq(ind5) = phihydsq(ind5) + temp0(ind5);
temp0 = mfdata.SALTSQ(1:nx,1:ny,:,it);
temp0(find(temp0<0.))=0.;
saltsq(ind5) = saltsq(ind5) + temp0(ind5);
temp0 = mfdata.UV_VEL_C(1:nx,1:ny,:,it);
uvvelc(ind5) = uvvelc(ind5) + temp0(ind5);
temp0 = mfdata.WU_VEL(1:nx,1:ny,:,it);
wuvel(ind5) = wuvel(ind5) + temp0(ind5);
temp0 = mfdata.WV_VEL(1:nx,1:ny,:,it);
wvvel(ind5) = wvvel(ind5) + temp0(ind5);
temp0 = mfdata.UVELTH(1:nx,1:ny,:,it);
uvelth(ind5) = uvelth(ind5) + temp0(ind5);
temp0 = mfdata.VVELTH(1:nx,1:ny,:,it);
vvelth(ind5) = vvelth(ind5) + temp0(ind5);
temp0 = mfdata.WVELTH(1:nx,1:ny,:,it);
wvelth(ind5) = wvelth(ind5) + temp0(ind5);
temp0 = mfdata.UVELSLT(1:nx,1:ny,:,it);
uvelslt(ind5) = uvelslt(ind5) + temp0(ind5);
temp0 = mfdata.VVELSLT(1:nx,1:ny,:,it);
vvelslt(ind5) = vvelslt(ind5) + temp0(ind5);
temp0 = mfdata.WVELSLT(1:nx,1:ny,:,it);
wvelslt(ind5) = wvelslt(ind5) + temp0(ind5);
temp0 = mfdata.UVELPHI(1:nx,1:ny,:,it);
uvelphi(ind5) = uvelphi(ind5) + temp0(ind5);
temp0 = mfdata.VVELPHI(1:nx,1:ny,:,it);
vvelphi(ind5) = vvelphi(ind5) + temp0(ind5);
end
%Release memory for ML time-dependant quantities
clear mfdata
% Divide the sums by the appropriate counter to get time average
%
ind3=find(hfactot~=0);
uvelsq(ind3) = uvelsq(ind3) ./ hfactot(ind3);
vvelsq(ind3) = vvelsq(ind3) ./ hfactot(ind3);
thetasq(ind3) = thetasq(ind3) ./ hfactot(ind3);
wvelsq(ind3) = wvelsq(ind3) ./ hfactot(ind3);
phihydsq(ind3) = phihydsq(ind3) ./ hfactot(ind3);
saltsq(ind3) = saltsq(ind3) ./ hfactot(ind3);
uvvelc(ind3) = uvvelc(ind3) ./ hfactot(ind3);
wuvel(ind3) = wuvel(ind3) ./ hfactot(ind3);
wvvel(ind3) = wvvel(ind3) ./ hfactot(ind3);
uvelth(ind3) = uvelth(ind3) ./ hfactot(ind3);
vvelth(ind3) = vvelth(ind3) ./ hfactot(ind3);
wvelth(ind3) = wvelth(ind3) ./ hfactot(ind3);
uvelslt(ind3) = uvelslt(ind3) ./ hfactot(ind3);
vvelslt(ind3) = vvelslt(ind3) ./ hfactot(ind3);
wvelslt(ind3) = wvelslt(ind3) ./ hfactot(ind3);
uvelphi(ind3) = uvelphi(ind3) ./ hfactot(ind3);
vvelphi(ind3) = vvelphi(ind3) ./ hfactot(ind3);
%2)Use calc2ndmom to compute UV, U squared and V squared
[UVtot,UVtrans,U2tot,U2trans,V2tot,V2trans,errFlag]=calc2ndmom(uav,vav,uvelsq,vvelsq,uvvelc,AngleCS,AngleSN);
%3)Move to centers and rotate moments related to u and v transports:
[wuE,wvN] = rotate_uv2uvEN(wuvel,wvvel,AngleCS,AngleSN);
[uEth,vNth] = rotate_uv2uvEN(uvelth,vvelth,AngleCS,AngleSN);
[uEslt,vNslt] = rotate_uv2uvEN(uvelslt,vvelslt,AngleCS,AngleSN);
[uEphi,vNphi] = rotate_uv2uvEN(uvelphi,vvelphi,AngleCS,AngleSN);
%4) Compute zonal means:
temp = UVtot;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
UVtotz=tempZ(:,:,1);
temp = U2tot;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
U2totz=tempZ(:,:,1);
temp = V2tot;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
V2totz=tempZ(:,:,1);
temp = wvelsq;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
wvelsqz=tempZ(:,:,1);
temp = thetasq;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
potempsqz=tempZ(:,:,1);
temp = phihydsq;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
phihydsqz=tempZ(:,:,1);
temp = saltsq;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
saltsqz=tempZ(:,:,1);
temp = wuE;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
wuEz=tempZ(:,:,1);
temp = wvN;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
wvNz=tempZ(:,:,1);
temp = vNth;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
vNthz=tempZ(:,:,1);
temp = wvelth;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
wvelthz=tempZ(:,:,1);
temp = vNslt;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
vNsltz=tempZ(:,:,1);
temp = wvelslt;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
wvelsltz=tempZ(:,:,1);
temp = vNphi;
[tempZ,dum1,dum2] = calc_ZonAv_CS(temp,1,1,0,XC,YC,XG,YG,RAC,'./',weightout);
vNphiz=tempZ(:,:,1);
%
weightoutz2 = ones(64,nr);
weightoutz2(:,1) = dum1(:,1) ./ dum1(:,17);
%
% Add reference geopotential to anomaly
for ilev = 1:nr
ind1d = find(weightoutz(:,ilev)~=0);
phihydsqz(ind1d,ilev)=phihydsqz(ind1d,ilev) - phiref(ilev).*phiref(ilev) + 2.*phiref(ilev).*phizav(ind1d,ilev);
vNphiz(ind1d,ilev)=vNphiz(ind1d,ilev) + phiref(ilev).*vzav(ind1d,ilev);
end
%
% Write out time averages of zonal mean second moments
fid = fopen('uvtotz.interim','w','b'); fwrite(fid,UVtotz,'real*8'); fclose(fid);
fid = fopen('u2totz.interim','w','b'); fwrite(fid,U2totz,'real*8'); fclose(fid);
fid = fopen('v2totz.interim','w','b'); fwrite(fid,V2totz,'real*8'); fclose(fid);
fid = fopen('wvelsqz.interim','w','b'); fwrite(fid,wvelsqz,'real*8'); fclose(fid);
fid = fopen('potempsqz.interim','w','b'); fwrite(fid,potempsqz,'real*8'); fclose(fid);
fid = fopen('phihydsqz.interim','w','b'); fwrite(fid,phihydsqz,'real*8'); fclose(fid);
fid = fopen('saltsqz.interim','w','b'); fwrite(fid,saltsqz,'real*8'); fclose(fid);
fid = fopen('wuEz.interim','w','b'); fwrite(fid,wuEz,'real*8'); fclose(fid);
fid = fopen('wvNz.interim','w','b'); fwrite(fid,wvNz,'real*8'); fclose(fid);
fid = fopen('vNthz.interim','w','b'); fwrite(fid,vNthz,'real*8'); fclose(fid);
fid = fopen('wvelthz.interim','w','b'); fwrite(fid,wvelthz,'real*8'); fclose(fid);
fid = fopen('vNsltz.interim','w','b'); fwrite(fid,vNsltz,'real*8'); fclose(fid);
fid = fopen('wvelsltz.interim','w','b'); fwrite(fid,wvelsltz,'real*8'); fclose(fid);
fid = fopen('vNphiz.interim','w','b'); fwrite(fid,vNphiz,'real*8'); fclose(fid);
fid = fopen('hfacztot2.interim','w','b'); fwrite(fid,weightoutz2,'real*8'); fclose(fid);
%
% Last step has to wait until all 10-day peices are processed: Compute terms
%
eval(['cd ../'])
