% load COS & SIN of rotation angle:
fid=fopen('proj_cs32_2uEvN.bin','r','b'); 
nx = 192;
ny =  32;
uvEN=fread(fid,nx*ny*2,'real*8'); fclose(fid);
uvEN=reshape(uvEN,[prod(size(uvEN))/2 2]);
AngleCS=uvEN(:,1);
AngleSN=uvEN(:,2);
CS = reshape(AngleCS,[nx ny]);
SN = reshape(AngleSN,[nx ny]);
for fi = 1:6
  is = (fi - 1)*32;
  nTUu(:,:,fi) =  CS(is+[1:32],1:32);
  nTUv(:,:,fi) = -SN(is+[1:32],1:32);
  nTVu(:,:,fi) =  SN(is+[1:32],1:32);
  nTVv(:,:,fi) =  CS(is+[1:32],1:32);
end

load ../set_001/TUV.mat

oTUu = TUu;
oTUv = TUv;
oTVu = TVu;
oTVv = TVv;

clear   TUu TUv TVu TVv

figure(1)
subplot(1,1,1)
for fi = 1:6
  
  iif = (fi-1)*4;
  subplot(6,4,iif+1), surf(oTUu(:,:,fi)), view(2), shading flat
  subplot(6,4,iif+2), surf(oTUv(:,:,fi)), view(2), shading flat
  subplot(6,4,iif+3), surf(oTVu(:,:,fi)), view(2), shading flat
  subplot(6,4,iif+4), surf(oTVv(:,:,fi)), view(2), shading flat

end

figure(2)
subplot(1,1,1)
for fi = 1:6
  
  iif = (fi-1)*4;
  subplot(6,4,iif+1), surf(nTUu(:,:,fi)), view(2), shading flat
  subplot(6,4,iif+2), surf(nTUv(:,:,fi)), view(2), shading flat
  subplot(6,4,iif+3), surf(nTVu(:,:,fi)), view(2), shading flat
  subplot(6,4,iif+4), surf(nTVv(:,:,fi)), view(2), shading flat

end

TUu = nTUu;
TUv = nTUv;
TVu = nTVu;
TVv = nTVv;

save TUV_from_proj_cs32_2uEvN   TUu TUv TVu TVv



