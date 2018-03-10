function [uCs,vCs,errFlag]=uvLatLon2cube(xc,yc,uFld,vFld,xcs,ycs,spv,cosalpha,sinalpha);

% [uCs,vCs]=uvLatLon2cube(xc,yc,uFld,vFld,xcs,ycs,spv,cosalpha,sinalpha);
% put a vector field (uFld,vFld) on to the C-grid, Cubed-sphere grid: uCs,vCs
% xc,yc   : long,lat position of vector field (uFld,vFld)
%           assume: size(xc) == size(uFld,1) == size(vFld,1)
%             and : size(yc) == size(uFld,2) == size(vFld,2)
% xcs,ycs : long,lat of the Cell-Center point on CS-grid
%           assume: size(xcs)=size(ycs)=[6*nc nc]=size(uCs)[1:2]=size(vCs)[1:2]
% spv     : masking value of lat-lon fields
% cosalpha,sinalpha : [optional] cos and sin of angle of cube-sphered grid with
%                     lat/lon directions, can be obtained from cubeCalcAngle.m
%
% Written by jmc@ocean.mit.edu, 2005.

if nargin < 7, mask=0 ; else mask=1 ; end
uCs=0; vCs=0; mCsU=0; mCsV=0; errFlag=0;

%Rac='/home/jmc/grid_cs32/';
Rac='grid_files/';

nc=size(xcs,2); ncx=6*nc; nPg=ncx*nc; ncp=nc+1;
nr=size(uFld,3);

fprintf('Entering uvLatLon2cube:');
msk1=ones(size(uFld));
if mask == 1,
 fld=ones(size(vFld));
 msk1(find(uFld == spv))=0;
 fld(find(vFld == spv))=0;
 fld=fld-msk1; dd=sum(sum(sum(abs(fld))));
 if dd == 0,
  fprintf(' mask from uFld & vFld match\n');
 else
  fprintf(' different uFld & vFld mask in %i pts\n',dd);
  errFlag=1; return;
 end
 uFld(find(msk1 == 0))=0;
 vFld(find(msk1 == 0))=0;
else
 fprintf(' no mask applied\n');
end

%-- check longitude:
 dd=mean(mean(xcs))-mean(xc);
 fprintf('mean longitude (data/CS): %5.3f %5.3f and dx= %5.3f \n', ...
         mean(xc),mean(mean(xcs)),xc(2)-xc(1));
 fprintf(' min max longitude (data)  : %5.3f %5.3f \n',xc(1),xc(end));
 fprintf(' min max longitude (CSgrid): %5.3f %5.3f \n',min(min(xcs)),max(max(xcs)));
%- add 1 column at the end :
if xc(end) < max(max(xcs)),
 dimFld=size(uFld); nx=dimFld(1); nxp=nx+1; dimFld(1)=nxp;
 fld=uFld; uFld=zeros(dimFld);
 uFld(1:nx,:,:)=fld(1:nx,:,:); uFld(nxp,:,:)=fld(1,:,:);
 fld=vFld; vFld=zeros(dimFld);
 vFld(1:nx,:,:)=fld(1:nx,:,:); vFld(nxp,:,:)=fld(1,:,:);
 fld=msk1; msk1=zeros(dimFld);
 msk1(1:nx,:,:)=fld(1:nx,:,:); msk1(nxp,:,:)=fld(1,:,:);
 xExt=zeros(1,nxp); xExt(1:nx)=xc; xExt(nxp)=xc(1)+360;
 fprintf('Add one column at the end (i=%i): lon= %5.3f\n',dimFld(1),xExt(end));
else
 xExt=xc;
end

% Read cos and sin of rotation angle if not provided on input
if nargin < 9
 namfil=['proj_cs',int2str(nc),'_2uEvN.bin'];
 cosalpha=zeros(nPg); sinalpha=zeros(nPg);
 fid=fopen([Rac,namfil],'r','b');
 cosalpha=fread(fid,nPg,'real*8');
 sinalpha=fread(fid,nPg,'real*8');
 fclose(fid);
end

uvEN=zeros(nPg, 2);
uvEN(:,1)=cosalpha;
uvEN(:,2)=sinalpha;

%- go to CS grid, keeping E-W, N-S direction:
x6s=permute(reshape(xcs,[nc 6 nc]),[1 3 2]);
y6s=permute(reshape(ycs,[nc 6 nc]),[1 3 2]);
[uCsE]=int2CS(uFld,xExt,yc,x6s,y6s);
[vCsN]=int2CS(vFld,xExt,yc,x6s,y6s);
[mskC]=int2CS(msk1,xExt,yc,x6s,y6s);
uCsE=uCsE./mskC;
vCsN=vCsN./mskC;
uCsE(find(mskC==0))=0;
vCsN(find(mskC==0))=0;

%- rotate toward CS-grid directions :
uCsE=reshape(permute(uCsE,[1 3 2 4]),nPg,nr);
vCsN=reshape(permute(vCsN,[1 3 2 4]),nPg,nr);

ucs=zeros(nPg,nr);
vcs=zeros(nPg,nr);

for k=1:nr,
 ucs(:,k)= uvEN(:,1).*uCsE(:,k)+uvEN(:,2).*vCsN(:,k);
 vcs(:,k)=-uvEN(:,2).*uCsE(:,k)+uvEN(:,1).*vCsN(:,k);
end

ucs=reshape(ucs,[ncx nc nr]);
vcs=reshape(vcs,[ncx nc nr]);
mskC=reshape(permute(mskC,[1 3 2 4]),[ncx nc nr]);

%- from A-grid to C-grid :
[u6t]=split_C_cub(ucs);
[v6t]=split_C_cub(vcs);
[m6t]=split_C_cub(mskC);
if nr == 1
 u6t = reshape(u6t,ncp,ncp,nr,6);
 v6t = reshape(v6t,ncp,ncp,nr,6);
 m6t = reshape(m6t,ncp,ncp,nr,6);
end
for n=1:2:6,
 uloc=u6t(1,:,:,n);
 u6t(1,:,:,n)=v6t(1,:,:,n);
 v6t(1,:,:,n)=uloc;
end
for n=2:2:6,
 uloc=u6t(:,1,:,n);
 u6t(:,1,:,n)=v6t(:,1,:,n);
 v6t(:,1,:,n)=uloc;
end

uCs=zeros(nc,nc,nr,6);
vCs=zeros(nc,nc,nr,6);
mCsU=zeros(nc,nc,nr,6);
mCsV=zeros(nc,nc,nr,6);
uCs(:,:,:,:)=u6t([1:nc],[2:ncp],:,:)+u6t([2:ncp],[2:ncp],:,:);
mCsU(:,:,:,:)=m6t([1:nc],[2:ncp],:,:)+m6t([2:ncp],[2:ncp],:,:);
vCs(:,:,:,:)=v6t([2:ncp],[1:nc],:,:)+v6t([2:ncp],[2:ncp],:,:);
mCsV(:,:,:,:)=m6t([2:ncp],[1:nc],:,:)+m6t([2:ncp],[2:ncp],:,:);
uCs=reshape(permute(uCs,[1 4 2 3]),[ncx nc nr])/2;
vCs=reshape(permute(vCs,[1 4 2 3]),[ncx nc nr])/2;
mCsU=reshape(permute(mCsU,[1 4 2 3]),[ncx nc nr])/2;
mCsV=reshape(permute(mCsV,[1 4 2 3]),[ncx nc nr])/2;

if mask == 1,
 uCs(find(mCsU==0.))=spv;
 vCs(find(mCsV==0.))=spv;
end

return

function Qcs=int2CS(Qini, x0,y0,xc,yc)

Qcs=zeros([size(xc) size(Qini,3)]);
for m=1:size(Qini,3)
  for f=1:size(xc,3);
    Qcs(:,:,f,m)=interp2(y0,x0,Qini(:,:,m),yc(:,:,f),xc(:,:,f));
  end
end

return
