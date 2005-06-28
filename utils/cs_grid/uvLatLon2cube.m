function [uCs,vCs]=uvLatLon2cube(xc,yc,uFld,vFld,xcs,ycs);
% [uCs,vCs]=uvLatLon2cube(xc,yc,uFld,vFld,xcs,ycs);
% put a vector field (uFld,vFld) on to the C-grid, Cubed-sphere grid: uCs,vCs
% xc,yc   = long,lat position of vector field (uFld,vFld)
%           assume: size(xc) == size(uFld,1) == size(vFld,1)
%             and : size(yc) == size(uFld,2) == size(vFld,2)
% xcs,ycs = long,lat of the Cell-Center point on CS-grid
%           assume: size(xcs)=size(ycs)=[6*nc nc]=size(uCs)[1:2]=size(vCs)[1:2]
% $Header: /u/gcmpack/MITgcm/utils/cs_grid/Attic/uvLatLon2cube.m,v 1.1 2005/06/28 02:34:54 jmc Exp $

%Rac='/home/jmc/grid_cs32/';
Rac='grid_files/';

nc=size(xcs,2); ncx=6*nc; nPg=ncx*nc;
nr=size(uFld,3);

namfil=['proj_cs',int2str(nc),'_2uEvN.bin'];
fid=fopen([Rac,namfil],'r','b'); uvEN=fread(fid,nPg*2,'real*8'); fclose(fid);
uvEN=reshape(uvEN,[nPg 2]);

%- go to CS grid, keeping E-W, N-S direction:
x6s=permute(reshape(xcs,[nc 6 nc]),[1 3 2]);
y6s=permute(reshape(ycs,[nc 6 nc]),[1 3 2]);
[uCsE]=int2CS(uFld,xc,yc,x6s,y6s);
[vCsN]=int2CS(vFld,xc,yc,x6s,y6s);

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

%- from A-grid to C-grid :
[u6t]=split_C_cub(ucs);
[v6t]=split_C_cub(vcs);
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
%size(u6t)
ncp=nc+1;

uCs=zeros(nc,nc,nr,6);
vCs=zeros(nc,nc,nr,6);
uCs(:,:,:,:)=u6t([1:nc],[2:ncp],:,:)+u6t([2:ncp],[2:ncp],:,:);
vCs(:,:,:,:)=v6t([2:ncp],[1:nc],:,:)+v6t([2:ncp],[2:ncp],:,:);
uCs=reshape(permute(uCs,[1 4 2 3]),[ncx nc nr])/2;
vCs=reshape(permute(vCs,[1 4 2 3]),[ncx nc nr])/2;

return

function Qcs=int2CS(Qini, x0,y0,xc,yc)

Qcs=zeros([size(xc) size(Qini,3)]);
for m=1:size(Qini,3)
  for f=1:size(xc,3);
    Qcs(:,:,f,m)=interp2(y0,x0,Qini(:,:,m),yc(:,:,f),xc(:,:,f));
  end
end

return
