function [uE,vN] = rotate_csAg_EN(u,v)
% [uE,vN] = rotate_csAg_EN(u,v)
%
% Rotate U,V vector components of cs-grid (A-grid) to East,North directions
%  u,v   is a 2-D or 3-D horizontal components of a flow fields.
%  uE,vN are the Eastward,Northward components of the rotated flow field
% assume that the 1rst 2 dimensions of (u,v) are [6*nc nc] for a CS(nc x nc) grid
%
% $Header: /u/gcmpack/MITgcm/utils/cs_grid/Attic/rotate_csAg_EN.m,v 1.2 2005/08/18 18:26:15 jmc Exp $

%Rac='/home/jmc/grid_cs32/' ;
Rac='grid_files/';

NN=size(u);
if length(NN) < 3, nz=1;
else nz=prod(NN(3:end));
end
nnx=NN(1); nc=NN(2); nPg=nnx*nc;
if nnx ~= 6*nc,
 fprintf('Error in CS-dim: %i %i %i \n',NN);
 return
end

u=reshape(u,[nPg nz]);
v=reshape(v,[nPg nz]);

%- rotate toward E,N (lon,lat) directions :
% load COS & SIN of rotation angle:
namfil=['proj_cs',int2str(nc),'_2uEvN.bin'];
fid=fopen([Rac,namfil],'r','b'); uvEN=fread(fid,nPg*2,'real*8'); fclose(fid);
uvEN=reshape(uvEN,[nPg 2]);

uE=zeros(nPg,nz); vN=zeros(nPg,nz);
for k=1:nz;
 uE(:,k)=uvEN(:,1).*u(:,k)-uvEN(:,2).*v(:,k);
 vN(:,k)=uvEN(:,2).*u(:,k)+uvEN(:,1).*v(:,k);
end

uE=reshape(uE,NN);
vN=reshape(vN,NN);

return
