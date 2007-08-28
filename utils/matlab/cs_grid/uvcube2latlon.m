function [U,V,ub,vb] = uvcube2latlon(LON,LAT,u,v,xc,yc,cosalpha,sinalpha)
% [ui,vi]=cube2latlon(x,y,u,v,xi,yi);
%
% Re-grids model output on expanded spherical cube to lat-lon grid.
%  x,y   are 2-D arrays of the cell-centered coordinates 
%  u,v   is a 2-D or 3-D horizontal components of model flow fields.
%  xi,yi are vectors of the new regular lat-lon grid to interpolate to.
%  ui,vi are the flow fields with dimensions of size(xi) x size(yi) size(u,3).
%
% e.g.
% >> x=rdmds('XC');
% >> y=rdmds('YC');
% >> u=rdmds('uVeltave.0000513360');
% >> v=rdmds('vVeltave.0000513360');
% >> xi=-179:2:180;yi=-89:2:90;
% >> [ui,vi]=uvcube2latlon(x,y,u,v,xi,yi);
%
% Written by adcroft@.mit.edu, 2001.
% $Header: /u/gcmpack/MITgcm/utils/matlab/cs_grid/uvcube2latlon.m,v 1.2 2007/08/28 16:16:16 molod Exp $
% $Name:  $

NN=size(u);
[nnx ny nz]=size(u);

U=reshape(u,[ny 6 ny nz]);
V=reshape(v,[ny 6 ny nz]);

uu=zeros(ny+1,6,ny,nz);
vv=zeros(ny,6,ny+1,nz);

for k=1:6;
 uu(1:ny,k,:,:)=U(:,k,:,:);
 vv(:,k,1:ny,:)=V(:,k,:,:);
end

for k=1:nz;
uu(ny+1,1:2:6,:,k)=uu(1,2:2:6,:,k);
uu(ny+1,2:2:6,:,k)=vv(ny:-1:1,[4:2:6 2:2:3],1,k)';
vv(:,2:2:6,ny+1,k)=vv(:,[3:2:6 1],1,k);
vv(:,1:2:6,ny+1,k)=squeeze(uu(1,[3:2:6 1],ny:-1:1,k))';
end

ub=(uu(1:ny,:,:,:)+uu(2:ny+1,:,:,:))/2;
vb=(vv(:,:,1:ny,:)+vv(:,:,2:ny+1,:))/2;

% Read cos and sin of rotation angle if not provided on input
if nargin == 8
 cosalpha=reshape(cosalpha,[ny 6 ny]);
 sinalpha=reshape(sinalpha,[ny 6 ny]);
clear U V
for kk=1:nz;
for k=1:6;
 U(:,k,:,kk)=squeeze(cosalpha(:,k,:)).*squeeze(ub(:,k,:,kk))-squeeze(sinalpha(:,k,:)).*squeeze(vb(:,k,:,kk));
 V(:,k,:,kk)=squeeze(sinalpha(:,k,:)).*squeeze(ub(:,k,:,kk))+squeeze(cosalpha(:,k,:)).*squeeze(vb(:,k,:,kk));
end
end

else

load TUV
clear U V

for kk=1:nz;
for k=1:6;
 U(:,k,:,kk)=TUu(:,:,k).*squeeze(ub(:,k,:,kk))+TUv(:,:,k).*squeeze(vb(:,k,:,kk));
 V(:,k,:,kk)=TVu(:,:,k).*squeeze(ub(:,k,:,kk))+TVv(:,:,k).*squeeze(vb(:,k,:,kk));
end
end

end

ub=reshape(U,[nnx NN(2:end)]);
vb=reshape(V,[nnx NN(2:end)]);

%U=cube2latlon(LON,LAT,U,xc,yc);
%V=cube2latlon(LON,LAT,V,xc,yc);
del=cube2latlon_preprocess(LON,LAT,xc,yc);
U=cube2latlon_fast(del,ub);
V=cube2latlon_fast(del,vb);
