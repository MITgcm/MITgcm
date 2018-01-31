function [UVtot,UVtrans,U2tot,U2trans,V2tot,V2trans,errFlag]=calc2ndmom(u,v,usq,vsq,uv,cosalpha,sinalpha);
% [UVtot,UVtrans,U2tot,U2trans,V2tot,V2trans,errFlag]=calc2ndmom(u,v,usq,vsq,uv);
% Compute UV, U2 and V2 (at cubed-sphere A grid points) using cubed-sphere input
% u       = time averaged u-wind on cubed sphere grid at u-points
% v       = time averaged v-wind on cubed sphere grid at v-points
% usq     = time averaged square of u-wind on cubed sphere grid at u-points
% vsq     = time averaged square of v-wind on cubed sphere grid at v-points
% uv      = time averaged product of u-wind and v-wind on cubed sphere grid at mass points
%
% Written by molod@ocean.mit.edu, 2005.
fprintf('Entering calc2ndmom: \n');
errFlag=0;
%
nc=size(u,2); ncx=6*nc; nPg=ncx*nc;
nz=size(u,3);
%
%Check size of input arrays. if read by rdmnc, remove column and row
%                            if read by rdmds, leave alone
%                            if wrong size altogether, get out with error
%
NN = size(u);
if NN(1) == 6*nc+1,
 fprintf('Need to remove a column \n');
 U = u(1:6*nc,:);
 U2 = usq(1:6*nc,:);
elseif NN(1) == 6*nc,
 fprintf('No U reshaping needed \n');
 U = u;
 U2 = usq;
else
 fprintf('Error in U-point CS-dim: %i %i %i \n',NN);
 return
end
%
NN = size(v);
if NN(2) == nc+1,
 fprintf('Need to remove a row ');
 V = v(:,1:nc);
 V2 = vsq(:,1:nc);
elseif NN(2) == nc,
 fprintf('No V reshaping needed \n');
 V = v;
 V2 = vsq;
else
 fprintf('Error in V-point CS-dim: %i %i %i \n',NN);
 return
end
%
%Get ready to do the exchange
%
U=reshape(U,[nc 6 nc nz]);
U2=reshape(U2,[nc 6 nc nz]);
V=reshape(V,[nc 6 nc nz]);
V2=reshape(V2,[nc 6 nc nz]);
%
uu=zeros(nc+1,6,nc,nz);
uu2=zeros(nc+1,6,nc,nz);
vv=zeros(nc,6,nc+1,nz);
vv2=zeros(nc,6,nc+1,nz);
%
for ifc=1:6;
 uu(1:nc,ifc,:,:)=U(:,ifc,:,:);
 uu2(1:nc,ifc,:,:)=U2(:,ifc,:,:);
 vv(:,ifc,1:nc,:)=V(:,ifc,:,:);
 vv2(:,ifc,1:nc,:)=V2(:,ifc,:,:);
end
%
% do the exchange
for k=1:nz;
uu(nc+1,1:2:6,:,k)=uu(1,2:2:6,:,k);
uu(nc+1,2:2:6,:,k)=vv(nc:-1:1,[4:2:6 2:2:3],1,k)';
uu2(nc+1,1:2:6,:,k)=uu2(1,2:2:6,:,k);
uu2(nc+1,2:2:6,:,k)=vv2(nc:-1:1,[4:2:6 2:2:3],1,k)';
vv(:,2:2:6,nc+1,k)=vv(:,[3:2:6 1],1,k);
vv(:,1:2:6,nc+1,k)=squeeze(uu(1,[3:2:6 1],nc:-1:1,k))';
vv2(:,2:2:6,nc+1,k)=vv2(:,[3:2:6 1],1,k);
vv2(:,1:2:6,nc+1,k)=squeeze(uu2(1,[3:2:6 1],nc:-1:1,k))';
end
%
% Interp to mass points
ui=(uu(1:nc,:,:,:)+uu(2:nc+1,:,:,:))/2;
ub2i=(uu(1:nc,:,:,:).*uu(1:nc,:,:,:)+uu(2:nc+1,:,:,:).*uu(2:nc+1,:,:,:))/2;
u2i=(uu2(1:nc,:,:,:)+uu2(2:nc+1,:,:,:))/2;
vj=(vv(:,:,1:nc,:)+vv(:,:,2:nc+1,:))/2;
vb2j=(vv(:,:,1:nc,:).*vv(:,:,1:nc,:)+vv(:,:,2:nc+1,:).*vv(:,:,2:nc+1,:))/2;
v2j=(vv2(:,:,1:nc,:)+vv2(:,:,2:nc+1,:))/2;
%
ui=reshape(ui,[nPg nz]);
u2i=reshape(u2i,[nPg nz]);
ub2i=reshape(ub2i,[nPg nz]);
vj=reshape(vj,[nPg nz]);
v2j=reshape(v2j,[nPg nz]);
vb2j=reshape(vb2j,[nPg nz]);
UV=reshape(uv,[nPg nz]);
%
% Read cos and sin of rotation angle if not provided on input
if nargin == 5
 Rac='/u/u2/jmc/';
 namfil=['proj_cs',int2str(nc),'_2uEvN.bin'];
 cosalpha=zeros(nPg); sinalpha=zeros(nPg);
 fid=fopen([Rac,namfil],'r','b'); 
 cosalpha=fread(fid,nPg,'real*8'); 
 sinalpha=fread(fid,nPg,'real*8'); 
 fclose(fid);
end
%
UVtrans=zeros(nPg,nz); UVtot=zeros(nPg,nz);
U2trans=zeros(nPg,nz); U2tot=zeros(nPg,nz);
V2trans=zeros(nPg,nz); V2tot=zeros(nPg,nz);
%
for k=1:nz;
 UVtrans(:,k)=( (u2i(:,k)-ub2i(:,k)) - (v2j(:,k)-vb2j(:,k))) .* cosalpha .* sinalpha ...
              + ( UV(:,k) - ui(:,k).*vj(:,k) ) .* (cosalpha.*cosalpha - sinalpha.*sinalpha);
 UVtot(:,k)=UVtrans(:,k) + ( ui(:,k).*ui(:,k) - vj(:,k).*vj(:,k) ) .* cosalpha .* sinalpha ...
                    + ui(:,k).*vj(:,k).*(cosalpha.*cosalpha - sinalpha.*sinalpha);
 U2tot(:,k) = u2i(:,k) .* cosalpha.*cosalpha + v2j(:,k).*sinalpha.*sinalpha ...
              - 2.* UV(:,k) .*cosalpha .* sinalpha;
 U2trans(:,k) = U2tot(:,k) - ...
                (  ui(:,k).*ui(:,k).*cosalpha.*cosalpha + vj(:,k).*vj(:,k).*sinalpha.*sinalpha ...
                   - 2.* ui(:,k).*vj(:,k) .*cosalpha .* sinalpha);
 V2tot(:,k) = u2i(:,k) .* sinalpha.*sinalpha + v2j(:,k).*cosalpha.*cosalpha ...
              + 2.* UV(:,k) .*cosalpha .* sinalpha;
 V2trans(:,k) = V2tot(:,k) - ...
                (  ui(:,k).*ui(:,k).*sinalpha.*sinalpha + vj(:,k).*vj(:,k).*cosalpha.*cosalpha ...
                   + 2.* ui(:,k).*vj(:,k) .*cosalpha .* sinalpha);
end
%
UVtot=reshape(UVtot,[6*nc nc nz]);
UVtrans=reshape(UVtrans,[6*nc nc nz]);
U2tot=reshape(U2tot,[6*nc nc nz]);
U2trans=reshape(U2trans,[6*nc nc nz]);
V2tot=reshape(V2tot,[6*nc nc nz]);
V2trans=reshape(V2trans,[6*nc nc nz]);
%
clear sinalpha cosalpha
