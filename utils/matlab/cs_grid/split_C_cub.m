function [v6t] = split_C_cub(v3d,kad)
% [v6t] = split_C_cub(v3d,[kad])
%---------------------------------------------
% split 2d/3d arrays V, center, to 2d/3d x 6 faces
% and (kad=1): add 1 column + 1 row <== at the begining !!!
%  => output is v6t(ny+1,ny+1,[nr],6)
% and (kad=2): add also 1 column + 1 row <== at the end !!!
%  => output is v6t(ny+2,ny+2,[nr],6)
%----------------------------------------------
% Written by jmc@mit.edu, 2005.

if nargin < 2, kad=1; end
if kad~=0 & kad~=1 & kad~=2, fprintf('kad= %f => Bad value',kad); return;end
%------------------------------
 dims=size(v3d); n1h=dims(1); n2h=dims(2);
 if length(dims) == 2, nr=1; else nr=prod(dims(3:end)); end
 if n1h == 6*n2h, nc=n2h;
   v3d=permute(reshape(v3d,[nc 6 nc nr]),[1 3 2 4]);
 elseif n1h*6 == n2h, nc=n1h;
   v3d=reshape(v3d,[nc nc 6 nr]);
 else
  error([' input var size: ',int2str(n1h),' x ',int2str(n2h),' does not fit regular cube !']);
 end
 ncp=nc+1 ; n2p=nc+2; nye=nc+kad;
%--
%=================================================================

%- split on to 6 tiles with overlap in i+1 & j+1 :
 v6t=zeros(nye,nye,6,nr);

 if kad == 0,
  v6t=v3d;
 else
  v6t(2:ncp,2:ncp,:,:)=v3d(1:nc,1:nc,:,:);

%- add overlap in i=1 & j=1 :

  v6t(1,[2:ncp], 1,:)=v6t([ncp:-1:2],ncp,5,:);
  v6t(1,[2:ncp], 3,:)=v6t([ncp:-1:2],ncp,1,:);
  v6t(1,[2:ncp], 5,:)=v6t([ncp:-1:2],ncp,3,:);
  v6t(1,[2:ncp], 2,:)=v6t(ncp,[2:ncp],1,:);
  v6t(1,[2:ncp], 4,:)=v6t(ncp,[2:ncp],3,:);
  v6t(1,[2:ncp], 6,:)=v6t(ncp,[2:ncp],5,:);

  v6t([1:ncp],1, 1,:)=v6t([1:ncp],ncp,6,:);
  v6t([1:ncp],1, 3,:)=v6t([1:ncp],ncp,2,:);
  v6t([1:ncp],1, 5,:)=v6t([1:ncp],ncp,4,:);
  v6t([2:ncp],1, 2,:)=v6t(ncp,[ncp:-1:2],6,:);
  v6t([2:ncp],1, 4,:)=v6t(ncp,[ncp:-1:2],2,:);
  v6t([2:ncp],1, 6,:)=v6t(ncp,[ncp:-1:2],4,:);
  v6t(1,1,2,:)=v6t(ncp,2,1,:);
  v6t(1,1,4,:)=v6t(ncp,2,3,:);
  v6t(1,1,6,:)=v6t(ncp,2,5,:);

 end

%- add overlap in i=ny+1 & j=ny+1 :
 if kad == 2,
  v6t(n2p,[1:ncp], 1,:)=v6t(2,[1:ncp],2,:);
  v6t(n2p,[1:ncp], 3,:)=v6t(2,[1:ncp],4,:);
  v6t(n2p,[1:ncp], 5,:)=v6t(2,[1:ncp],6,:);
  v6t(n2p,[2:n2p], 2,:)=v6t([ncp:-1:1],2,4,:);
  v6t(n2p,[2:n2p], 4,:)=v6t([ncp:-1:1],2,6,:);
  v6t(n2p,[2:n2p], 6,:)=v6t([ncp:-1:1],2,2,:);
  v6t(n2p,n2p,1,:)=v6t(2,2,3,:);
  v6t(n2p,n2p,3,:)=v6t(2,2,5,:);
  v6t(n2p,n2p,5,:)=v6t(2,2,1,:);
  v6t(n2p,1,2,:)=v6t(ncp,1,6,:);
  v6t(n2p,1,4,:)=v6t(ncp,1,2,:);
  v6t(n2p,1,6,:)=v6t(ncp,1,4,:);

  v6t([1:n2p],n2p, 2,:)=v6t([1:n2p],2,3,:);
  v6t([1:n2p],n2p, 4,:)=v6t([1:n2p],2,5,:);
  v6t([1:n2p],n2p, 6,:)=v6t([1:n2p],2,1,:);
  v6t([2:n2p],n2p, 1,:)=v6t(2,[ncp:-1:1],3,:);
  v6t([2:n2p],n2p, 3,:)=v6t(2,[ncp:-1:1],5,:);
  v6t([2:n2p],n2p, 5,:)=v6t(2,[ncp:-1:1],1,:);
  v6t(1,n2p,1,:)=v6t(1,ncp,5,:);
  v6t(1,n2p,3,:)=v6t(1,ncp,1,:);
  v6t(1,n2p,5,:)=v6t(1,ncp,3,:);

 end

%- Put back to standard shape:
 v6t=permute(v6t,[1 2 4 3]);
 if length(dims) == 2,
   v6t=squeeze(v6t);
 else
   v6t=reshape(v6t,[nye nye dims(3:end) 6]);
 end

return
