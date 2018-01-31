function [v6t] = split_C_cub(v3d,kad)
% [v6t] = split_C_cub(v3d,[kad])
%---------------------------------------------
% split 2d/3d arrays V, center, to 2d/3d x 6 faces
% and (kad=1): add 1 column + 1 row <== at the begining !!!
%  => output is v6t(ny+1,ny+1,[nr],6)
% and (kad=2): add also 1 column + 1 row <== at the end !!!
%  => output is v6t(ny+2,ny+2,[nr],6)
%----------------------------------------------
% Written by jmc@ocean.mit.edu, 2005.
if nargin < 2, kad=1; end
if kad~=0 & kad~=1 & kad~=2, fprintf('kad= %f => Bad value',kad); return;end
%--
dims=size(v3d);
nx=dims(1);
ny=dims(2);
nyp=ny+1; n2p=ny+2; nye=ny+kad;
if length(dims) == 2, 
 nr=1; 
%fprintf('split_C_cub: kad= %i ; output dim: %i %i %i \n',kad,nye,nye,6);
else 
 nr=prod(dims(3:end));
%fprintf('split_C_cub: kad= %i ; output dim: %i %i %i %i \n',kad,nye,nye,nr,6);
end
%=================================================================

%- split on to 6 tiles with overlap in i+1 & j+1 :
 v3d=reshape(v3d,[nx ny nr]);
 v6t=zeros(nye,nye,6,nr);

 if kad == 0,
  v6t=permute(reshape(v3d,[ny 6 ny nr]),[1 3 2 4]);
 else
  for n=1:6,
   v6t([2:nyp],[2:nyp],n,:)=v3d([(n-1)*ny+1:n*ny],[1:ny],:);
  end

%- add overlap in i=1 & j=1 :

  v6t(1,[2:nyp], 1,:)=v6t([nyp:-1:2],nyp,5,:);
  v6t(1,[2:nyp], 3,:)=v6t([nyp:-1:2],nyp,1,:);
  v6t(1,[2:nyp], 5,:)=v6t([nyp:-1:2],nyp,3,:);
  v6t(1,[2:nyp], 2,:)=v6t(nyp,[2:nyp],1,:);
  v6t(1,[2:nyp], 4,:)=v6t(nyp,[2:nyp],3,:);
  v6t(1,[2:nyp], 6,:)=v6t(nyp,[2:nyp],5,:);

  v6t([1:nyp],1, 1,:)=v6t([1:nyp],nyp,6,:);
  v6t([1:nyp],1, 3,:)=v6t([1:nyp],nyp,2,:);
  v6t([1:nyp],1, 5,:)=v6t([1:nyp],nyp,4,:);
  v6t([2:nyp],1, 2,:)=v6t(nyp,[nyp:-1:2],6,:);
  v6t([2:nyp],1, 4,:)=v6t(nyp,[nyp:-1:2],2,:);
  v6t([2:nyp],1, 6,:)=v6t(nyp,[nyp:-1:2],4,:);
  v6t(1,1,2,:)=v6t(nyp,2,1,:);
  v6t(1,1,4,:)=v6t(nyp,2,3,:);
  v6t(1,1,6,:)=v6t(nyp,2,5,:);

 end

%- add overlap in i=ny+1 & j=ny+1 :
 if kad == 2,
  v6t(n2p,[1:nyp], 1,:)=v6t(2,[1:nyp],2,:);
  v6t(n2p,[1:nyp], 3,:)=v6t(2,[1:nyp],4,:);
  v6t(n2p,[1:nyp], 5,:)=v6t(2,[1:nyp],6,:);
  v6t(n2p,[2:n2p], 2,:)=v6t([nyp:-1:1],2,4,:);
  v6t(n2p,[2:n2p], 4,:)=v6t([nyp:-1:1],2,6,:);
  v6t(n2p,[2:n2p], 6,:)=v6t([nyp:-1:1],2,2,:);
  v6t(n2p,n2p,1,:)=v6t(2,2,3,:);
  v6t(n2p,n2p,3,:)=v6t(2,2,5,:);
  v6t(n2p,n2p,5,:)=v6t(2,2,1,:);
  v6t(n2p,1,2,:)=v6t(nyp,1,6,:);
  v6t(n2p,1,4,:)=v6t(nyp,1,2,:);
  v6t(n2p,1,6,:)=v6t(nyp,1,4,:);

  v6t([1:n2p],n2p, 2,:)=v6t([1:n2p],2,3,:);
  v6t([1:n2p],n2p, 4,:)=v6t([1:n2p],2,5,:);
  v6t([1:n2p],n2p, 6,:)=v6t([1:n2p],2,1,:);
  v6t([2:n2p],n2p, 1,:)=v6t(2,[nyp:-1:1],3,:);
  v6t([2:n2p],n2p, 3,:)=v6t(2,[nyp:-1:1],5,:);
  v6t([2:n2p],n2p, 5,:)=v6t(2,[nyp:-1:1],1,:);
  v6t(1,n2p,1,:)=v6t(1,nyp,5,:);
  v6t(1,n2p,3,:)=v6t(1,nyp,1,:);
  v6t(1,n2p,5,:)=v6t(1,nyp,3,:);

 end

%- Put back to standard shape:
 v6t=permute(v6t,[1 2 4 3]);
 if length(dims) == 2,
   v6t=squeeze(v6t);
 else
   v6t=reshape(v6t,[nye nye dims(3:end) 6]);
 end

return
