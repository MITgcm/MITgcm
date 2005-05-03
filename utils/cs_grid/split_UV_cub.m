function [u6t,v6t] = split_UV_cub(u3d,v3d,ksign)
% [u6t,v6t] = split_UV_cub(u3d,v3d,ksign)
%---------------------------------------------
% split 3d arrays u,v to 3d x 6 faces
% and add 1 column to U and one row to V
% output is u6t(ny+1,ny,nr,6) & v6t(ny,ny+1,nr,6)
%----------------------------------------------
[nx ny nr]=size(u3d) ;
if nargin < 3 , ksign = 0; end
% ksign = 0 ==> no sign ; ksign = 1 ==> change the sign where needed
% ==> in fact, not used here
plmn = 1 - 2*ksign ; 

dim=length(size(u3d));
if dim == 2, nr=1; [nx ny]=size(u3d) ; else [nx ny nr]=size(u3d) ; end 
nyp=ny+1;
%=================================================================

if dim == 2,

%- split on to 6 tile with overlap in i+1 for u and j+1 for v :
 u6t=zeros(nyp,ny,6);
 v6t=zeros(ny,nyp,6);

 for n=1:6,
  u6t([1:ny],:,n)=u3d([(n-1)*ny+1:n*ny],:);
  v6t(:,[1:ny],n)=v3d([(n-1)*ny+1:n*ny],:);
 end

  u6t(nyp,[1:ny],1)=u3d(1+ny,[1:ny]);
  u6t(nyp,[1:ny],2)=v3d([4*ny:-1:1+(4-1)*ny],1);
  u6t(nyp,[1:ny],3)=u3d(1+3*ny,[1:ny]);
  u6t(nyp,[1:ny],4)=v3d([6*ny:-1:1+(6-1)*ny],1);
  u6t(nyp,[1:ny],5)=u3d(1+5*ny,[1:ny]);
  u6t(nyp,[1:ny],6)=v3d([2*ny:-1:1+(2-1)*ny],1);
 
  v6t([1:ny],nyp,1)=u3d(1+2*ny,[ny:-1:1]);
  v6t([1:ny],nyp,2)=v3d([(3-1)*ny+1:3*ny],1);
  v6t([1:ny],nyp,3)=u3d(1+4*ny,[ny:-1:1]);
  v6t([1:ny],nyp,4)=v3d([(5-1)*ny+1:5*ny],1);
  v6t([1:ny],nyp,5)=u3d(1+0*ny,[ny:-1:1]);
  v6t([1:ny],nyp,6)=v3d([(1-1)*ny+1:1*ny],1);   

else

%- split on to 6 tile with overlap in i+1 for u and j+1 for v :
 u6t=zeros(nyp,ny,nr,6);
 v6t=zeros(ny,nyp,nr,6);

 for n=1:6,
  u6t([1:ny],:,:,n)=u3d([(n-1)*ny+1:n*ny],:,:);
  v6t(:,[1:ny],:,n)=v3d([(n-1)*ny+1:n*ny],:,:);
 end

  u6t(nyp,[1:ny],:,1)=u3d(1+ny,[1:ny],:);
  u6t(nyp,[1:ny],:,2)=v3d([4*ny:-1:1+(4-1)*ny],1,:);
  u6t(nyp,[1:ny],:,3)=u3d(1+3*ny,[1:ny],:);
  u6t(nyp,[1:ny],:,4)=v3d([6*ny:-1:1+(6-1)*ny],1,:);
  u6t(nyp,[1:ny],:,5)=u3d(1+5*ny,[1:ny],:);
  u6t(nyp,[1:ny],:,6)=v3d([2*ny:-1:1+(2-1)*ny],1,:);
 
  v6t([1:ny],nyp,:,1)=u3d(1+2*ny,[ny:-1:1],:);
  v6t([1:ny],nyp,:,2)=v3d([(3-1)*ny+1:3*ny],1,:);
  v6t([1:ny],nyp,:,3)=u3d(1+4*ny,[ny:-1:1],:);
  v6t([1:ny],nyp,:,4)=v3d([(5-1)*ny+1:5*ny],1,:);
  v6t([1:ny],nyp,:,5)=u3d(1+0*ny,[ny:-1:1],:);
  v6t([1:ny],nyp,:,6)=v3d([(1-1)*ny+1:1*ny],1,:);   

end

return
