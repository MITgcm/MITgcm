function [z6t] = split_Z_cub(z3d)
% [z6t] = split_Z_cub(z3d)
%---------------------------------------------
% split 2d/3d arrays Z to 2d/3d x 6 faces
% and add 1 column + 1 row
% output is z6t(ny+1,ny+1,[nr],6)
% take the average value (over the 3 neighbours) for the 2 missing corners
%----------------------------------------------
dim=length(size(z3d));
if dim == 2, nr=1; [nx ny]=size(z3d) ; else [nx ny nr]=size(z3d) ; end 
nyp=ny+1;
%=================================================================

if dim == 2,

%- split on to 6 tile with overlap in i+1 & j+1 :
 z6t=zeros(nyp,ny+1,6);

 for n=1:6,
   z6t([1:ny],[1:ny],n)=z3d([(n-1)*ny+1:n*ny],[1:ny]);
 end

  z6t(nyp,[1:ny], 1)=z3d(1+ny,[1:ny]);
  z6t(nyp,[2:nyp],2)=z3d([4*ny:-1:1+(4-1)*ny],1);
  z6t(nyp,[1:ny], 3)=z3d(1+3*ny,[1:ny]);
  z6t(nyp,[2:nyp],4)=z3d([6*ny:-1:1+(6-1)*ny],1);
  z6t(nyp,[1:ny], 5)=z3d(1+5*ny,[1:ny]);
  z6t(nyp,[2:nyp],6)=z3d([2*ny:-1:1+(2-1)*ny],1);
 
  z6t([2:nyp],nyp,1)=z3d(1+2*ny,[ny:-1:1]);
  z6t([1:ny], nyp,2)=z3d([(3-1)*ny+1:3*ny],1);
  z6t([2:nyp],nyp,3)=z3d(1+4*ny,[ny:-1:1]);
  z6t([1:ny], nyp,4)=z3d([(5-1)*ny+1:5*ny],1);
  z6t([2:nyp],nyp,5)=z3d(1+0*ny,[ny:-1:1]);
  z6t([1:ny], nyp,6)=z3d([(1-1)*ny+1:1*ny],1);   

%- missing corner :
 zz1=z3d(2*ny,1)+z3d(4*ny,1)+z3d(6*ny,1);
 zz1=zz1/3;
  z6t(nyp,1,2)=zz1;
  z6t(nyp,1,4)=zz1;
  z6t(nyp,1,6)=zz1;

 zz2=z3d(1,ny)+z3d(1+2*ny,ny)+z3d(1+4*ny,ny);
 zz2=zz2/3;
  z6t(1,nyp,1)=zz2;
  z6t(1,nyp,3)=zz2;
  z6t(1,nyp,5)=zz2;
 
else

%- split on to 6 tile with overlap in i+1 & j+1 :
 z6t=zeros(ny+1,ny+1,nr,6);

 for n=1:6,
   z6t([1:ny],[1:ny],:,n)=z3d([(n-1)*ny+1:n*ny],[1:ny],:);
 end

  z6t(nyp,[1:ny], :,1)=z3d(1+ny,[1:ny],:);
  z6t(nyp,[2:nyp],:,2)=z3d([4*ny:-1:1+(4-1)*ny],1,:);
  z6t(nyp,[1:ny], :,3)=z3d(1+3*ny,[1:ny],:);
  z6t(nyp,[2:nyp],:,4)=z3d([6*ny:-1:1+(6-1)*ny],1,:);
  z6t(nyp,[1:ny], :,5)=z3d(1+5*ny,[1:ny],:);
  z6t(nyp,[2:nyp],:,6)=z3d([2*ny:-1:1+(2-1)*ny],1,:);
 
  z6t([2:nyp],nyp,:,1)=z3d(1+2*ny,[ny:-1:1],:);
  z6t([1:ny], nyp,:,2)=z3d([(3-1)*ny+1:3*ny],1,:);
  z6t([2:nyp],nyp,:,3)=z3d(1+4*ny,[ny:-1:1],:);
  z6t([1:ny], nyp,:,4)=z3d([(5-1)*ny+1:5*ny],1,:);
  z6t([2:nyp],nyp,:,5)=z3d(1+0*ny,[ny:-1:1],:);
  z6t([1:ny], nyp,:,6)=z3d([(1-1)*ny+1:1*ny],1,:);   

%- missing corner :
zz1=zeros(1,nr); zz2=zeros(1,nr);
 zz1=z3d(2*ny,1,:)+z3d(4*ny,1,:)+z3d(6*ny,1,:);
 zz1=zz1/3;
  z6t(nyp,1,:,2)=zz1;
  z6t(nyp,1,:,4)=zz1;
  z6t(nyp,1,:,6)=zz1;

 zz2=z3d(ny,1,:)+z3d(ny,1+2*ny,:)+z3d(ny,1+4*ny,:)+z3d(6*ny,1,:);
 zz2=zz2/3;
  z6t(1,nyp,:,1)=zz2;
  z6t(1,nyp,:,3)=zz2;
  z6t(1,nyp,:,5)=zz2;
 
end

return
