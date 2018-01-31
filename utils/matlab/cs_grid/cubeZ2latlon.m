function [z] = cubeZ2latlon(x,y,c,xi,yi)
% z=cubeZ2latlon(x,y,c,xi,yi);
%
% Re-grids model output on expanded spherical cube to lat-lon grid.
%  x,y   are 1-D arrays (reshaped Horiz. grid) of the cell-corner coordinates 
%  c     is a 1-D or 2-D scalar field
%  xi,yi are vectors of the new regular lat-lon grid to interpolate to.
%  z     is the interpolated data with dimensions of size(xi) by size(yi).
%
% e.g.
% >> xg=rdmds('XG'); nPts=prod(size(xg)); x=rehsape(xg,nPts,1);
% >> yg=rdmds('YG'); y=rehsape(yg,nPts,1);
% >> PsiH=rdmds('psiH.0000513360');
% >> xi=-179:2:180;yi=-89:2:90;
% >> psiI=cubeZ2latlon(x,y,psiH,xi,yi);
% can also add the 2 missing corner :
% >> nc=sqrt(nPts/6);
% >> x(end+1)=mean(xg([1:2*nc:6*nc],nc)); y(end+1)=mean(yg([1:2*nc:6*nc],nc));
% >> x(end+1)=mean(xg([2*nc:2*nc:6*nc],1)); y(end+1)=mean(yg([2*nc:2*nc:6*nc],1));
%
% Written by jmc@ocean.mit.edu, 2005.
NN=size(c);
[nPt2 nz]=size(c);
nc=fix(sqrt(nPt2/6)); nPts=6*nc*nc;

for k=1:nz;
X=x(1:nPts);X=reshape(X,6*nc,nc);
Y=y(1:nPts);Y=reshape(Y,6*nc,nc);
C=c(1:nPts,k);C=reshape(C,6*nc,nc);

i=3*nc+(1:nc);j=floor(nc/2)+1;
X(end+1,:)=X(i,j)'-360; Y(end+1,:)=Y(i,j)'; C(end+1,:)=C(i,j)';
%i=3*nc+(1:nc);j=floor(nc/2)+1;
%X(end+1,:)=X(i,j)'+360; Y(end+1,:)=Y(i,j)'; C(end+1,:)=C(i,j)';
i=5*nc+1+floor(nc/2);j=1:floor(nc/2);
X(end+1,j)=X(i,j)-360;
Y(end+1,j)=Y(i,j);
C(end+1,j)=C(i,j);
%--
j=1+floor(nc/2);i=2*nc+j; if Y(i,j)==90, X(i,j)=180; end
i=2*nc+(floor(nc/2)+1:nc);j=1+floor(nc/2);
X(end,i-2*nc)=X(i,j)'-360;
Y(end,i-2*nc)=Y(i,j)';
C(end,i-2*nc)=C(i,j)';
%--
%i=5*nc+floor(nc/2)+1;j=1:floor(nc/2);
%X(end,j+nc/2)=X(i,j)-360;
%Y(end,j+nc/2)=Y(i,j);
%C(end,j+nc/2)=C(i,j);
%i=2*nc+(nc/2+1:nc);j=floor(nc/2);
%X(end+1,1:nc/2)=X(i,j)'-360;
%Y(end+1,1:nc/2)=Y(i,j)';
%C(end+1,1:nc/2)=C(i,j)';
%i=2*nc+(nc/2+1:nc);j=floor(nc/2)+1;
%X(end,nc/2+1:nc)=X(i,j)'+360;
%Y(end,nc/2+1:nc)=Y(i,j)';
%C(end,nc/2+1:nc)=C(i,j)';

j=1+floor(nc/2);i=5*nc+j; ij=i+(j-1)*nc*6;
if Y(i,j)==-90, 
% fprintf('South pole: %i %i %f %f\n',i,j,X(i,j),Y(i,j));
  X(i,j)=180;
end

X=reshape(X,prod(size(X)),1);
Y=reshape(Y,prod(size(Y)),1);
C=reshape(C,prod(size(C)),1);

[I]=find(Y==-90);
if length(I)==1, 
% fprintf('South pole: %i %f %f\n',I,X(I),Y(I));
  X(end+1)=X(I)-360;
  Y(end+1)=Y(I);
  C(end+1)=C(I);
end

if nPt2 > nPts,
 X(end+1)=x(nPts+1);
 Y(end+1)=y(nPts+1);
 C(end+1)=c(nPts+1,k);
end
if nPt2 == nPts+2,
 X(end+1)=x(nPt2);
 Y(end+1)=y(nPt2);
 C(end+1)=c(nPt2,k);
end

z(:,:,k)=griddata(Y,X,C,yi,xi');
end % k

if length(NN)>1
 z=reshape(z,[size(z,1) size(z,2) NN(2:end)]);
end
