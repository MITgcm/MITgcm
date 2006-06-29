function [] = merccube(XX,YY,C)
% merccube(x,y,c)
%
% Plots cubed-sphere data in mercator projection. (x,y) are
% coordinates, c is cell-centered scalar to be plotted.
% All arrays (x,y,c) should have dimensions of NxNx6 or 6NxN.
%
% The default plotting mode is shading faceted. Using this or
% shading flat, (x,y) should be the coordinates of grid-corners
% and can legitimately have dimension (N+1)x(N+1)x6.
%
% If using shading interp, then (x,y) must be the coordinates of
% the cell centers.
%
% e.g.
%
% xg=rdmds('XG');
% yg=rdmds('YG');
% ps=rdmds('Eta.0000000000');
% mercube(xg,yg,ps);
% colorbar;xlabel('Longitude');ylabel('Latitude');
%
% xc=rdmds('XC');
% yc=rdmds('YC');
% mercube(xc,yc,ps);shading interp

XXdim = size(XX);
XX = reshape(XX,[prod(XXdim),1]);
XX(find(abs(XX) < 0.01)) = 0;
XX(find(abs(XX) > 179.99)) = 180;
XX = reshape(XX,XXdim);

if max(max(max(YY)))-min(min(min(YY))) < 3*pi
 convert=(180/pi);
else
 convert=1;
end
X=XX;
Y=YY;
Q=C;

if ndims(Q)==2 & size(Q,1)==6*size(Q,2)
 [nx ny nt]=size(X);
 X=permute( reshape(X,[nx/6 6 ny]),[1 3 2]);
 Y=permute( reshape(Y,[nx/6 6 ny]),[1 3 2]);
 Q=permute( reshape(Q,[nx/6 6 ny]),[1 3 2]);
elseif ndims(Q)==3 & size(Q,2)==6
 X=permute( X,[1 3 2]);
 Y=permute( Y,[1 3 2]);
 Q=permute( Q,[1 3 2]);
elseif ndims(Q)==3 & size(Q,3)==6
 [nx ny nt]=size(X);
else
 size(XX)
 size(YY)
 size(C)
 error('Dimensions should be 2 or 3 dimensions: NxNx6, 6NxN or Nx6xN');
end

if size(X,1)==size(Q,1)
 X(end+1,:,:)=NaN;
 X(:,end+1,:)=NaN;
 X(end,:,[1 3 5])=X(1,:,[2 4 6]);
 X(:,end,[2 4 6])=X(:,1,[3 5 1]);
 X(:,end,[1 3 5])=squeeze(X(1,end:-1:1,[3 5 1]));
 X(end,:,[2 4 6])=squeeze(X(end:-1:1,1,[4 6 2]));
 X(1,end,[1 3 5])=X(1,1,1);
 X(end,1,[2 4 6])=X(end,end,2);
 Y(end+1,:,:)=NaN;
 Y(:,end+1,:)=NaN;
 Y(end,:,[1 3 5])=Y(1,:,[2 4 6]);
 Y(:,end,[2 4 6])=Y(:,1,[3 5 1]);
 Y(:,end,[1 3 5])=squeeze(Y(1,end:-1:1,[3 5 1]));
 Y(end,:,[2 4 6])=squeeze(Y(end:-1:1,1,[4 6 2]));
 Y(1,end,[1 3 5])=Y(end,end,1);
 Y(end,1,[2 4 6])=Y(1,1,2);
end
[nx ny nt]=size(X);

Q(end+1,:,:)=NaN;
Q(:,end+1,:)=NaN;
Q(end,:,[1 3 5])=Q(1,:,[2 4 6]);
Q(:,end,[2 4 6])=Q(:,1,[3 5 1]);
Q(:,end,[1 3 5])=squeeze(Q(1,end:-1:1,[3 5 1]));
Q(end,:,[2 4 6])=squeeze(Q(end:-1:1,1,[4 6 2]));

hnx=ceil(nx/2);
hny=ceil(ny/2);

for k=1:6;
 i=1:hnx;
 x=longitude(convert.*X(i,:,k))./convert;
 pcolor(x,Y(i,:,k),Q(i,:,k))
 axis([-180 180 -90 90]./convert);
 hold on
 if max(max(max(x)))>180./convert
  pcolor(x-360./convert,Y(i,:,k),Q(i,:,k))
 end
 i=hnx:nx;
 x=longitude(convert.*X(i,:,k))./convert;
 pcolor(x,Y(i,:,k),Q(i,:,k))
 if max(max(max(x)))>180./convert
  pcolor(x-360./convert,Y(i,:,k),Q(i,:,k))
 end
end
hold off
