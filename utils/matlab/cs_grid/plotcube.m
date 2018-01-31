function [] = plotcube(XX,YY,C)
% plotcube(x,y,c)
%
% Plots cubed-sphere data in 3D on sphere. (x,y) are
% coordinates, c is cell-centered scalar to be plotted.
% Dimensions should be N+1 x N+1 x 6 for (x,y)
% and                   N  x  N  x 6 for c
%
% The default plotting mode is shading faceted. Using this or
% shading flat, (x,y) should be the coordinates of grid-corners
% and can legitimately have dimension (N+1)x(N+1)x6.
%
% If using shading interp, then (x,y) must be the coordinates of
% the cell centers with same dimensions as c.
%
% e.g.
%
% xg=rdmds('XG');
% yg=rdmds('YG');
% ps=rdmds('Eta.0000000000');
% plotube(xg,yg,ps);
%
% xc=rdmds('XC');
% yc=rdmds('YC');
% plotube(xg,yg,ps);shading interp
%
% Written by adcroft@.mit.edu, 2001.
if max(max(max(YY)))-min(min(min(YY))) < 3*pi
 X=tiles(XX*180/pi,1:6);
 Y=tiles(YY*180/pi,1:6);
else
 X=tiles(XX,1:6);
 Y=tiles(YY,1:6);
end
Q=tiles(C,1:6);

% Assume model grid corner coordinates were provided.
if size(X,1)==size(Q,1)
 X(end+1,:,:)=NaN;
 X(:,end+1,:)=NaN;
 X(end,:,[1 3 5])=X(1,:,[2 4 6]);
 X(:,end,[2 4 6])=X(:,1,[3 5 1]);
 X(:,end,[1 3 5])=squeeze(X(1,end:-1:1,[3 5 1]));
 X(end,:,[2 4 6])=squeeze(X(end:-1:1,1,[4 6 2]));
 X(1,end,[1 3 5]) = X(1,1,1);
 X(end,1,[2 4 6]) = X(end,end,2);
 Y(end+1,:,:)=NaN;
 Y(:,end+1,:)=NaN;
 Y(end,:,[1 3 5])=Y(1,:,[2 4 6]);
 Y(:,end,[2 4 6])=Y(:,1,[3 5 1]);
 Y(:,end,[1 3 5])=squeeze(Y(1,end:-1:1,[3 5 1]));
 Y(end,:,[2 4 6])=squeeze(Y(end:-1:1,1,[4 6 2]));
 Y(1,end,[1 3 5]) = Y(end,end,1);
 Y(end,1,[2 4 6]) = Y(1,1,2);
end
[nx ny nt]=size(X);

z=sin(Y*pi/180);
x=cos(Y*pi/180).*cos(X*pi/180);
y=cos(Y*pi/180).*sin(X*pi/180);

surf(x(:,:,1),y(:,:,1),z(:,:,1),Q(:,:,1))
hold on
for j=2:6
 surf(x(:,:,j),y(:,:,j),z(:,:,j),Q(:,:,j))
end
hold off
xlabel('X');
ylabel('Y');
zlabel('Z');
