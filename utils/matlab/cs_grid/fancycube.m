function [] = fancycube(XX,YY,C,H,r)
% fancycube(x,y,c,h)
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
% h=rdmds('ETOPO5');
% fancycube(xg,yg,ps,h,0.05/5000);
%
% Written by adcroft@.mit.edu, 2005.
if max(max(max(YY)))-min(min(min(YY))) < 3*pi
 X=tiles(XX*180/pi,1:6);
 Y=tiles(YY*180/pi,1:6);
else
 X=tiles(XX,1:6);
 Y=tiles(YY,1:6);
end
Q=(tiles(C,1:6));
Qmin=min(Q(:)); Qmax=max(Q(:)); Q=(Q-Qmin)/(Qmax-Qmin)-1;
q=(tiles(H,1:6));
q( find(~isnan(Q)) )=NaN;
%q=min(q,5000);
%q=max(q,0);
qmin=min(q(:)); qmax=max(q(:)); q=(q-qmin)/(qmax-qmin);
Q( find(isnan(Q)) )=q( find(isnan(Q)) );

% Assume model grid corner coordinates were provided.
if size(X,1)==size(Q,1)
 X(end+1,:,:)=NaN;
 X(:,end+1,:)=NaN;
 X(end,:,[1 3 5])=X(1,:,[2 4 6]);
 X(:,end,[2 4 6])=X(:,1,[3 5 1]);
 X(:,end,[1 3 5])=squeeze(X(1,end:-1:1,[3 5 1]));
 X(end,:,[2 4 6])=squeeze(X(end:-1:1,1,[4 6 2]));
 Y(end+1,:,:)=NaN;
 Y(:,end+1,:)=NaN;
 Y(end,:,[1 3 5])=Y(1,:,[2 4 6]);
 Y(:,end,[2 4 6])=Y(:,1,[3 5 1]);
 Y(:,end,[1 3 5])=squeeze(Y(1,end:-1:1,[3 5 1]));
 Y(end,:,[2 4 6])=squeeze(Y(end:-1:1,1,[4 6 2]));
end
[nx ny nt]=size(X);

H=tiles(H,1:6); H(end+1,:,:)=H(end,:,:);H(:,end+1,:)=H(:,end,:);
R=ones(size(X))+r*H;

z=R.*sin(Y*pi/180);
x=R.*cos(Y*pi/180).*cos(X*pi/180-pi/2);
y=R.*cos(Y*pi/180).*sin(X*pi/180-pi/2);

surf(x(:,:,1),y(:,:,1),z(:,:,1),Q(:,:,1))
hold on
for j=2:6
 surf(x(:,:,j),y(:,:,j),z(:,:,j),Q(:,:,j))
end
hold off
xlabel('X');
ylabel('Y');
zlabel('Z');

% Remake colormap
if size(colormap,1)==64
 CM=colormap;
 cm=cm_landwater(64,0);
 colormap( [CM' cm']' )
end

shading flat
axis([-1 1 -1 1 -1 1]*max(R(:)))
axis square
axis vis3d
pos=get(gcf,'pos');set(gcf,'pos',[pos(1:2) [1 1]*480])	% Movie size (+ screen)
set(gca,'pos',[0 0 1 1]+.25*[-1 -1 2 2]);		% Fill window
set(gcf,'paperposition',[0 0 4 4])			% Square printing
set(gca,'projection','perspective')


