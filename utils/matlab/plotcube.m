function [] = plotcube(X,Y,Z,C)
% e.g.
% load CUBE_3DGRID
% tt=rdmds('TTtave.0000103680');
% plotcube(XG,YG,ZG,tt(:,:,1))
%
% $Header: %
% $Name: %

n=size(C,2);

surf(X(:,:,1),Y(:,:,1),Z(:,:,1),C(1:n,:))
hold on
for j=2:6
 surf(X(:,:,j),Y(:,:,j),Z(:,:,j),C((j-1)*n+1:j*n,:))
end
hold off
xlabel('X');
ylabel('Y');
zlabel('Z');
