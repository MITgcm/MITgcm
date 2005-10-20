function [AngleCS,AngleSN] = cubeCalcAngle(YG,RAC,dxG,dyG);
% [AngleCS,AngleSN] = cubeCalcAngle(YG,RAC,dxG,dyG);
%
% Determine rotation angle, alpha (returned as cos(alpha) and sin(alpha)),
% to convert cube sphere C-grid vectors to east-west and north-south
% components.
%
% e.g.
%
% >> XG=rdmds('XG');
% >> YG=rdmds('YG');
% >> RAC=rdmds('RAC');
% >> dxG=rdmds('dxG');
% >> dyG=rdmds('dyG');
% >> [AngleCS,AngleSN] = cubeCalcAngle(YG,RAC,dxG,dyG);
%
% >> u=rdmds('uVeltave.0000513360');
% >> v=rdmds('vVeltave.0000513360');
% >> [uE,vN] = rotate_csCg_EN(u,v,AngleCS,AngleSN,XG,YG);

nc=size(RAC,2);
deg2rad=pi/180;
r_earth=sqrt(sum(sum(RAC))./(4.*pi));

[yZ6]=split_Z_cub(YG);

% Build purely zonal flow from a stream function, psi = r_earth * latitude.
psi=-r_earth.*yZ6.*deg2rad;
uZ=psi([1:nc],[1:nc],:)-psi([1:nc],[2:nc+1],:);
vZ=psi([2:nc+1],[1:nc],:)-psi([1:nc],[1:nc],:);
uZ=reshape(permute(uZ,[1 3 2]),[6.*nc nc])./dyG;
vZ=reshape(permute(vZ,[1 3 2]),[6.*nc nc])./dxG;

% Put constructed zonal wind at cell center.
[uu,vv]=split_UV_cub(uZ,vZ);
u=(uu(1:nc,:,:)+uu(2:nc+1,:,:))/2;
v=(vv(:,1:nc,:)+vv(:,2:nc+1,:))/2;
uZc=reshape(permute(u,[1 3 2]),[6.*nc nc]);
vZc=reshape(permute(v,[1 3 2]),[6.*nc nc]);

% Calculate angles.
norm=sqrt(uZc.*uZc+vZc.*vZc);
AngleCS =  uZc./norm;
AngleSN = -vZc./norm;
