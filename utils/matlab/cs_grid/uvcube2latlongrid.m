function [U,V] = uvcube2latlongrid(del,u,v,XG,YG,RAC,dxG,dyG)
% [ui,vi]=cube2latlongrid(del,u,v,xg,yg,rac,dxg,dyg)
%
% Re-grids model output on expanded spherical cube to lat-lon grid.
%  del     pre-processed transformation data
%  u,v     is a 2-D or 3-D horizontal components of model flow fields.
%  xg,yg   coordinates of cell corners
%  rac     grid cell areas
%  dxg,dyg distances between cell corners
%  ui,vi are the flow fields with dimensions of size(xi) x size(yi) size(u,3).
%
% e.g.
%
% Getting the transfer matrix:
% >> xc=rdmds('XC');
% >> yc=rdmds('YC');
% >> xi=-179:2:180;yi=-89:2:90;
% >> del=cube2latlon_preprocess(xc,yc,xi,yi);
%
% Moving u,v:
% >> XG=rdmds('XG');
% >> YG=rdmds('YG');
% >> RAC=rdmds('RAC');
% >> dxG=rdmds('dxG');
% >> dyG=rdmds('dyG');
% >> u=rdmds('uVeltave.0000513360');
% >> v=rdmds('vVeltave.0000513360');
% >> [ui,vi]=uvcube2latlongrid(del,u,v,XG,YG,RAC,dxG,dyG);

% Written by gmaze@mit.edu, 2007
[nnx ny nz]=size(u);

for iz = 1 : nz
  
% Get the angles:
[AngleCS,AngleSN] = cubeCalcAngle(YG,RAC,dxG,dyG);

% Rotate vectors:
[uE,vN] = rotate_uv2uvEN(squeeze(u(:,:,iz)),squeeze(v(:,:,iz)),AngleCS,AngleSN,'C');

% Move to lat/lon:
U(:,:,iz) = cube2latlon_fast(del,uE);
V(:,:,iz) = cube2latlon_fast(del,vN);

end %or iz
