function [cos_alpha, sin_alpha, u_a, v_a] = calcFacetAngles(yG,dxG,dyG)
% [cos_alpha, sin_alpha, u_a, v_a] = calcFacetAngles
%
% Find the rotation angle that projects cell centered velocity on
% locally orthogonal mesh1 to cell centered velocity on locally orthogonal 
% mesh2.
% yG is location in meridional mesh2 coordinate of mesh1 points.
% dxG is spacing between mesh1 points along local x coordinate line of mesh1.
% dyG is spacing between mesh2 points along local y coordinate line of mesh1.
% Indexing is assumed to be standard MITgcm (western most, southern most) 
% indexing.
% yG is defined at cell corners.
% dxG is along cell southern most face.
% dyG is along cell western most face.
% x-coordinate is dimension 1 (the left most dimension)
% y-coordinate is dimension 2 (the right most dimension)
% cos_alpha, sin_alpha, u_a and v_a are at cell centers.
% u_a and v_a are returned for testing, they are not really needed.

% Define a stream function that varies along the meridional coordinate of 
% mesh2 only.
  psi= (yG);
% Calculate local gradients of psi and the cell centered averages of gradients
% on mesh1.
  v  =-(psi(2:end,:)-psi(1:end-1,:))./dxG(:,:);
  u  = (psi(:,2:end)-psi(:,1:end-1))./dyG(:,:);
  u_a=0.5*(u(1:end-1,:)+u(2:end,:));
  v_a=0.5*(v(:,1:end-1)+v(:,2:end));
  speed = ( u_a.^2 + v_a.^2 ).^0.5;
% Using v_mesh2   = cos_alpha.v_a + sin_alpha.u_a
%       u_mesh2   = cos_alpha.u_a - sin_alpha.v_a
%  and  u_mesh2^2 = u_a^2 + v_a^2
% we get
  sin_alpha = -v_a./speed;
  cos_alpha =  u_a./speed;
return
end
