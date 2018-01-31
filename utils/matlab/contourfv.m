function [hc,hh,hcf] = contourfv(x,z,h,a,varargin)
% CONTOURFV(X,Z,H,A)
%
% Is the same as CONTOURF except it automatically extends the dataset
% to the depth of the topography "H". This is srictly for plotting
% vertical (x,z) sections in conjuction with "partial cells" in the MITgcm.
%
% X is horizontal coordinate (vector)
% Z is vertical coordinate (vector)
% H is depth of bottom (vector)
% A is field to be plotted (matrix)
%
% Optional arguments are passed on to CONTOURF

nx=prod(size(x));
nz=prod(size(z));
%max(size(h))
%prod(size(h))
if max(size(h)) ~= prod(size(h)) | prod(size(h)) ~= nx
 error('H must have dimensions of size(X)');
end
%ndims(squeeze(a))
%size(squeeze(a))
if ndims(squeeze(a)) ~= 2 | sum(size(squeeze(a)) ~= [nx nz])
 error('A must have dimensions of size(X) x size(H)');
end

% Create extended Z coordinate
zz=zeros(1,nz+2);
zz(2:nz+1)=z;
zz(1)=max(0,z(1));
zz(nz+2)=1.5*z(nz)-0.5*z(nz-1);

[Z,X]=meshgrid(zz,x);

aa=squeeze(a);
au=aa(:,[1 1:nz-1]);
aa(isnan(aa))=au(isnan(aa));
A=zeros(nx,nz+2);
A(:,2:nz+1)=aa;
A(:,1)=aa(:,1);
A(:,nz+2)=aa(:,nz);
clear aa au

[Hz,Hx]=meshgrid(zz,h);
clear Hz


% H might be positive...
if min(z)*min(h) > 0
 Z=max(Z,Hx);
else
 Z=max(Z,-Hx);
end
clear Hx

[hc hh hcf]=contourf(X,Z,A,varargin{:});
