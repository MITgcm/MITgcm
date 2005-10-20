function [uE,vN] = rotate_uv2uvEN(u,v,AngleCS,AngleSN,Grid)
% [uE,vN] = rotate_uv2uvEN(u,v,AngleCS,AngleSN.Grid)
%
% Rotate cube sphere U and V vector components to east-west (uE) and
% north-south (vN) components located on cube sphere grid centers.
%
% Incoming u and v matricies are assumed to be cube sphere A-grid or C-grid
% vector fields (defaut is C-grid) where the first two dimensions are (6*nc
% nc), where nc is the cube face resolution.  There may up to 4 additional
% dimensions (likely z and time, trials, etc.) beyond this.
%
% e.g.
%
% >> uC=rdmds('uVeltave.0000513360');
% >> vC=rdmds('vVeltave.0000513360');
% >> AngleCS=rdmds('AngleCS');
% >> AngleSN=rdmds('AngleSN');
% >> [uE,vN] = rotate_uv2uvEN(uC,vC,AngleCS,AngleSN);
%
% >> uA=rdmds('uVeltaveA.0000513360');
% >> vA=rdmds('vVeltaveA.0000513360');
% >> AngleCS=rdmds('AngleCS');
% >> AngleSN=rdmds('AngleSN');
% >> [uE,vN] = rotate_uv2uvEN(uA,vA,AngleCS,AngleSN,'A');

% Default is a C-grid configuration.
if nargin == 4, Grid = 'C'; end

% Verify dimensions are that of cube-sphere
if ~isequal(size(u,1),6.*size(u,2)) | ...
   ~isequal(size(v,1),6.*size(v,2))
    error(['Error in CS-dimensions: ',...
           'u = ',mat2str(size(u)),', ',...
           'v = ',mat2str(size(v))]);
end

% Parse dimension information, flatten extra dimensions. 
dim=size(u); nc=dim(2); nz=prod(dim(3:end));
u=reshape(u,[6*nc nc nz]);
v=reshape(v,[6*nc nc nz]);

% Do simple average to put u,v at the cell center (A-grid) as needed.
[uu,vv] = split_UV_cub(u,v);
if isequal(Grid,'A')
    u = uu(1:nc,1:nc,1:nz,1:6);
    v = vv(1:nc,1:nc,1:nz,1:6);
elseif isequal(Grid,'C')
    uu=reshape(uu,[nc+1 nc nz 6]);
    vv=reshape(vv,[nc nc+1 nz 6]);
    u=(uu(1:nc,:,:,:)+uu(2:nc+1,:,:,:))/2;
    v=(vv(:,1:nc,:,:)+vv(:,2:nc+1,:,:))/2;
else
    error(['Unrecognized grid type:  ',Grid]);
end
u=reshape(permute(u,[1 4 2 3]),[6*nc*nc nz]);
v=reshape(permute(v,[1 4 2 3]),[6*nc*nc nz]);

% Make rotation to find uE, vN.
uE=NaN.*zeros(6*nc*nc,nz);
vN=NaN.*zeros(6*nc*nc,nz);
for k=1:nz;
    uE(:,k)=AngleCS(:).*u(:,k)-AngleSN(:).*v(:,k);
    vN(:,k)=AngleSN(:).*u(:,k)+AngleCS(:).*v(:,k);
end
uE = reshape(uE,dim);
vN = reshape(vN,dim);