function [uE,vN] = cubeuv2uvEN_C(u,v,AngleCS,AngleSN,XG,YG)
% [uE,vN] = cubeuv2uvEN_C(u,v,AngleCS,AngleSN,XG,YG)
%
% Rotate cube sphere C-grid U and V vector components of to east-west (uE),
% north-south (vN) components locateds and cube sphere grid centers.
%
% Incoming u and v matricies are assumed to be cube sphere C-grid vector
% fields where the first two dimensions are (6*nc nc), where nc is the cube
% face resolution.  There may up to 4 additional dimensions (likely z and
% time, trials, etc.) beyond this.
%
% e.g.
%
% >> u=rdmds('uVeltave.0000513360');
% >> v=rdmds('vVeltave.0000513360');
% >> AngleCS=rdmds('AngleCS');
% >> AngleSN=rdmds('AngleSN');
% >> XG=rdmds('XG');
% >> YG=rdmds('YG');
% >> [uE,vN] = cubeuv2uvEN_C(u,v,AngleCS,AngleSN,XG,YG);

if ~isequal(size(u,1),6.*size(u,2)) | ...
   ~isequal(size(v,1),6.*size(v,2))
    error(['Error in CS-dimensions: ',...
           'u = ',mat2str(size(u)),', ',...
           'v = ',mat2str(size(v))]);
end

% Parse dimension information, flatted extra dimensions. 
dim=size(u); nc=dim(2); nz=prod(dim(3:end));
u=reshape(u,[6*nc nc nz]);
v=reshape(v,[6*nc nc nz]);

% Do simple average to put u,v at the cell center (A-grid).
[uu,vv] = split_UV_cub(u,v);
uu=reshape(uu,[nc+1 nc nz 6]);
vv=reshape(vv,[nc nc+1 nz 6]);
u=(uu(1:nc,:,:,:)+uu(2:nc+1,:,:,:))/2;
v=(vv(:,1:nc,:,:)+vv(:,2:nc+1,:,:))/2;
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