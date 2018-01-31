function [uE,vN,msk] = rotate_uv2uvEN(u,v,AngleCS,AngleSN,Grid,maskW,maskS)
% [uE,vN,msk] = rotate_uv2uvEN(u,v,AngleCS,AngleSN,[Grid,maskW,maskS])
%
% Rotate cube sphere U and V vector components to east-west (uE) and
% north-south (vN) components located on cube sphere grid centers.
%
% Incoming u and v matricies are assumed to be cube sphere A-grid or C-grid
% vector fields (defaut is C-grid) where the first two dimensions are (6*nc
% nc), where nc is the cube face resolution.  There may up to 4 additional
% dimensions (likely z and time, trials, etc.) beyond this.
% Optional maskW & maskS can be provided (for C-grid vector input case) 
%  and used for cell-center interpolation
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
UVmsk=0; if isequal(Grid,'C') & nargin == 7, UVmsk=1; end

% Verify dimensions are that of cube-sphere
if ~isequal(size(u,1),6.*size(u,2)) | ...
   ~isequal(size(v,1),6.*size(v,2)) | ...
   ~isequal(size(u),size(v))
    error(['Error in CS-dimensions: ',...
           'u = ',mat2str(size(u)),', ',...
           'v = ',mat2str(size(v))]);
end
dim=size(u); nc=dim(2); 
nz=1; if length(dim) > 2, nz=prod(dim(3:end)); end

if UVmsk == 1,
 if length(dim) == 2,
   n3d=1;
   maskW=maskW(:,:,1);
   maskS=maskS(:,:,1);
 else
   n3d=dim(3);
 end 
 dim3d=[dim(1:2) n3d];
 if ~isequal(size(maskW),dim3d),
    error(['Error in MaskW-dimensions: ',...
           'u = ',mat2str(size(u)),', ',...
           'maskW = ',mat2str(size(maskW))]);
 end
 if ~isequal(size(maskS),dim3d),
    error(['Error in MaskS-dimensions: ',...
           'v = ',mat2str(size(v)),', ',...
           'maskS = ',mat2str(size(maskS))]);
 end
else
 dim3d=dim;
end

%- apply mask (if provided)
if UVmsk == 1,
  if n3d == nz,
   nt=1;
   u=u.*maskW;
   v=v.*maskS;
  else
   nt=dim(4);
   u=reshape(u,[6*nc*nc*n3d nt]);
   v=reshape(v,[6*nc*nc*n3d nt]);
   maskW=reshape(maskW,[6*nc*nc*n3d 1]);
   maskS=reshape(maskS,[6*nc*nc*n3d 1]);
   u=u.*(maskW*ones(1,nt));
   v=v.*(maskS*ones(1,nt));
  end
end
% Parse dimension information, flatten extra dimensions. 
u=reshape(u,[6*nc nc nz]);
v=reshape(v,[6*nc nc nz]);

% Do simple average to put u,v at the cell center (A-grid) as needed.
if isequal(Grid,'A')
    msk=ones(dim3d);
    u=reshape(u,[6*nc*nc nz]);
    v=reshape(v,[6*nc*nc nz]);
elseif isequal(Grid,'C')
    [uu,vv] = split_UV_cub(u,v,1);
    uu=reshape(uu,[nc+1 nc nz 6]);
    vv=reshape(vv,[nc nc+1 nz 6]);
    if UVmsk == 1,
      maskW=reshape(maskW,[6*nc nc n3d]);
      maskS=reshape(maskS,[6*nc nc n3d]);
      [mu,mv] = split_UV_cub(maskW,maskS,0);
      mu=reshape(mu,[nc+1 nc n3d 6]);
      mv=reshape(mv,[nc nc+1 n3d 6]);
      um=(mu(1:nc,:,:,:)+mu(2:nc+1,:,:,:))/2;
      vm=(mv(:,1:nc,:,:)+mv(:,2:nc+1,:,:))/2;
      msk=(um+vm)/2;
      msk = reshape(msk,dim3d);
%-----
      u=(uu(1:nc,:,:,:)+uu(2:nc+1,:,:,:))/2;
      v=(vv(:,1:nc,:,:)+vv(:,2:nc+1,:,:))/2;
      u=reshape(permute(u,[1 4 2 3]),[6*nc*nc*n3d nt]);
      v=reshape(permute(v,[1 4 2 3]),[6*nc*nc*n3d nt]);
      um=reshape(permute(um,[1 4 2 3]),[6*nc*nc*n3d 1]);
      vm=reshape(permute(vm,[1 4 2 3]),[6*nc*nc*n3d 1]);
      um(find(um==0))=1;
      vm(find(vm==0))=1;
      um=1./um;
      vm=1./vm;
      u=u.*(um*ones(1,nt));
      v=v.*(vm*ones(1,nt));
      u=reshape(u,[6*nc*nc nz]);
      v=reshape(v,[6*nc*nc nz]);
    else
      msk=ones(dim);
      u=(uu(1:nc,:,:,:)+uu(2:nc+1,:,:,:))/2;
      v=(vv(:,1:nc,:,:)+vv(:,2:nc+1,:,:))/2;
      u=reshape(permute(u,[1 4 2 3]),[6*nc*nc nz]);
      v=reshape(permute(v,[1 4 2 3]),[6*nc*nc nz]);
    end
else
    error(['Unrecognized grid type:  ',Grid]);
end

% Make rotation to find uE, vN.
uE=NaN.*zeros(6*nc*nc,nz);
vN=NaN.*zeros(6*nc*nc,nz);
for k=1:nz;
    uE(:,k)=AngleCS(:).*u(:,k)-AngleSN(:).*v(:,k);
    vN(:,k)=AngleSN(:).*u(:,k)+AngleCS(:).*v(:,k);
end
uE = reshape(uE,dim);
vN = reshape(vN,dim);
return
