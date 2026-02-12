function [uE,vN,msk] = rotate_uv2uvEN(u,v,AngleCS,AngleSN,Grid,maskW,maskS)
% [uE,vN,msk] = rotate_uv2uvEN(u,v,AngleCS,AngleSN,[Grid,maskW,maskS])
%
% Rotate cube sphere U and V vector components to east-west (uE) and
% north-south (vN) components located on cube sphere grid centers.
%
% Assume all input arrays have same shape, either original format (nc*6,nc,*)
%  or compact format (nc,nc*6,*), where nc is the cube face resolution.
% Incoming u and v matricies are cube sphere A-grid (Grid='A') or C-grid vector
%  fields (defaut, Grid='C'). There may be up 2 more dimensions (likely z and
%  time) for a total of 4 dimensions.
% Output uE,vN is returned in the shape shape (with same dimensions).
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

n1h=size(u,1); n2h=size(u,2);
% Verify dimensions are that of cube-sphere
 if ~isequal(size(u),size(v)) | ...
    ~isequal(size(AngleCS),[n1h n2h]) | ...
    ~isequal(size(AngleSN),[n1h n2h])
    error(['Error in Input-array dimensions: ',...
           'u = ',mat2str(size(u)),', ',...
           'v = ',mat2str(size(v)),', ',...
           'AnCS = ',mat2str(size(AngleCS)),', ',...
           'AnSN = ',mat2str(size(AngleSN))]);
 end
 if n1h == n2h*6, nc=n2h; elseif n1h*6 == n2h, nc=n1h;
 else
    error(['Error in CS-dimensions: ',...
           'n1h,n2h = ',int2str(n1h),', ',int2str(n2h)])
 end
dim=size(u); ncp=nc+1;
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
   u=reshape(u,[n1h*n2h*n3d nt]);
   v=reshape(v,[n1h*n2h*n3d nt]);
   maskW=reshape(maskW,[n1h*n2h*n3d 1]);
   maskS=reshape(maskS,[n1h*n2h*n3d 1]);
   u=u.*(maskW*ones(1,nt));
   v=v.*(maskS*ones(1,nt));
  end
end
% Parse dimension information, flatten extra dimensions.
u=reshape(u,[n1h n2h nz]);
v=reshape(v,[n1h n2h nz]);

% Do simple average to put u,v at the cell center (A-grid) as needed.
if isequal(Grid,'A')
    msk=ones(dim3d);
    u=reshape(u,[n1h*n2h nz]);
    v=reshape(v,[n1h*n2h nz]);
elseif isequal(Grid,'C')
    [uu,vv] = split_UV_cub(u,v,1);
    uu=reshape(uu,[ncp nc nz 6]);
    vv=reshape(vv,[nc ncp nz 6]);
    if UVmsk == 1,
      maskW=reshape(maskW,[n1h n2h n3d]);
      maskS=reshape(maskS,[n1h n2h n3d]);
      [mu,mv] = split_UV_cub(maskW,maskS,0);
      mu=reshape(mu,[ncp nc n3d 6]);
      mv=reshape(mv,[nc ncp n3d 6]);
      um=(mu(1:nc,:,:,:)+mu(2:ncp,:,:,:))/2;
      vm=(mv(:,1:nc,:,:)+mv(:,2:ncp,:,:))/2;
      msk=(um+vm)/2;
      msk = reshape(msk,dim3d);
%-----
      u=(uu(1:nc,:,:,:)+uu(2:ncp,:,:,:))/2;
      v=(vv(:,1:nc,:,:)+vv(:,2:ncp,:,:))/2;
      u=reshape(permute(u,[1 2 4 3]),[nc*nc*6*n3d nt]);
      v=reshape(permute(v,[1 2 4 3]),[nc*nc*6*n3d nt]);
      um=reshape(permute(um,[1 2 4 3]),[nc*nc*6*n3d 1]);
      vm=reshape(permute(vm,[1 2 4 3]),[nc*nc*6*n3d 1]);
      um(find(um==0))=1; um=1./um;
      vm(find(vm==0))=1; vm=1./vm;
      u=u.*(um*ones(1,nt));
      v=v.*(vm*ones(1,nt));
      if n2h == nc,
       u=permute(reshape(u,[nc nc 6 nz]),[1 3 2 4]);
       v=permute(reshape(v,[nc nc 6 nz]),[1 3 2 4]);
      end
    else
      msk=ones(dim);
      u=(uu(1:nc,:,:,:)+uu(2:ncp,:,:,:))/2;
      v=(vv(:,1:nc,:,:)+vv(:,2:ncp,:,:))/2;
      if n1h == nc,
       u=permute(u,[1 2 4 3]);
       v=permute(v,[1 2 4 3]);
      else
       u=permute(u,[1 4 2 3]);
       v=permute(v,[1 4 2 3]);
      end
    end
    u=reshape(u,[n1h*n2h nz]);
    v=reshape(v,[n1h*n2h nz]);
else
    error(['Unrecognized grid type:  ',Grid]);
end

% Make rotation to find uE, vN.
uE=NaN.*zeros(n1h*n2h,nz);
vN=NaN.*zeros(n1h*n2h,nz);
for k=1:nz;
    uE(:,k)=AngleCS(:).*u(:,k)-AngleSN(:).*v(:,k);
    vN(:,k)=AngleSN(:).*u(:,k)+AngleCS(:).*v(:,k);
end
uE = reshape(uE,dim);
vN = reshape(vN,dim);
return
