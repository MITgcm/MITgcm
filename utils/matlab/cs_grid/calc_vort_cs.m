function [vort,z6t]=calc_vort_cs(u3d,v3d,dxC,dyC,rAz);
% [vort,z6t]=calc_vort_cs(u3d,v3d,dxC,dyC,rAz);
% compute vorticity (3rd component) on CS-grid.
%  assume all input (except rAz) have same shape, either original format (nc*6,nc,*)
%    or compact format (nc,nc*6,*); and rAz can either have same shape as others
%    or can include 2 missing corner in a long vector shape (nc*nc*6+2)
%  output is provided with 2 shapes:
%   1) vort(nc*nc*6+2,*) = long-vector (compact) form;
%   2) z6t(nc+1,nc+1,*,6) = face splitted.
%
% Written by jmc@mit.edu, 2005.

dims=size(u3d);
 n1h=dims(1); n2h=dims(2);
%-- Verify input array dimensions
 if n1h == n2h*6, nc=n2h; elseif n1h*6 == n2h, nc=n1h;
 else
    error(['Error in CS-dimensions: ',...
           'n1h,n2h = ',int2str(n1h),', ',int2str(n2h)])
 end
 ncp=nc+1; n2p=nc+2; nPg=nc*nc*6 ;
 if ~isequal(size(u3d),size(v3d)) | ...
    ~isequal(size(dxC),[n1h n2h]) | ...
    ~isequal(size(dyC),[n1h n2h])
    error(['Error in Input-array dimensions: ',...
           'u3d = ',mat2str(size(u3d)),', ',...
           'v3d = ',mat2str(size(v3d)),', ',...
           'dxC = ',mat2str(size(dxC)),', ',...
           'dyC = ',mat2str(size(dyC))]);
 end
 if ~isequal(size(rAz),[n1h n2h]) & ~isequal(size(rAz),[nPg+2 1]),
    error(['Error in rAz array dimensions: ',mat2str(size(rAz))]);
 end

if length(dims) == 2, nr=1; else nr=prod(dims(3:end)); end

siZ=prod(size(rAz));
if size(rAz,1) == nPg+2,
 rAz=reshape(rAz,[nPg+2 1]);
 aZ=split_Z_cub(rAz);
else
 if n2h == nc,
   rAz=permute(reshape(rAz,[nc 6 nc]),[1 3 2]);
 end
 rAz=reshape(rAz,[nPg 1]);
 aZc=zeros(nPg+2,1); aZc(1:nPg,:)=rAz; aZc(nPg+1)=rAz(1); aZc(nPg+2)=rAz(1);
 aZ=split_Z_cub(aZc);
end

u3d=reshape(u3d,[n1h n2h nr]);
v3d=reshape(v3d,[n1h n2h nr]);

[u6t,v6t]=split_UV_cub(u3d,v3d,1,2);
[d6x,d6y]=split_UV_cub(dxC,dyC,0,2);
if nr > 1,
  u6t=permute(u6t,[1 2 4 3]);
  v6t=permute(v6t,[1 2 4 3]);
end
z6t=zeros(ncp,ncp,6,nr);

for k=1:nr,
 vv1=d6x.*u6t(:,:,:,k);
 vv2=d6y.*v6t(:,:,:,k);
 z6t(:,:,:,k)=vv2([2:n2p],:,:)-vv2([1:ncp],:,:);
 z6t(:,:,:,k)=z6t(:,:,:,k)-(vv1(:,[2:n2p],:)-vv1(:,[1:ncp],:));
%- corner: the quick way:
% z6t(1,1,:,k)   = vv2(2,1,:)  -vv2(1,1,:)  -vv1(1,2,:);
% z6t(1,ncp,:,k) = vv2(2,ncp,:)-vv2(1,ncp,:)+vv1(1,ncp,:);
% z6t(ncp,1,:,k) = vv2(n2p,1,:)-vv2(ncp,1,:)-vv1(ncp,2,:);
% z6t(ncp,ncp,:,k)=vv2(n2p,ncp,:)-vv2(ncp,ncp,:)+vv1(ncp,ncp,:);
%- corner: add the 3 terms always in the same order
%   to get the same truncation on the 3 faces
 for n=1:3,
   f=2*n-1; %- odd face number
   vc=-vv2(1,1,f); %- S-W corner
   z6t(1,1,f,k)   = (vv2(2,1,f)-vv1(1,2,f))+vc;
   vc=+vv2(n2p,1,f); %- S-E corner
   z6t(ncp,1,f,k) = (vc-vv1(ncp,2,f))-vv2(ncp,1,f);
   vc=+vv2(n2p,ncp,f); %- N-E corner
   z6t(ncp,ncp,f,k)=(vc-vv2(ncp,ncp,f))+vv1(ncp,ncp,f);
   vc=-vv2(1,ncp,f); %- N-W corner
   vc3=[vc vv2(2,ncp,f) vv1(1,ncp,f) vc vv2(2,ncp,f)];
   z6t(1,ncp,f,k) = (vc3(n+2)+vc3(n+1))+vc3(n);
   f=2*n; %- even face number
   vc=-vv2(1,1,f); %- S-W corner
   z6t(1,1,f,k)   = (vv2(2,1,f)-vv1(1,2,f))+vc;
   vc=+vv2(n2p,1,f); %- S-E corner
   vc3=[-vv1(ncp,2,f) -vv2(ncp,1,f) vc -vv1(ncp,2,f) -vv2(ncp,1,f)];
   z6t(ncp,1,f,k) = (vc3(n)+vc3(n+1))+vc3(n+2);
   vc=+vv2(n2p,ncp,f); %- N-E corner
   z6t(ncp,ncp,f,k)=(vv1(ncp,ncp,f)+vc)-vv2(ncp,ncp,f);
   vc=-vv2(1,ncp,f); %- N-W corner
   z6t(1,ncp,f,k) = (vv2(2,ncp,f)+vc)+vv1(1,ncp,f);
 end
%- divide by rAz:
 z6t(:,:,:,k)=z6t(:,:,:,k)./aZ;
end

%- put in compressed form:
vort=zeros(nPg+2,nr);
%  extract the interior
 vort([1:nPg],:)=reshape(z6t(1:nc,1:nc,:,:),[nPg nr]);
%  put back the 2 missing corners (N.W corner of 1rst face & S.E corner of 2nd face)
 vort(nPg+1,:)=z6t(1,ncp,1,:);
 vort(nPg+2,:)=z6t(ncp,1,2,:);

%- back into final shape:
z6t=permute(z6t,[1 2 4 3]);
if length(dims) == 2,
 vort=squeeze(vort);
 z6t=squeeze(z6t);
else
 vort=reshape(vort,[nPg+2 dims(3:end)]);
 z6t=reshape(z6t,[ncp ncp dims(3:end) 6]);
end

%-----------------
return
