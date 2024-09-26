function [u6t,v6t] = split_UV_cub(u3d,v3d,ksign,kad)
% [u6t,v6t] = split_UV_cub(u3d,v3d,[ksign,kad])
%---------------------------------------------
% Split 2d/3d vector field u,v (on C-grid if kad > 0, on A-grid if kad < 0)
% into 3d x 6 faces and:
%  kad=1: (C-grid) add 1 column to U and one row to V <== at the end !!!
%   => output is u6t(nc+1,nc,[nr],6) & v6t(nc,nc+1,[nr],6)
%  kad=2: add 1 column + 2 rows to U and 2 columns + 1 row to V
%   => output is u6t(nc+1,nc+2,[nr],6) & v6t(nc+2,nc+1,[nr],6)
%  kad=3: add 2 columns + 2 rows to U and V (i.e, 1 on both side)
%   => output is u6t(nc+2,nc+2,[nr],6) & v6t(nc+2,nc+2,[nr],6)
%  kad=-1: assuming input u3d & v3d are on A-grid,
%           add 1 column to U and one row to V <== at the begining !!!
%   => output is u6t(nc+1,nc,[nr],6) & v6t(nc,nc+1,[nr],6)
% ksign=0 : --> no sign change ; ksign=1 : --> change the sign where needed
%           note: ksign used only if kad=2 (or =3)
%----------------------------------------------
% Written by jmc@mit.edu, 2005.

if nargin < 3 , ksign = 0; end
if nargin < 4 , kad = 1; end
if rem(kad,1) ~= 0 | kad < -1 | kad > 3, fprintf('kad= %f => Bad value\n',kad); return; end
plmn = 1 - 2*ksign ;

%------------------------------
 if ~isequal(size(u3d),size(v3d))
   error(['Error in Input-array dimensions: ',...
           'u = ',mat2str(size(u3d)),', ','v = ',mat2str(size(v3d))]);
 end
 dims=size(u3d); nDim=length(dims); n1h=dims(1); n2h=dims(2);
%fprintf(' nDim= %i , dims:',nDim);fprintf(' %i',dims);fprintf('\n');
 if nDim == 2, nr=1; else nr=prod(dims(3:end)); end
 if n1h == 6*n2h, nc=n2h;
   u3d=permute(reshape(u3d,[nc 6 nc nr]),[1 3 4 2]);
   v3d=permute(reshape(v3d,[nc 6 nc nr]),[1 3 4 2]);
 elseif n1h*6 == n2h, nc=n1h;
   u3d=permute(reshape(u3d,[nc nc 6 nr]),[1 2 4 3]);
   v3d=permute(reshape(v3d,[nc nc 6 nr]),[1 2 4 3]);
 else
  error([' input var size: ',int2str(n1h),' x ',int2str(n2h),' does not fit regular cube !']);
 end
 ncp=nc+1 ; n2p=nc+2; %nye=nc+kad;
%--
%=================================================================

if kad == 0,
%- split on to 6 faces with no overlap:
 dims(1)=nc; dims(2)=nc;
 u6t=u3d;
 v6t=v3d;

elseif kad == 1,

 dims(1)=ncp; dims(2)=nc;
%- split on to 6 faces with overlap in i+1 for u and j+1 for v :
 u6t=zeros(ncp,nc,nr,6);
 v6t=zeros(nc,ncp,nr,6);
 u6t([1:nc],:,:,:)=u3d(:,:,:,:);
 v6t(:,[1:nc],:,:)=v3d(:,:,:,:);

  u6t(ncp,[1:nc],:,1)=u3d(1,[1:nc],:,2);
  u6t(ncp,[1:nc],:,3)=u3d(1,[1:nc],:,4);
  u6t(ncp,[1:nc],:,5)=u3d(1,[1:nc],:,6);
  u6t(ncp,[1:nc],:,2)=v3d([nc:-1:1],1,:,4);
  u6t(ncp,[1:nc],:,4)=v3d([nc:-1:1],1,:,6);
  u6t(ncp,[1:nc],:,6)=v3d([nc:-1:1],1,:,2);

  v6t([1:nc],ncp,:,1)=u3d(1,[nc:-1:1],:,3);
  v6t([1:nc],ncp,:,3)=u3d(1,[nc:-1:1],:,5);
  v6t([1:nc],ncp,:,5)=u3d(1,[nc:-1:1],:,1);
  v6t([1:nc],ncp,:,2)=v3d([1:nc],1,:,3);
  v6t([1:nc],ncp,:,4)=v3d([1:nc],1,:,5);
  v6t([1:nc],ncp,:,6)=v3d([1:nc],1,:,1);

elseif kad == 2 | kad == 3,

%- fill for kad=2 case:
 dims(1)=ncp; dims(2)=n2p;
%- split on to 6 faces:
 u6t=zeros(ncp,n2p,nr,6);
 v6t=zeros(n2p,ncp,nr,6);
 u6t([1:nc],[2:ncp],:,:)=u3d(:,:,:,:);
 v6t([2:ncp],[1:nc],:,:)=v3d(:,:,:,:);

%- add overlap in i=nc+1 for u and j=nc+1 for v :
  u6t(ncp,[2:ncp],:,1)=u3d(1,[1:nc],:,2);
  u6t(ncp,[2:ncp],:,3)=u3d(1,[1:nc],:,4);
  u6t(ncp,[2:ncp],:,5)=u3d(1,[1:nc],:,6);
  u6t(ncp,[2:ncp],:,2)=v3d([nc:-1:1],1,:,4);
  u6t(ncp,[2:ncp],:,4)=v3d([nc:-1:1],1,:,6);
  u6t(ncp,[2:ncp],:,6)=v3d([nc:-1:1],1,:,2);

  v6t([2:ncp],ncp,:,1)=u3d(1,[nc:-1:1],:,3);
  v6t([2:ncp],ncp,:,3)=u3d(1,[nc:-1:1],:,5);
  v6t([2:ncp],ncp,:,5)=u3d(1,[nc:-1:1],:,1);
  v6t([2:ncp],ncp,:,2)=v3d([1:nc],1,:,3);
  v6t([2:ncp],ncp,:,4)=v3d([1:nc],1,:,5);
  v6t([2:ncp],ncp,:,6)=v3d([1:nc],1,:,1);

%- add overlap in j=0,nc+1 for u and i=0,nc+1 for v :
  u6t([1:ncp], 1 ,:,1)=u6t([1:ncp],ncp,:,6);
  u6t([1:ncp], 1 ,:,3)=u6t([1:ncp],ncp,:,2);
  u6t([1:ncp], 1 ,:,5)=u6t([1:ncp],ncp,:,4);
  u6t([1:ncp],n2p,:,1)=v6t( 2 ,[ncp:-1:1],:,3)*plmn;
  u6t([1:ncp],n2p,:,3)=v6t( 2 ,[ncp:-1:1],:,5)*plmn;
  u6t([1:ncp],n2p,:,5)=v6t( 2 ,[ncp:-1:1],:,1)*plmn;

  u6t([1:ncp], 1 ,:,2)=v6t(ncp,[ncp:-1:1],:,6)*plmn;
  u6t([1:ncp], 1 ,:,4)=v6t(ncp,[ncp:-1:1],:,2)*plmn;
  u6t([1:ncp], 1 ,:,6)=v6t(ncp,[ncp:-1:1],:,4)*plmn;
  u6t([1:ncp],n2p,:,2)=u6t([1:ncp], 2 ,:,3);
  u6t([1:ncp],n2p,:,4)=u6t([1:ncp], 2 ,:,5);
  u6t([1:ncp],n2p,:,6)=u6t([1:ncp], 2 ,:,1);

  v6t( 1 ,[1:ncp],:,1)=u6t([ncp:-1:1],ncp,:,5)*plmn;
  v6t( 1 ,[1:ncp],:,3)=u6t([ncp:-1:1],ncp,:,1)*plmn;
  v6t( 1 ,[1:ncp],:,5)=u6t([ncp:-1:1],ncp,:,3)*plmn;
  v6t(n2p,[1:ncp],:,1)=v6t( 2 ,[1:ncp],:,2);
  v6t(n2p,[1:ncp],:,3)=v6t( 2 ,[1:ncp],:,4);
  v6t(n2p,[1:ncp],:,5)=v6t( 2 ,[1:ncp],:,6);

  v6t( 1 ,[1:ncp],:,2)=v6t(ncp,[1:ncp],:,1);
  v6t( 1 ,[1:ncp],:,4)=v6t(ncp,[1:ncp],:,3);
  v6t( 1 ,[1:ncp],:,6)=v6t(ncp,[1:ncp],:,5);
  v6t(n2p,[1:ncp],:,2)=u6t([ncp:-1:1], 2 ,:,4)*plmn;
  v6t(n2p,[1:ncp],:,4)=u6t([ncp:-1:1], 2 ,:,6)*plmn;
  v6t(n2p,[1:ncp],:,6)=u6t([ncp:-1:1], 2 ,:,2)*plmn;

 if kad == 3,
%- finish the kad=3 case:
  dims(1)=n2p;
  var=u6t; u6t=zeros(n2p,n2p,nr,6); u6t(2:n2p,:,:,:)=var;
  var=v6t; v6t=zeros(n2p,n2p,nr,6); v6t(:,2:n2p,:,:)=var;
 clear var

  u6t(1,[2:ncp],:,1)=v6t([ncp:-1:2],ncp,:,5);
  u6t(1,[2:ncp],:,3)=v6t([ncp:-1:2],ncp,:,1);
  u6t(1,[2:ncp],:,5)=v6t([ncp:-1:2],ncp,:,3);
  u6t(1,[2:ncp],:,2)=u6t(ncp,[2:ncp],:,1);
  u6t(1,[2:ncp],:,4)=u6t(ncp,[2:ncp],:,3);
  u6t(1,[2:ncp],:,6)=u6t(ncp,[2:ncp],:,5);

  v6t([2:ncp],1,:,1)=v6t([2:ncp],ncp,:,6);
  v6t([2:ncp],1,:,3)=v6t([2:ncp],ncp,:,2);
  v6t([2:ncp],1,:,5)=v6t([2:ncp],ncp,:,4);
  v6t([2:ncp],1,:,2)=u6t(ncp,[ncp:-1:2],:,6);
  v6t([2:ncp],1,:,4)=u6t(ncp,[ncp:-1:2],:,2);
  v6t([2:ncp],1,:,6)=u6t(ncp,[ncp:-1:2],:,4);
 end

elseif kad == -1,

 dims(1)=ncp; dims(2)=nc;
%- split on to 6 faces with overlap in i-1 for u and j-1 for v :
 u6t=zeros(ncp,nc,nr,6);
 v6t=zeros(nc,ncp,nr,6);
 u6t([2:ncp],:,:,:)=u3d(:,:,:,:);
 v6t(:,[2:ncp],:,:)=v3d(:,:,:,:);

  u6t(1,[1:nc],:,1)=v6t([nc:-1:1],ncp,:,5);
  u6t(1,[1:nc],:,3)=v6t([nc:-1:1],ncp,:,1);
  u6t(1,[1:nc],:,5)=v6t([nc:-1:1],ncp,:,3);
  u6t(1,[1:nc],:,2)=u6t(ncp,[1:nc],:,1);
  u6t(1,[1:nc],:,4)=u6t(ncp,[1:nc],:,3);
  u6t(1,[1:nc],:,6)=u6t(ncp,[1:nc],:,5);

  v6t([1:nc],1,:,1)=v6t([1:nc],ncp,:,6);
  v6t([1:nc],1,:,3)=v6t([1:nc],ncp,:,2);
  v6t([1:nc],1,:,5)=v6t([1:nc],ncp,:,4);
  v6t([1:nc],1,:,2)=u6t(ncp,[nc:-1:1],:,6);
  v6t([1:nc],1,:,4)=u6t(ncp,[nc:-1:1],:,2);
  v6t([1:nc],1,:,6)=u6t(ncp,[nc:-1:1],:,4);

end

%- restaure the right shape:
if nDim == 2,
  u6t=squeeze(u6t);
  v6t=squeeze(v6t);
else
  u6t=reshape(u6t,[dims 6]);
  v6t=reshape(v6t,[dims(2) dims(1) dims(3:end) 6]);
end

return
