function [z6t] = split_Z_cub(z3d)
% [z6t] = split_Z_cub(z3d)
%---------------------------------------------
% split 2d/3d arrays z3d to 2d/3d x 6 faces
% and add 1 column + 1 row
%  => output is z6t(nc+1,nc+1,[nr],6)
% either input is z3d(nc*6*nc+2,*): includes the 2 missing corners
%   or   input is z3d(nc*6,nc,*): => use the average value (from the 3
%                                 neighbours) for the 2 missing corners
%----------------------------------------------
% Written by jmc@ocean.mit.edu, 2005.
dims=size(z3d); nDim=length(dims);
%fprintf(' nDim= %i , dims:',nDim);fprintf(' %i',dims);fprintf('\n');

nc=fix((dims(1)-2)/6); nc=fix(sqrt(abs(nc)));
if dims(1) == nc*nc*6+2,
 nr=dims(2); if nDim > 2, nr=prod(dims(2:end)); end
 nx=6*nc ; nPg=nx*nc; nPts=nPg+2; dims=[nPts 1 dims(2:end)];
 z3d=reshape(z3d,[nPts nr]);
 zzC=z3d(nPg+1:nPg+2,:); z3d=z3d(1:nPg,:);
elseif dims(1) == 6*dims(2),
 if nDim == 2, nr=1; else nr=prod(dims(3:end)); end
 nc=dims(2); nx=6*nc ; nPg=nx*nc; nPts=nPg+2;
 zzC=zeros(2,nr);
else
 fprintf(' Error in split_Z_cub: bad input dimensions :');
 fprintf(' %i',dims); fprintf('\n');
 z6t=0; return 
end

%=================================================================

 z3d=reshape(z3d,[nc 6 nc nr]); z3d=permute(z3d,[1 3 4 2]);
 ncp=nc+1; z6t=zeros(ncp,ncp,nr,6);

%-- split on to 6 faces:
 z6t([1:nc],[1:nc],:,:)=z3d;

%-- add overlap in i+1 & j+1 :
 z6t(ncp,[1:nc], :,1)=z3d(1,[1:nc],:,2);
 z6t(ncp,[2:ncp],:,2)=z3d([nc:-1:1],1,:,4);
 z6t(ncp,[1:nc], :,3)=z3d(1,[1:nc],:,4);
 z6t(ncp,[2:ncp],:,4)=z3d([nc:-1:1],1,:,6);
 z6t(ncp,[1:nc], :,5)=z3d(1,[1:nc],:,6);
 z6t(ncp,[2:ncp],:,6)=z3d([nc:-1:1],1,:,2);
 
 z6t([2:ncp],ncp,:,1)=z3d(1,[nc:-1:1],:,3);
 z6t([1:nc], ncp,:,2)=z3d([1:nc],1,:,3);
 z6t([2:ncp],ncp,:,3)=z3d(1,[nc:-1:1],:,5);
 z6t([1:nc], ncp,:,4)=z3d([1:nc],1,:,5);
 z6t([2:ncp],ncp,:,5)=z3d(1,[nc:-1:1],:,1);
 z6t([1:nc], ncp,:,6)=z3d([1:nc],1,:,1);   

%----------------------------------------------------

%-- missing corners :
if dims(1) ~= nPts,
%- use the average value (from the 3 neighbours)
 zzC(1,:)=z3d(1,nc,:,1)+z3d(1,nc,:,3)+z3d(1,nc,:,5);
 zzC(2,:)=z3d(nc,1,:,2)+z3d(nc,1,:,4)+z3d(nc,1,:,6);
 zzC=zzC/3;
 fprintf('split_Z_cub: fills 2 missing corners with local mean value\n');
end
%- 1rst (nPg+1) = N.W corner of face 1
 z6t(1,ncp,:,1)=zzC(1,:);
 z6t(1,ncp,:,3)=zzC(1,:);
 z6t(1,ncp,:,5)=zzC(1,:);
%- 2nd  (nPg+2) = S.E corner of face 2
 z6t(ncp,1,:,2)=zzC(2,:);
 z6t(ncp,1,:,4)=zzC(2,:);
 z6t(ncp,1,:,6)=zzC(2,:);

if nDim > 2,
 z6t=reshape(z6t,[ncp ncp dims(3:end) 6]);
else
 z6t=squeeze(z6t);
end

return
