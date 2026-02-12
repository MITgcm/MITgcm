%- this is an example which do conversion between
% a) old-format (not compact) to compact format
% b) compact-format to per face format (matlab structure)

 rgbDim=[90 270 90];
%rgbDim=[1 1 1]*32;

%- set all 6 faces dimensions
 nR=rgbDim(1); nG=rgbDim(2); nB=rgbDim(3);
 nf=ones(6,2);
 nf(1,:)=[nR nG]; nf(2,:)=[nB nG]; nf(3,:)=[nB nR];
 nf(4,:)=[nG nR]; nf(5,:)=[nG nB]; nf(6,:)=[nR nB];
 fdim=prod(nf,2); fd2= cumsum(fdim); fd1=fd2-fdim+1;

 namf='XC';
 namf=['res_s00/',namf]; oldFormat=1;
%namf=['res_n00/',namf]; oldFormat=0;

%- read file:
fprintf(['reading file: ',namf,' ...']);TimeT0=clock;
v0=rdmds(namf);
TimeT1=clock; fprintf(' (took %6.4f s)\n',etime(TimeT1,TimeT0));
dim0=size(v0); ndims=length(dim0);

if oldFormat,
%- with old format, cannot guess Nb of faces to process --> specify it here:
%- default = 6 ; polar-cap (with or without missing tiles) = 5
% NOTE: has changed (2007-03-21) rdmds so that it always return
%       a full domain array (filling up for missing tiles)
 nFaces=6;

 lgx=nR+2*nB+2*nG+nR; lgy=nG;
 if nFaces==5, lgx=lgx-nR; end %- face 6 missing

%- field was read from standard (old) format file (global or tiled):
%  expected size: lgx * lgy ; transfert v0 -> v1 in compact format
 nPg=fd2(nFaces); ib=0;
 if ndims == 2,
  v1=zeros(nPg,1); dim1=[nPg 1];
  for n=1:nFaces
   vv=v0(ib+1:ib+nf(n,1),1:nf(n,2)); %- extract face n
   v1(fd1(n):fd2(n),1)=reshape(vv,[fdim(n) 1]);
   ib=ib+nf(n,1);
  end
 else
  n3=prod(dim0(3:end));
  v1=zeros(nPg,1,n3);
  for n=1:nFaces
   vv=v0(ib+1:ib+nf(n,1),1:nf(n,2),:); %- extract face n
   v1(fd1(n):fd2(n),1,:)=reshape(vv,[fdim(n) 1 n3]);
   ib=ib+nf(n,1);
  end
  dim1=[nPg 1 dim0(3:end)];
  v1=reshape(v1,dim1);
end

%-- end olfFormat
end

if ~oldFormat,
%- field was read from compact (new) format file (global or tiled):
%  just copy to v1:
 v1=v0;
 nPg=dim0(1)*dim0(2);
%-- check that number of points "nPg" matches fd2(nFaces)
 [N]=find(fd2 == nPg);
 if length(N) == 1, nFaces=N; else
   fprintf(' # of points nPg= %i do not match any Nb faces (fd2):\n',nPg);
   fprintf(' fd2='); fprintf(' %i ,',fd2); fprintf('\n');
   error('check size of (compact fmt) input !')
 end
 if ndims == 2, dim1=[nPg 1]; else dim1=[nPg 1 dim0(3:end)]; end
 v1=reshape(v1,dim1);
end

%- put variable into matlab structure "vF", with 6 faces: vF.f001, vF.f002,  ...
 if ndims == 2, dim1=[dim1 1]; end; n3=prod(dim1(3:end));
 v1=reshape(v1,[nPg n3]);
 clear vF
 for n=1:nFaces,
  v1f=reshape(v1(fd1(n):fd2(n),:),[nf(n,:) dim1(3:end)]);
  nvar=sprintf('vF.f%3.3i=v1f;',n); eval(nvar)
 end

return
%----------------------------------------------------------------------
%- make a plot to check
figure(1);clf; ccB=[0 0];
xyP=zeros(6,4);
xyP(1,:)=[0.05  0.05  0.20  0.72];
xyP(2,:)=[0.29  0.05  0.20  0.72];
xyP(3,:)=[0.29  0.80  0.20  0.18];
xyP(4,:)=[0.53  0.05  0.20  0.72];
xyP(5,:)=[0.77  0.05  0.20  0.72];
xyP(6,:)=[0.77  0.80  0.20  0.18];

k=1; ccB=[-1 1]*190;
for n=1:nFaces,
 nvar=sprintf('var=vF.f%3.3i;',n); eval(nvar);
 if ndims > 2, var=var(:,:,k); end
 if n > 3, var=fliplr(var'); end
 AA=axes('position',xyP(n,:));
 imagesc(var'); set(gca,'YDir','normal');
 if ccB(1) < ccB(2), caxis(ccB); end
end
H=colorbar('peer',AA,'North');
set(H,'Position',[xyP(4,1) 0.90 xyP(4,3) 0.020]);
