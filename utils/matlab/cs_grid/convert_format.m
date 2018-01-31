%- this is an example which do conversion between
% a) old-format (not compact) to compact format
% b) compact-format to per face format (matlab structure)

nr=360;nb=90;ng=90;
%- set all 6 faces dimensions
nf=ones(6,2); 
nf(1,:)=[nb nr]; nf(2,:)=[ng nr]; nf(3,:)=[ng nb]; 
nf(4,:)=[nr nb]; nf(5,:)=[nr ng]; nf(6,:)=[nb ng]; 
fdim=prod(nf,2); fd2= cumsum(fdim); fd1=fd2-fdim+1;

namf='XC';
%namf='T.0000000000';
namf=['res_s00/',namf]; oldFormat=1;
%namf=['res_n00/',namf]; oldFormat=0;

%- nb of face to process (default = 6, polar-cap with missing tile = 5)
% NOTE: has changed (2007-03-21) rdmds so that it always return a 
%       full domain array.
Nfaces=6;

if oldFormat,

lgx=nb+2*ng+2*nr+nb; lgy=nr;
if Nfaces==5, lgx=lgx-nb; end %- face 6 missing
%- read file in standard (old) format (global or tiled):
%  expected size: lgx * lgy
fprintf(['reading file: ',namf,' ...']);TimeT0=clock;
v0=rdmds(namf);
TimeT1=clock; fprintf(' (took %6.4f s)\n',etime(TimeT1,TimeT0));
dim0=size(v0); ndims=length(dim0);

%- transfert v0 -> v1 in compact format
ib=0; 
if ndims == 2,
 v1=zeros(fd2(end),1);
 for n=1:Nfaces
  vv=v0(ib+1:ib+nf(n,1),1:nf(n,2)); %- extract face n
  v1(fd1(n):fd2(n),1)=reshape(vv,[fdim(n) 1]);
  ib=ib+nf(n,1);
 end
else
 n3=prod(dim0(3:end));
 v1=zeros(fd2(end),1,n3);
 for n=1:Nfaces
  vv=v0(ib+1:ib+nf(n,1),1:nf(n,2),:); %- extract face n
  v1(fd1(n):fd2(n),1,:)=reshape(vv,[fdim(n) 1 n3]);
  ib=ib+nf(n,1);
 end
 dim1=[fd2(end) 1 dim0(3:end)];
 v1=reshape(v1,dim1);
end

%-- end olfFormat
end

if ~oldFormat,
%- read file in compact (new) format, (global or tiled):
fprintf(['reading file: ',namf,' ...']);TimeT0=clock;
v1=rdmds(namf);
TimeT1=clock; fprintf(' (took %6.4f s)\n',etime(TimeT1,TimeT0));
%- end newFormat
end
dim1=size(v1); ndims=length(dim1);

%- put variable into matlab structure "vF", with 6 faces: vF.f1, vF.f2,  ... 
if ndims == 2, dim1=[dim1 1]; end; n3=prod(dim1(3:end));
Np1=dim1(1)*dim1(2); v1=reshape(v1,[Np1 n3]);
clear vF
for n=1:Nfaces,
 vf1=reshape(v1(fd1(n):fd2(n),:),[nf(n,:) dim1(3:end)]);
 nvar=['vF.f',int2str(n),'=vf1;']; eval(nvar) 
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

k=1; ccB=[-1 1]*200;
for n=1:Nfaces,
 nvar=['var=vF.f',int2str(n),';']; eval(nvar);
 if ndims > 2, var=var(:,:,k); end
 if n > 3, var=fliplr(var'); end
 AA=axes('position',xyP(n,:));
 imagesc(var'); set(gca,'YDir','normal');
 if ccB(1) < ccB(2), caxis(ccB); end 
end
H=colorbar('peer',AA,'North');
set(H,'Position',[xyP(4,1) 0.90 xyP(4,3) 0.020]);
