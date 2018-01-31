function plot_faces(nfg,vF,k,ccB,rgbDim);
% plot_faces(nfg,vF,k [,ccB,rgbDim]);
%
% make 1 figure (fig. number = nfg) composed of Nface parts
% with color range ccB(1),ccB(2) (default = ccB=[0 0] = no call to caxis)
% corresponding to level k (enter k=0 if 2D field) 
%  a) of the structured array "vF": vF.f001 = face 1, vF.f002 = face 2 ...
% or
%  b) of simple array "vF" in compact format (with cs-grid dimensions =
%     rgbDim = [nr ng nb] )

if nargin < 4, ccB=[0 0]; end

%- extract list of faces:
if isstruct(vF),
 compact=0;
%  convention: faces identificator is "f{n}" (face number n written with 3 digits)
 listV=fieldnames(vF); listF=[];
 for i=1:size(listV,1)
  cvar=char(listV(i));
  if length(cvar) == 4 & strncmpi('f00',cvar,3),
   if isempty(listF), listF=cvar; else listF=char(listF,cvar); end
  end
 end
 listF=cellstr(listF);
 Nfaces=size(listF,1); compact=0;
else
%- compact format:
 compact=1; Nfaces=5;
 dim0=size(vF); ndims=length(dim0);
 if ndims == 2, nz=1; else nz=dim0(3); end
 vv=reshape(vF,[dim0(1)*dim0(2) nz]);
 if nargin < 5, 
% set default for nr,ng,nb (only used for compact fmt):
%  rgbDim=[360 90 90];
   n1=dim0(1); n2=dim0(2);
   n2=n2-n1; if rem(n2,4*n1) ~= 0, n2=n2-n1; end
   if rem(n2,4) ~= 0,
     error('compact fmt input => Need grid 3 dims (rgbDim, 5th Arg)')
   end
   rgbDim=[n2/4 n1 n1];
 end
 nr=rgbDim(1); ng=rgbDim(2); nb=rgbDim(3);
%- set all 6 faces dimensions
 nf=ones(6,2);
 nf(1,:)=[nb nr]; nf(2,:)=[ng nr]; nf(3,:)=[ng nb];
 nf(4,:)=[nr nb]; nf(5,:)=[nr ng]; nf(6,:)=[nb ng];
 fdim=prod(nf,2); fd2= cumsum(fdim); fd1=fd2-fdim+1;
%-
end

if Nfaces > 6,
  error([' Nb of faces =',int2str(Nfaces),' > 6 !'])
end

xyP=zeros(6,4);
xyP(1,:)=[0.05  0.05  0.20  0.72];
xyP(2,:)=[0.29  0.05  0.20  0.72];
xyP(3,:)=[0.29  0.80  0.20  0.18];
xyP(4,:)=[0.53  0.05  0.20  0.72];
xyP(5,:)=[0.77  0.05  0.20  0.72];
xyP(6,:)=[0.77  0.80  0.20  0.18];

xyP(3,:)=[0.05  0.80  0.20  0.18]; %- move face 3 in top-left corner
figure(nfg);clf;
for n=1:Nfaces,
 if compact == 1,
   var=reshape(vv(fd1(n):fd2(n),:),[nf(n,:) nz]);
 else
   cvar=char(listF(n)); var=vF.(cvar);
 end
 if k ~= 0, var=var(:,:,k); end
 if n > 3,  var=fliplr(var'); end %- rotate face 4,5,6
 if n == 3, var=flipud(var'); end %- rotate to put face 3 in top-left corner
 AA=axes('position',xyP(n,:));
 imagesc(var'); set(gca,'YDir','normal');
 if ccB(1) < ccB(2), caxis(ccB); end
end
H=colorbar('peer',AA,'North');
set(H,'Position',[xyP(4,1) 0.90 xyP(4,3) 0.020]);

return
