function plot_faces(nfg,vF,k,ccB,rgbDim);
% plot_faces(nfg,vF,k [,ccB,rgbDim]);
%
% make 1 figure (fig. number = nfg) composed of Nface parts
% with color range ccB(1),ccB(2) (default = ccB=[0 0] = no call to caxis)
% corresponding to level k (enter k=0 if 2D field)
%  a) of the structured array "vF": vF.f001 = face 1, vF.f002 = face 2 ...
% or
%  b) of simple array "vF" in compact format (with cs-grid dimensions =
%     rgbDim = [nR nG nB] )

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
   %- get facet dimensions:
   var=vF.(cvar);
   if i == 1, nR=size(var,1); nG=size(var,2);
   elseif i==2, nB=size(var,1); end
  end
 end
 listF=cellstr(listF);
 nFaces=size(listF,1); compact=0;
else
%- compact format:
 compact=1;
 dim0=size(vF); ndims=length(dim0);
 if ndims == 2, nz=1; else nz=dim0(3); end
 nPg=dim0(1)*dim0(2);
 vv=reshape(vF,[nPg nz]);
 if nargin < 5,
% set default for nR,nG,nB (only used for compact fmt):
%  rgbDim=[90 270 90];
   n1=dim0(1); n2=dim0(2);
   n2=n2-n1; if rem(n2,4*n1) ~= 0, n2=n2-n1; end
   if rem(n2,4) ~= 0,
     error('compact fmt input => Need grid 3 dims (rgbDim, 5th Arg)')
   end
   rgbDim=[n1 n2/4 n1];
 end
 nR=rgbDim(1); nG=rgbDim(2); nB=rgbDim(3);
%- set all 6 faces dimensions
 nf=ones(6,2);
 nf(1,:)=[nR nG]; nf(2,:)=[nB nG]; nf(3,:)=[nB nR];
 nf(4,:)=[nG nR]; nf(5,:)=[nG nB]; nf(6,:)=[nR nB];
 fdim=prod(nf,2); fd2= cumsum(fdim); fd1=fd2-fdim+1;
%-- check that "number of point "nPg" matches fd2(nFaces)
 [N]=find(fd2 == nPg);
 if length(N) == 1, nFaces=N; else
   fprintf(' # of points nPg= %i do not match any Nb faces (fd2):\n',nPg);
   fprintf(' fd2='); fprintf(' %i ,',fd2); fprintf('\n');
   error('check size of (compact fmt) input !')
 end
end

 fprintf(' plot_faces: nFaces= %i, nR= %i, nG= %i, nB= %i\n',nFaces,nR,nG,nB);
if nFaces > 6,
  error([' Nb of faces =',int2str(nFaces),' > 6 !'])
end

xyP=zeros(6,4); ySep=0.025; dyP1=nG/(nR+nG)*0.9; dyP2=nR/(nR+nG)*0.9;
xyP(1,:)=[0.05  0.05  0.21  dyP1]; yOr2=xyP(1,2)+ySep+dyP1;
xyP(2,:)=[0.29  0.05  0.21  dyP1];
xyP(3,:)=[0.29  yOr2  0.21  dyP2];
xyP(4,:)=[0.53  0.05  0.21  dyP1];
xyP(5,:)=[0.77  0.05  0.21  dyP1];
xyP(6,:)=[0.77  yOr2  0.21  dyP2];

%- move face 3 in top-left corner:
xyP(3,:)=[0.05  yOr2  0.21  dyP2];
%fprintf(' dyP1, yOr2, dyP2= %5.3f %5.3f %5.3f\n',dyP1,yOr2,dyP2);

figure(nfg);clf;
for n=1:nFaces,
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
