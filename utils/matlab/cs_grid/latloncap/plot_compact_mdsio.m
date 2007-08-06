
mitfields = 'y'
if mitfields == 'y'
  ypre  = 'run_ad'
  ydate = '0000000000'
  yvar = 'adxx_theta'
else
  binfile='run_ad/adxx_theta.0000000000.llc.0.bin';
end
cmin = -5.e-3
cmax =  5.e-3

% "facet" sizes
% Fifteent level llc_cs90x90x360
nr=360;nb=90;ng=90;
nf=ones(6,2);
nf(1,:)=[nb nr]; nf(2,:)=[ng nr]; nf(3,:)=[ng nb];
nf(4,:)=[nr nb]; nf(5,:)=[nr ng]; nf(6,:)=[nb ng];
fdim=prod(nf,2); fd2= cumsum(fdim); fd1=fd2-fdim+1;
mapIO=1; Nfaces=6;

% Read global bathymetry file
lgx=2*nb+2*ng+2*nr;
lgy=nr;

% Bathymetry
  if mapIO == -1,
   gbathy=zeros(lgx,lgy);
   fid=fopen(binfile,'r','ieee-be');
   gbathy=fread(fid,size(gbathy),'float64');
   fclose(fid);
   phi=gbathy;
  else
   gbathy=zeros(fd2(end),1);
   if mitfields == 'y'
     AAA=rdmds([ ypre, '/', yvar, '.', ydate ]);
     gbathy=reshape(squeeze(AAA(:,:,1)),fd2(end),1);
   else
     fid=fopen(binfile,'r','ieee-be');
     gbathy=fread(fid,size(gbathy),'float64');
     fclose(fid);
   end
%- convert to old format:
   phi=zeros(lgx,lgy); ish=0;
   for n=1:Nfaces,
    is0=ish+1; ie0=ish+nf(n,1);
    phi(is0:ie0,1:nf(n,2))=reshape(gbathy(fd1(n):fd2(n),1),[nf(n,:)]);
    ish=ie0 ;
   end
  end

% Layout facets in a "connected" form.
lgx=2*nb+2*ng;
lgy=nr+nb;
foo=zeros(lgx,lgy);
xx=[phi(1:nb,1:nr)' phi(nb+1:nb+ng,1:nr)' flipud(phi(nb+2*ng+1:nb+2*ng+nr,1:nb)) ...
    flipud(phi(nb+2*ng+nr+1:nb+2*ng+nr+nr,1:ng))]';
foo(1:lgx,1:nr)=xx;
% Duplicate 4 arctic faces on top
foo(1:nb,       nr+1:nr+ng)=flipud(phi(nb+ng+1:nb+ng+ng,1:nb)');
foo(1*nb+1:2*nb,nr+1:nr+ng)=phi(nb+ng+1:nb+ng+ng,1:nb);
foo(2*nb+1:3*nb,nr+1:nr+ng)=fliplr(phi(nb+ng+1:nb+ng+ng,1:nb)');
foo(3*nb+1:4*nb,nr+1:nr+ng)=flipud(fliplr(phi(nb+ng+1:nb+ng+ng,1:nb)));
%
% Fill in depths shallower than a certain depth;
foo(find(foo==0.))=NaN;

figure
  pcolor(flipud(foo'));
axis ij;
shading flat
  set(gca,'XTick',0:30:360);
  set(gca,'YTick',0:45:450);
grid on
caxis([cmin cmax]);
colorbar
