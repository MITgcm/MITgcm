
rDir='../run/'; nf=0;
kwr=1;

%-----------------------------
gDir=rDir; %gDir='res_ex2/';
G=load_grid(gDir);
nx=G.dims(1); ny=G.dims(2); nr=G.dims(3); 
nPxy=nx*ny; npx=nx+1; npy=ny+1;
%-----------------------------

mskInC=rdmds([rDir,'maskInC']);
mskInW=rdmds([rDir,'maskInW']);
mskInS=rdmds([rDir,'maskInS']);

msk=G.hFacC; 
msk=min(msk,1); msk=ceil(msk); msk1=msk(:,:,1);

ccB=[-1 2];
xax=G.xAxC ; yax=G.yAxC;
dxRed=0.1; dyRed=0; dxB=0.02; dyB=0.9;
[xyP,xyB]=def_subP(2,dxRed,dyRed,dxB,dyB);
xtxt=mean(xax); ytxt=yax(1)-(yax(end)-yax(1))*0.11;

%-- mask at OB:
mskOBw=squeeze(G.hFacW(2,:,:)); 
mskOBe=squeeze(G.hFacW(nx,:,:));
mskOBn=squeeze(G.hFacS(:,ny,:));
mskOBw=ceil(mskOBw);
mskOBe=ceil(mskOBe);
mskOBn=ceil(mskOBn);

%-- make connected pieces Id "by hand" (+ lot to check):
k=12;
nf=nf+1; figure(nf);clf; nsp=0;
 nsp=nsp+1; axes('position',xyP(nsp,:));
 var=mskInC;
 var(find(msk(:,:,k)==0))=NaN;
%mnV=min(var(:)); MxV=max(var(:));
 imagesc(xax,yax,var'); set(gca,'YDir','normal');%colorbar('horiz');
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1); 
 BB=colorbar; set(BB,'Position',xyB(nsp,:));
%text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
 title(['maskInC + domain @ k= ',int2str(k)]);

k=13; 
 mskOBw(11:ny,k)=2*mskOBw(11:ny,k);
 mskOBn(1,k)=2*mskOBn(1,k);
 mskOBn(21:nx-2,k)=3*mskOBn(21:nx-2,k);
 mskOBn(nx-1:nx,k)=4*mskOBn(nx-1:nx,k);
 mskOBe(1:10,k)=3*mskOBe(1:10,k);
 mskOBe(12:15,k)=5*mskOBe(12:15,k);
 mskOBe(17:ny,k)=4*mskOBe(17:ny,k);

 nsp=nsp+1; axes('position',xyP(nsp,:));
 var=mskInC;
 var(find(msk(:,:,k)==0))=NaN;
 imagesc(xax,yax,var'); set(gca,'YDir','normal');%colorbar('horiz');
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1); 
 BB=colorbar; set(BB,'Position',xyB(nsp,:));
 title(['maskInC + domain @ k= ',int2str(k)]);

k=14; 
 mskOBw(11:ny,k)=2*mskOBw(11:ny,k);
 mskOBn(1,k)=2*mskOBn(1,k);
 mskOBn(1:20,k)=5*mskOBn(1:20,k);
 mskOBn(21:nx-2,k)=3*mskOBn(21:nx-2,k);
 mskOBn(nx-1:nx,k)=4*mskOBn(nx-1:nx,k);
 mskOBe(1:10,k)=3*mskOBe(1:10,k);
 mskOBe(17:ny,k)=4*mskOBe(17:ny,k);

nf=nf+1; figure(nf);clf; nsp=0;
 nsp=nsp+1; axes('position',xyP(nsp,:));
 var=mskInC;
 var(find(msk(:,:,k)==0))=NaN;
 imagesc(xax,yax,var'); set(gca,'YDir','normal');%colorbar('horiz');
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1); 
 BB=colorbar; set(BB,'Position',xyB(nsp,:));
 title(['maskInC + domain @ k= ',int2str(k)]);

k=15; 
 mskOBw(11:ny,k)=2*mskOBw(11:ny,k);
 mskOBn(21:nx-2,k)=4*mskOBn(21:nx-2,k);
 mskOBe(1:10,k)=3*mskOBe(1:10,k);

 nsp=nsp+1; axes('position',xyP(nsp,:));
 var=mskInC;
 var(find(msk(:,:,k)==0))=NaN;
 imagesc(xax,yax,var'); set(gca,'YDir','normal');%colorbar('horiz');
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1); 
 BB=colorbar; set(BB,'Position',xyB(nsp,:));
 title(['maskInC + domain @ k= ',int2str(k)]);

prec='real*4';
if kwr > 0,
  var=mskOBn; namf='connect_obN.bin';
  fprintf(' write file: %-25s (%i %i)',namf,size(var));
  fid=fopen(namf,'w','b'); fwrite(fid,var,prec); fclose(fid);
  fprintf('\n');
  var=mskOBe; namf='connect_obE.bin';
  fprintf(' write file: %-25s (%i %i)',namf,size(var));
  fid=fopen(namf,'w','b'); fwrite(fid,var,prec); fclose(fid);
  fprintf('\n');
  var=mskOBw; namf='connect_obW.bin';
  fprintf(' write file: %-25s (%i %i)',namf,size(var));
  fid=fopen(namf,'w','b'); fwrite(fid,var,prec); fclose(fid);
  fprintf('\n');
%- also write a zero field
  var=zeros(nx,nr); namf='zeros_obX.bin';
  fprintf(' write file: %-25s (%i %i)',namf,size(var));
  fid=fopen(namf,'w','b'); fwrite(fid,var,prec); fclose(fid);
  fprintf('\n');
end

xob=[1:2*ny+nx]; zcp=G.rC; zfp=G.rF;
zc=[1:nr]; zf=0.5:nr+0.5;
xob=[1:nx]; yob=[1:ny];

ccB=[-0.2 5.2];
nf=nf+1; figure(nf); clf;
subplot(221);
 var=mskOBw; var(find(var==0))=NaN;
 imagesc(yob,zc,var'); set(gca,'YDir','reverse');%colorbar('horiz');
 caxis(ccB);
 change_colmap(-1); 
 colorbar;
 title('mskOBw');

subplot(222);
 var=mskOBe; var(find(var==0))=NaN;
 imagesc(yob,zc,var'); set(gca,'YDir','reverse');%colorbar('horiz');
 caxis(ccB);
%colorbar;
 title('mskOBe');

subplot(212);
 var=mskOBn; var(find(var==0))=NaN;
 imagesc(xob,zc,var'); set(gca,'YDir','reverse');%colorbar('horiz');
caxis(ccB);
%colorbar;
 title('mskOBn');

return

k1=1; ccB=[-1 2];
nf=nf+1; figure(nf);clf; nsp=0;
 nsp=nsp+1; axes('position',xyP(nsp,:));
 var=mskInW;
 var(find(G.hFacW(:,:,k1)==0))=NaN;
 mnV=min(var(:)); MxV=max(var(:));
 imagesc(xax,yax,var'); set(gca,'YDir','normal');%colorbar('horiz');
%ccB=[mnV MxV]+[-1 1]*(MxV-mnV)*0.05;
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1); 
 BB=colorbar; set(BB,'Position',xyB(nsp,:));
 text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
 title(['maskInW + domain @ k= ',int2str(k1)]);

 nsp=nsp+1; axes('position',xyP(nsp,:));
 var=mskInS;
 var(find(G.hFacS(:,:,k1)==0))=NaN;
 mnV=min(var(:)); MxV=max(var(:));
 imagesc(xax,yax,var'); set(gca,'YDir','normal');%colorbar('horiz');
 if ccB(2) > ccB(1), caxis(ccB); end
 change_colmap(-1); 
 BB=colorbar; set(BB,'Position',xyB(nsp,:));
 text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
 title(['maskInS + domain @ k= ',int2str(k1)]);
