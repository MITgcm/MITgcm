
kwr=0;
rDir='../input/';

inpSfx='.bin'; prec='real*8';
outSfx=inpSfx; precOut=prec;
%outSfx='.r4_bin'; precOut='real*4';

gDir='grid/';
G=load_grid(gDir,0);
nx=G.dims(1); ny=G.dims(2); nr=G.dims(3);

mskC=ceil(G.hFacC);
rAc=G.rAc.*mskC(:,:,1); areaG=sum(rAc(:));

%for j=1:length(listInpF)
% namF=char(listInpF(j));
  namF='shiEmPR_cs32'; n3d=1; j=1;
  fprintf('-- processing file (#%i) %s ',j,namF);
 %fprintf('(length= %i ) :',length(namF));
  fprintf(': n3d= %i ',n3d);
  var=rdda([rDir,namF,inpSfx],[nx ny n3d],1,prec,'b');
  fprintf(' Make changes\n');
  emp0=var;

  var=emp0; titv=namF;
  var=var.*mskC(:,:,1); mnV=min(var(:)); MxV=max(var(:));
  var=var.*rAc; Avr=sum(var(:))/areaG;
  fprintf(' field %15s : min,max= %e, %e , Mean= %e\n',titv,mnV,MxV,Avr);

  %- Multiply by 10 and substract 1.e-7 (~ 1.cm/d , excess of Precip):
  emp1=10*emp0-ones(nx,ny)*1.e-7;

  var=emp1; titv='- New-';
  var=var.*mskC(:,:,1); mnV=min(var(:)); MxV=max(var(:));
  var=var.*rAc; Avr=sum(var(:))/areaG;
  fprintf(' field %15s : min,max= %e, %e , Mean= %e\n',titv,mnV,MxV,Avr);

  %-- convert to addMass:
  rhoFresh=1000.;
  addM=-rhoFresh*emp1; %- mass input in kg/m2/s
  addM=addM.*rAc;      %- in kg/s

  var=addM; titv='addMass';
  var=var.*mskC(:,:,1); mnV=min(var(:)); MxV=max(var(:));
  var=var.*rAc; Avr=sum(var(:))/areaG;
  fprintf(' field %15s : min,max= %e, %e , Mean= %e\n',titv,mnV,MxV,Avr);

  if kwr > 0,
   fnam=[namF,outSfx];
   fnam=['large_Precip',outSfx]; var=emp1;
   fprintf('  writing file: %s ...',fnam);
   fid=fopen(fnam,'w','b');
   fwrite(fid,var,precOut); fclose(fid);
   fprintf(' done\n');

   fnam=['addMass_k1',outSfx]; var=zeros(nx,ny,nr); var(:,:,1)=addM;
   fprintf('  writing file: %s ...',fnam);
   fid=fopen(fnam,'w','b');
   fwrite(fid,var,precOut); fclose(fid);
   fprintf(' done\n');
  end

%end
%return

figure(1);clf; colormap('jet');
 var=emp0; ccB=[-1 1]*5.e-8;
%var=emp1; ccB=[-1 1]*5.e-7;
 imagesc([1:nx],[1:ny],var');
 set(gca,'YDir','normal');colorbar;
 caxis(ccB);

return
