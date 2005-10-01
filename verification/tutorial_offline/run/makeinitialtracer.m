
tr=rdmds('../../cfc_offline/run/PTRACER01.0004269600');
tr(find(tr==0))=-999;
tr2=tr;
tr(find(tr>0))=0;
tr1=tr;
tr1(55:60,45:50,:)=1000;
fid=fopen('tracer1.bin','w','b');
fwrite(fid,tr1,'float32');
fid=fopen('tracer2.bin','w','b');
fwrite(fid,tr2,'float32');


