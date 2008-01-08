function writebin(fnam,fld,typ,prec,skip,mform)

% Function writebin(fnam,fld,typ,prec,skip,mform)
% write N-D binary field
%
% INPUTS
% fnam  input path and file name
% fld   input array
% typ   0: sequential FORTRAN;  1: plain binary (the default)
% prec  numeric precision (default 'real*4')
% skip  records to skip before writing (default 0)
% mform machine format (see fopen; default: 'ieee-be')
%
% SEE ALSO
% readbin

if nargin < 6, mform='ieee-be'; end
if nargin < 5, skip=0; end
if nargin < 4, prec='real*4'; end
if nargin < 3, typ=1; end
if skip>0 & typ==0, error('feature not implemented yet'); end
if nargin < 2, error('please specify array and output file name'); end
reclength=0;

if exist(fnam)==2
  fid=fopen(fnam,'r+',mform);
else
  fid=fopen(fnam,'w',mform);
end

switch typ
  
  case 0

    write_record(fid,fld,prec);
  
  case 1

    if skip==0
      
      fwrite(fid,fld,prec);
    
    else

      switch prec
        case {'int8','integer*1'}
          reclength=prod(size(fld));
        case {'int16','integer*2','uint16','integer*2'}
          reclength=2*prod(size(fld));
        case {'int32','integer*4','uint32','single','real*4','float32'}
          reclength=4*prod(size(fld));
        case {'int64','integer*8','uint64','double','real*8','float64'}
          reclength=8*prod(size(fld));
      end
      
      if fseek(fid,skip*reclength,'bof') == 0
        
        fwrite(fid,fld,prec);
        
      else
        
        fseek(fid,0,'eof');
        file_length=ftell(fid);
        fwrite(fid,zeros((skip*reclength-file_length),1),'int8');
        fwrite(fid,fld,prec);
        
      end
      
    end
    
end

fid=fclose(fid);
