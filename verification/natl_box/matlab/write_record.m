function write_record (file_id, field, record_type)
% function write_record (file_id, field, record_type)
% write a fortran record
% file needs to be previously opened in write binary mode, i.e.,
% file_id=fopen(fname,'w','b');
% field contains the variable to be written
% record_type defaults to real*4

if nargin<3, record_type='real*4'; end
switch lower(record_type)
  case {'uint8','integer*1','int8', ...
        'schar','signed char','uchar','unsigned char','char','char*1'}
    record_length=length(field(:));
  case {'uint16','integer*2','int16','integer*2'}
    record_length=length(field(:))*2;
  case {'float32','real*4','uint32','integer*4','int32','integer*4'}
    record_length=length(field(:))*4;
  case {'float64','real*8','uint64','integer*8','int64','integer*8'}
    record_length=length(field(:))*8;
end
tmp=fwrite(file_id,record_length,'uint32'); % fortran record length
tmp=fwrite(file_id,field,record_type);
tmp=fwrite(file_id,record_length,'uint32'); % fortran record length
