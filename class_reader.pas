unit Class_Reader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Class_Matrix;

type
  //dmatrix = array of array of double;
  smatrix = array of array of string;
  //dvector = array of double;
  svector = array of string;

type
  CReader = class
    function StringToSVector(strvec: string): svector;
    function SVectorToDVector(svec: svector): dvector;
    function StringToDVector(strvec: string): dvector;
    function DVectorToDMatrixF(dvec: dvector): dmatrix;
    function DVectorToDMatrixC(dvec: dvector): dmatrix;
    function MatrixToString(dmtx: dmatrix): string;
    function MatrixError(dmtx:dmatrix): string;
  end;

implementation

function CReader.StringToSVector(strvec: string): svector;
var StrList: TStringList;
    tvec: svector;
    iPos: Integer;
    i: Integer;
begin
  StrList:= TStringList.Create;
  iPos:= 1;
  StringReplace(strvec,' ',',',[rfReplaceAll,rfIgnoreCase]);//::
  StrList.Delimiter:=',';
  StrList.StrictDelimiter:= true;
  StrList.DelimitedText:= Copy(strvec,iPos+1,Length(strvec)-iPos-1);
  SetLength(tvec,StrList.Count);
  for i:=0 to Length(tvec)-1 do
  begin
      tvec[i]:= StrList[i];
  end;
  StrList.Destroy;
  StringToSVector:= tvec;
end;

function CReader.SVectorToDVector(svec: svector): dvector;
var dvec: dvector;
    i: Integer;
begin
  SetLength(dvec,Length(svec));
  for i:=0 to Length(svec)-1 do
  begin
    dvec[i]:= StrToFloat(svec[i]);
  end;
  SVectorToDVector:= dvec;
end;

function CReader.StringToDVector(strvec: string): dvector;
var StrList: TStringList;
    tvec: dvector;
    iPos: Integer;
    i: Integer;
begin
  StrList:= TStringList.Create;
  iPos:= 1;
  StringReplace(strvec,' ',',',[rfReplaceAll,rfIgnoreCase]);//::
  StrList.Delimiter:=',';
  StrList.StrictDelimiter:= true;
  StrList.DelimitedText:= Copy(strvec,iPos+1,Length(strvec)-iPos-1);
  SetLength(tvec,StrList.Count);
  for i:=0 to Length(tvec)-1 do
  begin
      tvec[i]:= StrToFloat(StrList[i]);
  end;
  StrList.Destroy;
  StringToDVector:= tvec;
end;

function CReader.DVectorToDMatrixF(dvec: dvector): dmatrix;
var dmtx: dmatrix;
    i: Integer;
begin
  SetLength(dmtx,1,Length(dvec));
  for i:=0 to Length(dvec)-1 do
  begin
      dmtx[0,i]:= dvec[i];
  end;
  DVectorToDMatrixF:= dmtx;
end;

function CReader.DVectorToDMatrixC(dvec: dvector): dmatrix;
var dmtx: dmatrix;
    i: Integer;
begin
  SetLength(dmtx,Length(dvec),1);
  for i:=0 to Length(dvec)-1 do
  begin
      dmtx[i,0]:= dvec[i];
  end;
  DVectorToDmatrixC:= dmtx;
end;

function CReader.MatrixToString(dmtx: dmatrix): string;
var s: string;
    i,j: Integer;
    v: string;
begin
  s:= '[';

  for i:=0 to Length(dmtx)-1 do
  begin
      for j:=0 to Length(dmtx[0])-1 do
      begin
          v:= FloatToStr(dmtx[i,j]);
          s:= Concat(s,v,',');
      end;
      s:= Concat(s,';');
  end;
  s:= Concat(s,']');

  MatrixToString:= s;

end;

function CReader.MatrixError(dmtx:dmatrix): string;
var s: string;
    i,j: Integer;
    v: string;
begin
  s:= '[';

  for i:=0 to Length(dmtx)-1 do
  begin
      for j:=0 to Length(dmtx[0])-1 do
      begin
          v:= 'Nan';
          s:= Concat(s,v,',');
      end;
      s:= Concat(s,';');
  end;
  s:= Concat(s,']');

  MatrixError:= s;

end;

end.

