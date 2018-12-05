unit SpecialParse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Class_Matrix, Class_Reader, Class_RPolynomials,
  Class_LPolynomials, Class_NLEquations, ParseMath;

type
  TSpecialParse = class
    function FindNumberVar(nvar: string; smat: smatrix): string;
    function FindStringVar(nvar: string; smat: smatrix): string;
    function FindArrayVar(nvar: string; smat: smatrix): string;
    function IsStringVar(nvar: string): Boolean;
    //function Evaluate(sfunc: string; smat: smatrix): string;
    function EvalPolyroot(fdata: string; smat: smatrix): string;
    function EvalPolynomial(fdata: string; smat: smatrix): string;
    function EvalSENL(fdata: string; smat: smatrix): string;
    //function EvalSEDO(fdata: string; smat: smatrix): string;
    function EvalFunc(fdata: string): svector;//[],[],[]
    function SStringToVector(svec: string; smat: smatrix): svector;
    function DStringToVector(svec: string; smat: smatrix): dvector;

  end;

implementation

function TSpecialParse.FindNumberVar(nvar:string; smat: smatrix): string;
var i: Integer;
    b: Boolean;
    s: string;
begin
 for i:=0 to Length(smat)-1 do begin
   if smat[i,0]=nvar then
   begin
     if (smat[i,2]='double') or (smat[i,2]='integer') then
     begin
       s:= smat[i,1];
       b:= true;
     end;
   end;
 end;
 if b=true then FindNumberVar:= s
 else FindNumberVar:= 'NoFound';
end;

function TSpecialParse.FindStringVar(nvar:string; smat: smatrix): string;
var i: Integer;
    b: Boolean;
    s: string;
begin
 for i:=0 to Length(smat)-1 do begin
   if smat[i,0]=nvar then
   begin
     if (smat[i,2]='string') then
     begin
       s:= smat[i,1];
       b:= true;
     end;
   end;
 end;
 if b=true then FindStringVar:= s
 else FindStringVar:= 'NoFound';
end;

function TSpecialParse.FindArrayVar(nvar:string; smat: smatrix): string;
var i: Integer;
    b: Boolean;
    s: string;
begin
 b:= false;
 for i:=0 to Length(smat)-1 do begin
   if smat[i,0]=nvar then
   begin
     if (smat[i,2]='array') then
     begin
       s:= smat[i,1];
       b:= true;
     end;
   end;
 end;
 if b=true then FindArrayVar:= s
 else FindArrayVar:= 'NoFound';
end;

function TSpecialParse.IsStringVar(nvar: string): Boolean;
var qPos: Integer;
begin
 qPos:= Pos(#39,nvar);
 if (qPos<>0) and (nvar[Length(nvar)]=#39) then IsStringVar:= true
 else IsStringVar:= false;
end;

function TSpecialParse.EvalPolyroot(fdata: string; smat: smatrix): string;
var cPos: Integer;
    dvec: dvector;
    aval: string;
    RPoly: CRPolynomials;
begin
 RPoly:= CRPolynomials.create();
 cPos:= Pos('[',fdata);
 if (cPos<>0) and (fdata[Length(fdata)]=']') then
 begin
   dvec:= DStringToVector(fdata,smat);
   EvalPolyroot:= RPoly.Execute(dvec);
 end
 else
 begin
   aval:= FindArrayVar(fdata,smat);
   if aval='NoFound' then
   begin
     EvalPolyroot:= 'EvaluationError';
   end
   else
   begin
     dvec:= DStringToVector(aval,smat);
     EvalPolyroot:= RPoly.Execute(dvec);
   end;
 end;
end;

function TSpecialParse.EvalPolynomial(fdata: string; smat: smatrix): string;
var svec: svector;
    xspoints: string;
    yspoints: string;
    xdpoints: dvector;
    ydpoints: dvector;
    cPos: Integer;
    aval: string;
    LPoly: CLPolynomials;
begin
 svec:= EvalFunc(fdata);
 LPoly:= CLPolynomials.create();
 if Length(svec)=2 then
 begin
   xspoints:= svec[0];
   yspoints:= svec[1];
   //XPOINTS
   cPos:= Pos('[',xspoints);
   if (cPos<>0) and (xspoints[Length(xspoints)]=']') then
   begin
     xdpoints:= DStringToVector(xspoints,smat);
   end
   else
   begin
     aval:= FindArrayVar(xspoints,smat);
     if aval='NoFound' then
     begin
       EvalPolynomial:= 'EvaluationError';
     end
     else
     begin
       xdpoints:= DStringToVector(aval,smat);
     end;
   end;
   //YPOINTS
   cPos:= Pos('[',yspoints);
   if (cPos<>0) and (yspoints[Length(yspoints)]=']') then
   begin
     ydpoints:= DStringToVector(yspoints,smat);
   end
   else
   begin
     aval:= FindArrayVar(yspoints,smat);
     if aval='NoFound' then
     begin
       EvalPolynomial:= 'EvaluationError';
     end
     else
     begin
       ydpoints:= DStringToVector(aval,smat);
     end;
   end;
   //Execute
   if Length(xdpoints)=Length(ydpoints) then
   begin
     EvalPolynomial:= LPoly.Execute(xdpoints,ydpoints);
     G_ConvergenceCriteria:= LPoly.CC_Lagrange;//
   end
   else
   begin
     EvalPolynomial:= 'Different Sizes';
     G_ConvergenceCriteria:= '--Invalid--';
   end;
 end
 else
 begin
   EvalPolynomial:= 'The parameters do not match';
   G_ConvergenceCriteria:= '--Invalid--';
 end;
end;

function TSpecialParse.EvalSENL(fdata: string; smat: smatrix): string;
var svec: svector;
    vars: string;
    funcs: string;
    vals: string;
    vvars: svector;
    vfuncs: svector;
    vvals: dvector;
    cPos: Integer;
    aval: string;
    SENL: CNLEquations;
begin
 svec:= EvalFunc(fdata);
 SENL:= CNLEquations.create();
 if Length(svec)=3 then
 begin
   vars:= svec[0];
   funcs:= svec[1];
   vals:= svec[2];
   //VARIABLES
   cPos:= Pos('[',vars);
   if (cPos<>0) and (vars[Length(vars)]=']') then
   begin
     vvars:= SStringToVector(vars,smat);
   end
   else
   begin
     aval:= FindArrayVar(vars,smat);
     if aval='NoFound' then
     begin
       EvalSENL:= 'EvaluationError';
     end
     else
     begin
       vvars:= SStringToVector(aval,smat);
     end;
   end;
   //FUNCTIONS
   cPos:= Pos('[',funcs);
   if (cPos<>0) and (funcs[Length(funcs)]=']') then
   begin
     vfuncs:= SStringToVector(funcs,smat);
   end
   else
   begin
     aval:= FindArrayVar(funcs,smat);
     if aval='NoFound' then
     begin
       EvalSENL:= 'EvaluationError';
     end
     else
     begin
       vfuncs:= SStringToVector(aval,smat);
     end;
   end;
   //VALUES
   cPos:= Pos('[',vals);
   if (cPos<>0) and (vals[Length(vals)]=']') then
   begin
     vvals:= DStringToVector(vals,smat);
   end
   else
   begin
     aval:= FindArrayVar(vals,smat);
     if aval='NoFound' then
     begin
       EvalSENL:= 'EvaluationError';
     end
     else
     begin
       vvals:= DStringToVector(aval,smat);
     end;
   end;
   //Execute
   if (Length(vfuncs)=Length(vvars)) and (Length(vvals)=Length(vvars)) then
   begin
     SENL.ErrorAllowed:= G_Error;
     EvalSENL:= SENL.Execute(vvars,vvals,vfuncs);
   end
   else
   begin
     EvalSENL:= 'Different Sizes';
     G_ConvergenceCriteria:= '--Invalid--';
   end;
 end
 else
 begin
   EvalSENL:= 'The parameters do not match';
   G_ConvergenceCriteria:= '--Invalid--';
 end;
end;

function TSpecialParse.EvalFunc(fdata: string): svector;
var svec: svector;
    ciPos: Integer;
    cfPos: Integer;
    ccPos: Integer;
    i: Integer;
    p: Integer;
    n: Integer;
begin
  i:= 0;
  p:= 0;
  n:= Length(fdata);

  while p<n do begin
    ciPos:= Pos('[',fdata);
    cfPos:= Pos(']',fdata);
    if (ciPos=1) and (cfPos<>0) then
    begin
      SetLength(svec,i+1);
      svec[i]:= Copy(fdata,1,cfPos);
      i:= i+1;
      p:= p + cfPos + 1;
      fdata:= Copy(fdata,cfPos+2,Length(fdata)-cfPos-1);
    end
    else
    begin
      ccPos:= Pos(',',fdata);
      if ccPos<>0 then
      begin
        SetLength(svec,i+1);
        svec[i]:= Copy(fdata,1,ccPos-1);
        i:= i+1;
        fdata:= Copy(fdata,ccPos+1,Length(fdata)-ccPos);
        p:= p + ccPos;
      end
      else
      begin
        SetLength(svec,i+1);
        svec[i]:= fdata;
        i:= i+1;
        p:= n;
      end;
    end;
  end;
  EvalFunc:= svec;
end;

function TSpecialParse.SStringToVector(svec: string; smat: smatrix): svector;
var StrList: TStringList;
    tvec: svector;
    iPos: Integer;
    i: Integer;
    sval: string;
begin
  StrList:= TStringList.Create;
  iPos:= 1;
  StringReplace(svec,' ',',',[rfReplaceAll,rfIgnoreCase]);//::
  StrList.Delimiter:=',';
  StrList.StrictDelimiter:= true;
  StrList.DelimitedText:= Copy(svec,iPos+1,Length(svec)-iPos-1);
  SetLength(tvec,StrList.Count);
  for i:=0 to Length(tvec)-1 do
  begin
    //IsVarString -> 'f(x)'
    if IsStringVar(StrList[i])=true then
    begin
      tvec[i]:= Copy(StrList[i],2,Length(StrList[i])-2);
    end
    else
    begin
      //If NoFound -> Add NoFound
      sval:= FindStringVar(StrList[i],smat);
      tvec[i]:= sval;
    end;
  end;
  StrList.Destroy;

  SStringToVector:= tvec;
end;

function TSpecialParse.DStringToVector(svec: string; smat: smatrix): dvector;
var StrList: TStringList;
    tvec: dvector;
    iPos: Integer;
    i: Integer;
    sval: string;
    dval: Double;
begin
  StrList:= TStringList.Create;
  iPos:= 1;
  StringReplace(svec,' ',',',[rfReplaceAll,rfIgnoreCase]);//::
  StrList.Delimiter:=',';
  StrList.StrictDelimiter:= true;
  StrList.DelimitedText:= Copy(svec,iPos+1,Length(svec)-iPos-1);
  SetLength(tvec,StrList.Count);
  for i:=0 to Length(tvec)-1 do
  begin
    if TryStrToFloat(StrList[i],dval)=true then
    begin
      tvec[i]:= dval;
    end
    else
    begin
      sval:= FindNumberVar(StrList[i],smat);
      if sval='NoFound' then
      begin
        tvec[i]:= 0;//NaN (0)
      end
      else
      begin
        tvec[i]:= StrToFloat(sval);
      end;
    end;
  end;
  StrList.Destroy;

  DStringToVector:= tvec;
end;

end.

