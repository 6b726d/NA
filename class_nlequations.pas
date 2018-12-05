unit Class_NLEquations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Class_Reader, Class_Matrix, ParseMath;

type
  CNLEquations = class
    ErrorAllowed: Real;
    procedure UpdateM(mtx: dmatrix; vars: svector; vals: dvector);
    function ApplyDecimal(mtx: dmatrix): dmatrix;
    function PrintNoConvergence(mtx: dmatrix): string;
    function PrintConvergence(mtx: dmatrix): string;
    function Execute(vars: svector; vals: dvector; funcs: svector): string;
    function Distance0(mtx: dmatrix): double;
    function Distance(mtx2: dmatrix; mtx1: dmatrix): double;
    function Derivative(v: string; x: double): double;
    function Jacobian(vars: svector; vals: dvector; funcs: svector): dmatrix;
    function FunctionM(vars: svector; vals: dvector; funcs: svector): dmatrix;
    private
      Parse: TParseMath;
      function NewtonRaphson(vars: svector; vals: dvector; funcs: svector): string;
    public

  end;

implementation

procedure CNLEquations.UpdateM(mtx: dmatrix; vars: svector; vals: dvector);
var i: integer;
begin
  for i:=0 to Length(vals)-1 do
  begin
    vals[i]:= mtx[i,0];
    Parse.NewValue(vars[i],vals[i]);
  end;
end;

function CNLEquations.ApplyDecimal(mtx: dmatrix): dmatrix;
var nmtx: dmatrix;
    i: integer;
begin
  SetLength(nmtx,Length(mtx),Length(mtx[0]));
  for i:=0 to Length(mtx)-1 do
  begin
    nmtx[i,0]:= RoundTo(mtx[i,0],-G_Decimal);
  end;
  ApplyDecimal:= nmtx;
end;

function CNLEquations.PrintNoConvergence(mtx: dmatrix): string;
var s: string;
    i: integer;
    v: string;
begin
  s:= '[';

  for i:=0 to Length(mtx)-1 do
  begin
    v:= 'Nan';
    s:= Concat(s,v,',');
  end;
  s:= Copy(s,1,Length(s)-1);
  s:= Concat(s,']');

  PrintNoConvergence:= s;
end;

function CNLEquations.PrintConvergence(mtx: dmatrix): string;
var s: string;
    i: integer;
    v: string;
begin
  s:= '[';

  for i:=0 to Length(mtx)-1 do
  begin
    v:= FloatToStr(mtx[i,0]);
    s:= Concat(s,v,',');
  end;
  s:= Copy(s,1,Length(s)-1);
  s:= Concat(s,']');

  PrintConvergence:= s;
end;

function CNLEquations.Execute(vars: svector; vals: dvector; funcs: svector): string;
var sr: string;
    i: integer;
begin

  Parse:= TParseMath.create;

  for i:=0 to Length(vars)-1 do
     Parse.AddVariable(vars[i],vals[i]);

  sr:= NewtonRaphson(vars,vals,funcs);

  Execute:= sr;

end;

function CNLEquations.Distance0(mtx: dmatrix): double;
var p: double;
    a: double;
    r: double;
    i: integer;
begin
  a:=0;
  for i:=0 to Length(mtx)-1 do
  begin
    p:= Power(mtx[i,0],2);
    a:= a+p;
  end;
  r:= Sqrt(p);
  Distance0:= r;
end;

function CNLEquations.Distance(mtx2: dmatrix; mtx1: dmatrix): double;
var s: double;
    p: double;
    a: double;
    r: double;
    i: integer;
begin
  a:= 0;
  for i:=0 to Length(mtx2)-1 do
  begin
    s:= mtx2[i,0] - mtx1[i,0];
    p:= Power(s,2);
    a:= a+p;
  end;
  r:= Sqrt(a);
  Distance:= r;
end;

function CNLEquations.Derivative(v: string; x: double): double;
var d: double;
    h: double;
    v1: double;
    v2: double;
    fv1: double;
    fv2: double;
begin

  h:= ErrorAllowed/10;
  v1:= x+h;
  v2:= x-h;

  Parse.NewValue(v,v1);
  fv1:= Parse.Evaluate();//f(x+h)

  Parse.NewValue(v,v2);
  fv2:= Parse.Evaluate();//f(x-h)

  d:= (fv1-fv2)/(2*h);

  Parse.NewValue(v,x);

  Derivative:= d;

end;

function CNLEquations.Jacobian(vars: svector; vals: dvector; funcs: svector): dmatrix;
var mr: dmatrix;
    i,j: integer;
begin

  SetLength(mr,Length(funcs),Length(funcs));

  for i:=0 to Length(funcs)-1 do
  begin
    Parse.Expression:= funcs[i];
    for j:=0 to Length(funcs)-1 do
    begin
      mr[i,j]:= Derivative(vars[j],vals[j]);
    end;
  end;

  Jacobian:= mr;

end;

function CNLEquations.FunctionM(vars: svector; vals: dvector; funcs: svector): dmatrix;
var mr: dmatrix;
    i: integer;
begin
  SetLength(mr,Length(funcs),1);

  for i:=0 to Length(funcs)-1 do
  begin
    Parse.Expression:= funcs[i];
    mr[i,0]:= Parse.Evaluate();
  end;

  FunctionM:= mr;

end;

function CNLEquations.NewtonRaphson(vars: svector; vals: dvector; funcs: svector): string;
var Reader: CReader;
    Matrix: CMatrix;
    fm: dmatrix;
    jm: dmatrix;
    xn: dmatrix;
    xn1: dmatrix;
    Error: double;
    PError: double;//
begin
  Reader:= CReader.create();
  Matrix:= CMatrix.create();

  xn1:= Reader.DVectorToDMatrixC(vals);
  Error:= Distance0(xn1);
  PError:= Distance0(xn1)+1;//

  repeat
  xn:= xn1;
  UpdateM(xn,vars,vals);
  fm:= FunctionM(vars,vals,funcs);
  jm:= Jacobian(vars,vals,funcs);
  xn1:= Matrix.Subtraction(xn,Matrix.Product(Matrix.Inverse(jm),fm));
  PError:= Error;//
  Error:= abs(Distance(xn1,xn));
  until (Error<=ErrorAllowed) or (Error>PError) ;
  //until (Error<=ErrorAllowed);

  xn1:= ApplyDecimal(xn1);


  if (Error>PError) then
  begin
     NewtonRaphson:= PrintNoConvergence(xn1);
     G_ConvergenceCriteria:= '--NoConverge--';//_
  end
  else
  begin
    NewtonRaphson:= PrintConvergence(xn1);
    G_ConvergenceCriteria:= '--Converge--';//_
  end;


  //NewtonRaphson:= PrintConvergence(xn1);

end;

end.


