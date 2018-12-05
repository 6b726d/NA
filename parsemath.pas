unit ParseMath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, fpexprpars, Dialogs, Class_Matrix, Class_Reader,
  Class_Root, Class_Integral, Class_Area, Class_Differential;

var
  //G_Value: String;
  G_Decimal: Integer;
  G_Error: Double;
  G_Roots: String;
  G_DValues: dmatrix;
  //Arreglar
  G_f: String;
  G_g: String;
  G_a: Double;//TChartX
  G_b: Double;//TChartX
  G_c: Double;//TChartY
  G_d: Double;//TChartY
  G_cf: String;
  G_cg: String;
  //Convergence Criteria
  G_ConvergenceCriteria: String;

type
  TParseMath = Class

  Private
      FParser: TFPExpressionParser;
      identifier: array of TFPExprIdentifierDef;
      Procedure AddFunctions();


  Public

      Expression: string;
      function NewValue( Variable:string; Value: Double ): Double;
      function ChangeString( Variable:string; Value: string ): string;
      procedure AddVariable( Variable: string; Value: Double );
      procedure AddString( Variable: string; Value: string );
      function Evaluate(): Double;
      constructor create();
      destructor destroy;

  end;

implementation

constructor TParseMath.create;
begin
   FParser:= TFPExpressionParser.Create( nil );
   FParser.Builtins := [ bcMath ];
   AddFunctions();
end;

destructor TParseMath.destroy;
begin
    FParser.Destroy;
end;

function TParseMath.NewValue( Variable: string; Value: Double ): Double;
begin
    FParser.IdentifierByName(Variable).AsFloat:= Value;

end;

function TParseMath.ChangeString( Variable: string; Value: string ): string;
begin
    FParser.IdentifierByName(Variable).AsString:= Value;

end;

function TParseMath.Evaluate(): Double;
begin
     FParser.Expression:= Expression;
     Result:= ArgToFloat( FParser.Evaluate );
end;

function IsNumber(AValue: TExprFloat): Boolean;
begin
  result := not (IsNaN(AValue) or IsInfinite(AValue) or IsInfinite(-AValue));
end;

Procedure ExprTan( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and ((frac(x - 0.5) / pi) <> 0.0) then
      Result.resFloat := tan(x)

   else
     Result.resFloat := NaN;
end;

Procedure ExprSin( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   Result.resFloat := sin(x)

end;

Procedure ExprCos( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
   x := ArgToFloat( Args[ 0 ] );
   Result.resFloat := cos(x)

end;

Procedure ExprLn( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
    x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and (x > 0) then
      Result.resFloat := ln(x)

   else
     Result.resFloat := NaN;

end;

Procedure ExprLog( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
    x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and (x > 0) then
      Result.resFloat := ln(x) / ln(10)

   else
     Result.resFloat := NaN;

end;

Procedure ExprSQRT( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
    x := ArgToFloat( Args[ 0 ] );
   if IsNumber(x) and (x >= 0) then
      Result.resFloat := sqrt(x)

   else
     Result.resFloat := NaN;

end;
//Para evitar errores con ',', Usar x^2
(*
Procedure ExprPower( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x,y: Double;
begin
    x := ArgToFloat( Args[ 0 ] );
    y := ArgToFloat( Args[ 1 ] );

    Result.resFloat := power(x,y);

end;
*)

Procedure ExprEval( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
begin
    f:= Args[0].ResString;

    G_f:= f;

    Result.resFloat := NaN;

end;

Procedure ExprPlot2D( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  cf: string;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);
   cf:= Args[3].ResString;

   G_f:= f;
   G_a:= a;
   G_b:= b;
   G_cf:= cf;

   Result.ResFloat := NaN;//No se Usa

end;

Procedure ExprRoot( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  Root: CRoot;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);

   G_f:= f;
   G_a:= a;
   G_b:= b;

   Root:= CRoot.Create;
   Root.ErrorAllowed:= G_Error;
   Result.ResFloat := Root.Execute(f,a,b,5,false);

   Root.Destroy;

end;

Procedure ExprZRoot( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  m: Integer;
  r: Double;
  Root: CRoot;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);
   m:= Args[3].ResInteger;

   G_f:= f;
   G_a:= a;
   G_b:= b;

   Root:= CRoot.Create;
   Root.ErrorAllowed:= G_Error;
   r:= Root.Execute(f,a,b,m,false);
   if m=2 then
   begin
     if r<a then G_a:= r-1
     else if r>b then G_b:= r+1;
   end;

   Result.ResFloat := r;

   Root.Destroy;

end;

Procedure ExprARoot( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  tf: Boolean;
  Root: CRoot;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);
   tf:= Args[3].ResBoolean;

   G_f:= f;
   G_a:= a;
   G_b:= b;

   Root:= CRoot.Create;
   Root.ErrorAllowed:= G_Error;
   Result.ResFloat := Root.Execute(f,a,b,5,tf);
   G_Roots:= Root.SRoot;

   Root.Destroy;

end;

Procedure ExprZARoot( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  m: Integer;
  tf: Boolean;
  Root: CRoot;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);
   m:= Args[3].ResInteger;
   tf:= Args[4].ResBoolean;

   G_f:= f;
   G_a:= a;
   G_b:= b;

   Root:= CRoot.Create;
   Root.ErrorAllowed:= G_Error;
   Result.ResFloat := Root.Execute(f,a,b,m,tf);
   G_Roots:= Root.SRoot;

   Root.Destroy;

end;

Procedure ExprAZRoot( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  tf: Boolean;
  m: Integer;
  Root: CRoot;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);
   tf:= Args[3].ResBoolean;
   m:= Args[4].ResInteger;

   G_f:= f;
   G_a:= a;
   G_b:= b;

   Root:= CRoot.Create;
   Root.ErrorAllowed:= G_Error;
   Result.ResFloat := Root.Execute(f,a,b,m,tf);
   G_Roots:= Root.SRoot;

   Root.Destroy;

end;

Procedure ExprIntersection( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  g: string;
  a: Double;
  b: Double;
  cf: string;
  cg: string;
  fg: string;
  Root: CRoot;
begin
   f:= Args[0].ResString;
   g:= Args[1].ResString;
   a:= ArgToFloat(Args[2]);
   b:= ArgToFloat(Args[3]);
   cf:= Args[4].ResString;
   cg:= Args[5].ResString;

   G_f:= f;
   G_g:= g;
   G_a:= a;
   G_b:= b;
   G_cf:= cf;
   G_cg:= cg;

   fg:= '(' + f + ')-(' + g + ')';

   Root:= CRoot.Create;
   Root.ErrorAllowed:= G_Error;
   Result.ResFloat := Root.Execute(fg,a,b,5,true);
   G_Roots:= Root.SRoot;

   Root.Destroy;

end;

Procedure ExprZIntersection( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  g: string;
  a: Double;
  b: Double;
  cf: string;
  cg: string;
  m: Integer;
  fg: string;
  Root: CRoot;
begin
   f:= Args[0].ResString;
   g:= Args[1].ResString;
   a:= ArgToFloat(Args[2]);
   b:= ArgToFloat(Args[3]);
   cf:= Args[4].ResString;
   cg:= Args[5].ResString;
   m:= Args[6].ResInteger;

   G_f:= f;
   G_g:= g;
   G_a:= a;
   G_b:= b;
   G_cf:= cf;
   G_cg:= cg;

   fg:= '(' + f + ')-(' + g + ')';

   Root:= CRoot.Create;
   Root.ErrorAllowed:= G_Error;
   Result.ResFloat := Root.Execute(fg,a,b,m,true);
   G_Roots:= Root.SRoot;

   Root.Destroy;

end;

Procedure ExprIntegral( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  Integral: CIntegral;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);

   G_f:= f;
   G_a:= a;
   G_b:= b;

   Integral:= CIntegral.Create;
   Integral.ErrorAllowed:= G_Error;
   Result.ResFloat := Integral.Execute(f,a,b,3);

   Integral.Destroy;

end;

Procedure ExprZIntegral( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  a: Double;
  b: Double;
  m: Integer;
  Integral: CIntegral;
begin
   f:= Args[0].ResString;
   a:= ArgToFloat(Args[1]);
   b:= ArgToFloat(Args[2]);
   m:= Args[3].ResInteger;

   G_f:= f;
   G_a:= a;
   G_b:= b;

   Integral:= CIntegral.Create;
   Integral.ErrorAllowed:= G_Error;
   Result.ResFloat := Integral.Execute(f,a,b,m);

   Integral.Destroy;

end;

Procedure ExprArea( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  g: string;
  a: Double;
  b: Double;
  fg: string;
  Area: CArea;
begin
   f:= Args[0].ResString;
   g:= Args[1].ResString;
   a:= ArgToFloat(Args[2]);
   b:= ArgToFloat(Args[3]);

   G_f:= f;
   G_g:= g;
   G_a:= a;
   G_b:= b;

   fg:= '(' + f + ')-(' + g + ')';

   Area:= CArea.Create;
   Area.ErrorAllowed:= G_Error;
   Result.ResFloat := Area.Execute(fg,a,b,3);

   Area.Destroy;

end;

Procedure ExprZArea( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  f: string;
  g: string;
  a: Double;
  b: Double;
  m: Integer;
  fg: string;
  Area: CArea;
begin
   f:= Args[0].ResString;
   g:= Args[1].ResString;
   a:= ArgToFloat(Args[2]);
   b:= ArgToFloat(Args[3]);
   m:= Args[4].ResInteger;

   G_f:= f;
   G_g:= g;
   G_a:= a;
   G_b:= b;

   fg:= '(' + f + ')-(' + g + ')';

   Area:= CArea.Create;
   Area.ErrorAllowed:= G_Error;
   Result.ResFloat := Area.Execute(fg,a,b,m);

   Area.Destroy;

end;

Procedure ExprEDO( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  df: string;
  x0: Double;
  y0: Double;
  xn: Double;
  Differential: CDifferential;
begin
   df:= Args[0].ResString;
   x0:= ArgToFloat(Args[1]);
   y0:= ArgToFloat(Args[2]);
   xn:= ArgToFloat(Args[3]);

   G_a:= x0;//Chart
   G_b:= xn;//Chart

   Differential:= CDifferential.Create;
   Differential.ErrorAllowed:= G_Error;
   Result.ResFloat := Differential.Execute(df,x0,y0,xn,7);

   Differential.Destroy;

end;

Procedure ExprZEDO( var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  df: string;
  x0: Double;
  y0: Double;
  xn: Double;
  m: Integer;
  Differential: CDifferential;
begin
   df:= Args[0].ResString;
   x0:= ArgToFloat(Args[1]);
   y0:= ArgToFloat(Args[2]);
   xn:= ArgToFloat(Args[3]);
   m:= Args[4].ResInteger;

   G_a:= x0;//Chart
   G_b:= xn;//Chart

   Differential:= CDifferential.Create;
   Differential.ErrorAllowed:= G_Error;
   Result.ResFloat := Differential.Execute(df,x0,y0,xn,m);

   Differential.Destroy;

end;


Procedure TParseMath.AddFunctions();
begin
   with FParser.Identifiers do begin
       AddFunction('tan', 'F', 'F', @ExprTan);
       AddFunction('sin', 'F', 'F', @ExprSin);
       AddFunction('sen', 'F', 'F', @ExprSin);
       AddFunction('cos', 'F', 'F', @ExprCos);
       AddFunction('ln', 'F', 'F', @ExprLn);
       AddFunction('log', 'F', 'F', @ExprLog);
       AddFunction('sqrt', 'F', 'F', @ExprSQRT);
       //AddFunction('power', 'F', 'FF', @ExprPower);
       AddFunction('eval', 'F', 'S', @ExprEval);
       AddFunction('plot2d','F','SFFS', @ExprPlot2D);
       AddFunction('root', 'F', 'SFF', @ExprRoot);
       AddFunction('zroot', 'F', 'SFFI', @ExprZRoot);
       AddFunction('aroot', 'F', 'SFFB', @ExprARoot);
       AddFunction('zaroot', 'F', 'SFFIB', @ExprZARoot);
       AddFunction('azroot', 'F', 'SFFBI', @ExprAZRoot);
       AddFunction('intersection','F','SSFFSS', @ExprIntersection);
       AddFunction('zintersection','F','SSFFSSI', @ExprZIntersection);
       AddFunction('integral','F','SFF', @ExprIntegral);
       AddFunction('zintegral','F','SFFI', @ExprZIntegral);
       AddFunction('area','F','SSFF', @ExprArea);
       AddFunction('zarea','F','SSFFI', @ExprZArea);
       AddFunction('edo','F','SFFF', @ExprEDO);
       AddFunction('zedo','F','SFFFI', @ExprZEDO);

   end

end;

procedure TParseMath.AddVariable( Variable: string; Value: Double );
var Len: Integer;
begin
   Len:= length( identifier ) + 1;
   SetLength( identifier, Len ) ;
   identifier[ Len - 1 ]:= FParser.Identifiers.AddFloatVariable( Variable, Value);

end;

procedure TParseMath.AddString( Variable: string; Value: string );
var Len: Integer;
begin
   Len:= length( identifier ) + 1;
   SetLength( identifier, Len ) ;

   identifier[ Len - 1 ]:= FParser.Identifiers.AddStringVariable( Variable, Value);
end;

end.

