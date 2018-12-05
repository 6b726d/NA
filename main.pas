unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uCmdBox, TAGraph, TASeries, TATools, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, Grids, ComCtrls, Math, ParseMath,
  SpecialParse, Class_Matrix, Class_Reader, Types, TAChartUtils, Class_Root;

type

  { TFormMain }

  TFormMain = class(TForm)
    Chart1: TChart;
    Chart1ConstantLineX: TConstantLine;
    Chart1ConstantLineY: TConstantLine;
    Chart1CTPoints: TLineSeries;
    Chart1RootPoints: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomClickTool1: TZoomClickTool;
    ChartToolset1ZoomClickTool2: TZoomClickTool;
    CmdBox1: TCmdBox;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    StringGrid1: TStringGrid;
    TreeView1: TTreeView;
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    function FindVariable(nVar: string; vGrid: smatrix): Integer;
    procedure AssignVariable(nVar: string; vVar: string; tVar: string);
    procedure ClearPlot;
    procedure ClearFunctions;
    procedure ClearPoints;
    procedure ClearAreas;
    procedure Plot2D(f: string; a: Double; b: Double; fcolor: string);
    procedure DrawPoint(px: Double; fx: string);
    procedure DrawPoints(vpx: dvector; fx: string);
    procedure DrawCTPoints(vpx: dvector; fx: string);
    procedure DrawArea(fx: string; gx: string; a: Double; b: Double);
    procedure PlotEDO();
    procedure InputAnalysis(SInput: string);
    function FunctionAnalysis(nFunc: string; StrList: TStringList; IE: string): string;
    procedure CmdBox1Input(ACmdBox: TCmdBox; Input: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Parse: TParseMath;
    FParse: TParseMath;
    FSParse: TSpecialParse;
    VariablesGrid: smatrix;
    FunctionList: TList;
    FunctionData: TStringList;
    fx1,fx2: string;//Functions
    px1,px2: double;//Points x
    fcnt: integer;//Counter
    fxi: string;//Function (Intersection)
    Root: CRoot;//Intersection
    AreaList: TList;
    h: Double;
    f_h: Double;
    a_h: Double;
    function func(x: Double; expr: string): Double;


  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ChartToolset1DataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);
var Expression: string;
    r: double;
    vRoots: dvector;
    Reader: CReader;
    DPoint: TDoublePoint;
begin
 DPoint:= Chart1.ImageToGraph(APoint);
 with ATool as TDataPointClickTool do begin
   if Series is TLineSeries then
   begin
     Expression:= FunctionData[TLineSeries(Series).Tag];
     ShowMessage(Expression + LineEnding +
     FloatToStr(DPoint.X) + ',' + FloatToStr(DPoint.Y));
     if fcnt=0 then
     begin
       fx1:= FunctionData[TLineSeries(Series).Tag];
       px1:= DPoint.X;
       fcnt:= 1;
     end
     else if fcnt=1 then
     begin
       fx2:= FunctionData[TLineSeries(Series).Tag];
       px2:= DPoint.X;
       fxi:= '('+fx1+')-('+fx2+')';
       //Intersection Begin
       Root:= CRoot.Create;
       Root.ErrorAllowed:= G_Error;
       r := Root.Execute(fxi,px1,px2,5,true);
       G_Roots:= Root.SRoot;
       Root.Destroy;
       Reader:= CReader.create();
       vRoots:= Reader.StringToDVector(G_Roots);
       if G_Roots<>'[]' then DrawCTPoints(vRoots,fx1);
       //Intersection End
       fcnt:= 0;
     end;
   end;
 end;
end;

function TFormMain.FindVariable(nVar: string; vGrid: smatrix): Integer;
var i: Integer;
  fvb: Boolean;
  pos: Integer;
begin
 fvb:= false;

 for i:=0 to Length(vGrid)-1 do begin
   if vGrid[i,0]=nVar then
   begin
     pos:= i;
     fvb:= true;
   end;
 end;

 if fvb=true then FindVariable:= pos
 else FindVariable:= Length(vGrid);

end;

procedure TFormMain.AssignVariable(nVar: string; vVar: string; tVar: string);
var pos: Integer;
begin
 pos:= FindVariable(nVar,VariablesGrid);
 //La variable no existe
 if pos = Length(VariablesGrid) then
 begin
   if tVar='integer' then Parse.AddVariable(nVar,StrToFloat(vVar))
   else if tVar='double' then Parse.AddVariable(nVar,StrToFloat(vVar))
   else if tVar='string' then Parse.AddString(nVar,vVar)
   else if tVar='array' then Parse.AddString(nVar,vVar)
   else CmdBox1.Writeln('Type Error');//No deberia suceder
   SetLength(VariablesGrid,pos+1,3);
   VariablesGrid[pos,0]:= nVar;
   VariablesGrid[pos,1]:= vVar;
   VariablesGrid[pos,2]:= tVar;
   StringGrid1.RowCount:= StringGrid1.RowCount+1;
   StringGrid1.Cells[0,pos+1]:= nVar;
   StringGrid1.Cells[1,pos+1]:= vVar;
   StringGrid1.Cells[2,pos+1]:= tVar;
 end
 else
 begin
   if tVar='integer' then Parse.NewValue(nVar,StrToFloat(vVar))
   else if tVar='double' then Parse.NewValue(nVar,StrToFloat(vVar))
   else if tVar='string' then Parse.ChangeString(nVar,vVar)
   else if tVar='array' then Parse.ChangeString(nVar,vVar)
   else CmdBox1.Writeln('Type Error');//No deberia suceder
   VariablesGrid[pos,0]:= nVar;
   VariablesGrid[pos,1]:= vVar;
   VariablesGrid[pos,2]:= tVar;
   StringGrid1.Cells[0,pos+1]:= nVar;
   StringGrid1.Cells[1,pos+1]:= vVar;
   StringGrid1.Cells[2,pos+1]:= tVar;
 end;
end;

function TFormMain.func(x: Double; expr: string): Double;
begin
 FParse.Expression:= expr;
 FParse.NewValue('x',x);
 func:= FParse.Evaluate();
end;

procedure TFormMain.ClearPlot;
begin
 ClearFunctions;
 ClearPoints;
 ClearAreas;
end;

procedure TFormMain.ClearFunctions;
var i: Integer;
begin
 for i:=0 to FunctionList.Count-1 do begin
   with TLineSeries(FunctionList[i]) do begin
     Clear;
     Active:= False;
   end;
 end;
 //ShowMessage(IntToStr(FunctionList.Count));
 //FunctionList.Clear;
 //FunctionData.Clear;
end;

procedure TFormMain.ClearPoints;
begin
 Chart1RootPoints.Clear;
 Chart1RootPoints.Active:= False;
 Chart1CTPoints.Clear;
 Chart1CTPoints.Active:= False;
end;

procedure TFormMain.ClearAreas;
var i: Integer;
begin
 for i:=0 to AreaList.Count-1 do begin
   with TAreaSeries(AreaList[i]) do begin
     Clear;
     Active:= False;
   end;
 end;
 //AreaList.Clear;
end;

//No dibuja ln(x),sqrt(x) (Causa Chart.ExtendY)
procedure TFormMain.Plot2D(f: string; a: Double; b: Double; fcolor: string);
var NewX,NewY: Double;
    EXmin,EXmax: Double;
    EYmax,EYmin: Double;
begin
 EYmax:= NegInfinity;
 EYmin:= Infinity;
 if a>b then
 begin
   EXMin:= b;
   EXMax:= a;
 end
 else
 begin
   EXMin:= a;
   EXMax:= b
 end;
 Chart1.Extent.XMin:= EXMin;//Chart
 Chart1.Extent.XMax:= EXMax;//Chart
 NewX:= EXMin;
 FunctionList.Add(TLineSeries.Create(Chart1));
 FunctionData.Add(f);//
 with TLineSeries(FunctionList[FunctionList.Count-1]) do begin
   Name:= 'plot2d_f' + IntToStr(FunctionList.Count);
   Tag:= FunctionList.Count-1;
 end;
 Chart1.AddSeries(TLineSeries(FunctionList[FunctionList.Count-1]));
 with TLineSeries(FunctionList[FunctionList.Count-1]) do begin
   Active:= True;
   LinePen.Color:= StringToColor(fcolor);//Try
   while NewX<EXMax do begin
     NewY:= func(NewX,f);
     if NewY>EYMax then EYMax:= NewY;//
     if NewY<EYMin then EYMin:= NewY;//
     AddXY(NewX,NewY);
     NewX:= NewX + f_h;
   end;
 end;
 Chart1.Extent.YMin:= EYMin;//Chart
 Chart1.Extent.YMax:= EYMax;//Chart
end;

procedure TFormMain.DrawPoint(px: double; fx: string);
begin
 Chart1RootPoints.Active:= True;
 Chart1RootPoints.ShowLines:= False;
 Chart1RootPoints.ShowPoints:= True;
 Chart1RootPoints.AddXY(px,func(px,fx));
end;

procedure TFormMain.DrawPoints(vpx: dvector; fx: string);
var i: Integer;
begin
 Chart1RootPoints.Active:= True;
 Chart1RootPoints.ShowLines:= False;
 Chart1RootPoints.ShowPoints:= True;
 for i:=0 to Length(vpx)-1 do Chart1RootPoints.AddXY(vpx[i],func(vpx[i],fx));
end;

procedure TFormMain.DrawCTPoints(vpx: dvector; fx: string);
var i: Integer;
begin
 Chart1CTPoints.Active:= True;
 Chart1CTPoints.ShowLines:= False;
 Chart1CTPoints.ShowPoints:= True;
 for i:=0 to Length(vpx)-1 do Chart1CTPoints.AddXY(vpx[i],func(vpx[i],fx));
end;

//No dibuja ln(x),sqrt(x)
procedure TFormMain.DrawArea(fx: string; gx:string; a: Double; b: Double);
var NewX,NewY,NewZ: Double;
    EXmin,EXmax: Double;
begin
 if a>b then
 begin
   EXMin:= b;
   EXMax:= a;
 end
 else
 begin
   EXMin:= a;
   EXMax:= b
 end;
 NewX:= EXMin;
 NewY:= func(NewX,fx);
 while NewX<EXMax do begin
   AreaList.Add(TAreaSeries.Create(Chart1));
   with TAreaSeries(AreaList[AreaList.Count-1]) do begin
     Name:= 'plot2d_a' + IntToStr(AreaList.Count);
     Tag:= AreaList.Count-1;
   end;
   Chart1.AddSeries(TAreaSeries(AreaList[AreaList.Count-1]));
   with TAreaSeries(AreaList[AreaList.Count-1]) do begin
     AreaBrush.Color:= clPurple;
     //AreaContourPen.Color:= clPurple;
     AreaContourPen.Style:= psClear;
     Transparency:= 125;//ERROR IN UBUNTU
     Active:= True;
     UseZeroLevel:= True;
     AddXY(NewX,NewY);
     NewX:= NewX + a_h;
     NewY:= func(NewX,fx);
     NewZ:= func(NewX,gx);
     AddXY(NewX,NewY);
     ZeroLevel:= NewZ;
   end;
 end;
end;

procedure TFormMain.PlotEDO();
var i: Integer;
    EXmin,EXmax: Double;
    EYmax,EYmin: Double;
begin
 EYmax:= NegInfinity;
 EYmin:= Infinity;
 if G_a>G_b then
 begin
   EXMin:= G_b;
   EXMax:= G_a;
 end
 else
 begin
   EXMin:= G_a;
   EXMax:= G_b;
 end;
 Chart1.Extent.XMin:= EXMin;//Chart
 Chart1.Extent.XMax:= EXMax;//Chart
 FunctionList.Add(TLineSeries.Create(Chart1));
 FunctionData.Add('0');//EDO
 with TLineSeries(FunctionList[FunctionList.Count-1]) do begin
   Name:= 'plot2d_f' + IntToStr(FunctionList.Count);
   Tag:= FunctionList.Count-1;
 end;
 Chart1.AddSeries(TLineSeries(FunctionList[FunctionList.Count-1]));
 with TLineSeries(FunctionList[FunctionList.Count-1]) do begin
   Active:= True;
   LinePen.Color:= clRed;
   for i:=0 to Length(G_DValues)-1 do
   begin
     if G_DValues[i,1]>EYMax then EYMax:= G_DValues[i,1];//
     if G_DValues[i,1]<EYMin then EYMin:= G_DValues[i,1];//
     AddXY(G_DValues[i,0],G_DValues[i,1]);
   end;
 end;
 Chart1.Extent.YMin:= EYMin;//Chart
 Chart1.Extent.YMax:= EYMax;//Chart
end;

procedure TFormMain.InputAnalysis(SInput: string);
var TSInput: string;                     //Trim(Input)
    ePos: Integer;                       //Posicion de =
    iPos: Integer;                       //Posiciones de vVar
    qPos: Integer;                       //Posicion de la comilla
    cPos: Integer;                       //Posicion del corchete
    StrList: TStringList;                //Parametros
    nFunc: string;                       //Nombre de la Funcion
    fData: string;                       //Valores a Evaluar (Special Input)
    nVar: string;                        //Nombre de la Variable
    vVar: string;                        //Valor de la Variable
    iValue: Integer;                     //TryStrToInt(sValue,iValue)
    fValue: Double;                      //TryStrToFloat(sValue,fValue)
    sValue: string;                      //Valor de la Funcion
    fpValue: Double;                     //Valor de Evaluacion (Float)
    spValue: string;                     //Valor de Evaluacion (String)
    siValue: string;                     //Valor de Evaluacion (SpecialInput)
begin
  StrList:= TStringList.Create;
  TSInput:= Trim(SInput);

  ePos:= Pos('=',TSInput);
  if ePos<>0 then
  begin
    nVar:= Trim(Copy(TSInput,1,ePos-1));
    vVar:= Trim(Copy(TSInput,ePos+1,Length(TSInput)-ePos));
    vVar:= StringReplace(vVar,' ',',',[rfReplaceAll,rfIgnoreCase]);//::
  end
  else
  begin
    vVar:= TSInput;
    vVar:= StringReplace(vVar,' ',',',[rfReplaceAll,rfIgnoreCase]);//::
  end;

  iPos:= Pos('(',vVar);
  //TSInput[ePos+1]<>#39 (Para las variables string con funciones -> fun(x))
  if (iPos<>0) and (TSInput[ePos+1]<>#39) then
  begin
    nFunc:= Trim(Copy(vVar,1,iPos-1));
    fData:= Copy(vVar,iPos+1,Length(vVar)-iPos-1);
    //Special Input begin
    if nFunc='polyroot' then
    begin
      try
        siValue:= FSParse.EvalPolyroot(fdata,VariablesGrid);
        CmdBox1.Writeln(siValue);
      except
        CmdBox1.Writeln('Parameters Error');
      end;
      if nVar<>'' then
      begin
        try
          AssignVariable(nVar,siValue,'string');
        except
          CmdBox1.Writeln('Assignment Error');
        end;
      end;
    end
    else if nFunc='polynomial' then
    begin
      try
        siValue:= FSParse.EvalPolynomial(fdata,VariablesGrid);
        CmdBox1.Writeln(siValue);
        CmdBox1.Writeln(G_ConvergenceCriteria);
      except
        CmdBox1.Writeln('Parameters Error');
      end;
      if nVar<>'' then
      begin
        try
          AssignVariable(nVar,siValue,'string');
        except
          CmdBox1.Writeln('Assignment Error');
        end;
      end;
    end
    else if nFunc='senl' then
    begin
      try
        siValue:= FSParse.EvalSENL(fdata,VariablesGrid);
        CmdBox1.Writeln(siValue);
        CmdBox1.Writeln(G_ConvergenceCriteria);
      except
        CmdBox1.Writeln('Parameters Error');
      end;
      if nVar<>'' then
      begin
        try
          AssignVariable(nVar,siValue,'array');
        except
          CmdBox1.Writeln('Assignment Error');
        end;
      end;
    end
    //Special Input end
    else
    begin
      StrList.Delimiter:=',';
      StrList.StrictDelimiter:= true;
      StrList.DelimitedText:= Copy(vVar,iPos+1,Length(vVar)-iPos-1);
      try
        sValue:= FunctionAnalysis(nFunc,StrList,vVar);
      except
        CmdBox1.Writeln('Parameters Error');
      end;
      if nVar<>'' then
      begin
        if (sValue<>'Nan') and (sValue<>'[]') then
        begin
          //TryStrToFloat(s,f);
          if TryStrToFloat(sValue,fValue)=true then
          begin
            //CmdBox1.Writeln('IsNumber');
            if nVar<>'' then
            begin
              try
                AssignVariable(nVar,sValue,'double');
              except
                CmdBox1.Writeln('Assignment Error');
              end;
            end;
          end
          else
          begin
            cPos:= Pos('[',sValue);
            if (cPos<>0) and (sValue[Length(sValue)]=']') then
            begin
              //CmdBox1.Writeln('IsArray');
              if nVar<>'' then
              begin
                try
                  AssignVariable(nVar,sValue,'array');
                except
                  CmdBox1.Writeln('Assignment Error');
                end;
              end;
            end
            else
            begin
              //CmdBox1.Writeln('IsString');
              if nVar<>'' then
              begin
                try
                  AssignVariable(nVar,sValue,'string');
                except
                  CmdBox1.Writeln('Assignment Error');
                end;
              end;
            end;
          end;
        end
        else CmdBox1.Writeln('Assignment Error');
      end;
    end
  end
  else
  begin
    if (nVar='f_h') and (vVar<>'Nan') then
    begin
      if TryStrToFloat(vVar,fValue)=true then
      begin
        if (fValue<=0.1) and (fValue>=0.001) then
        begin
          f_h:= fValue;
        end
        else CmdBox1.Writeln('0.001 <= f_h <=0.1');
        CmdBox1.Writeln('--Special--');
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar='a_h') and (vVar<>'Nan') then
    begin
      if TryStrToFloat(vVar,fValue)=true then
      begin
        if (fValue<=0.1) and (fValue>=0.001) then
        begin
          a_h:= fValue;
        end
        else CmdBox1.Writeln('0.001 <= a_h <= 0.1');
        CmdBox1.Writeln('--Special--');
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar='XMin') and (vVar<>'Nan') then
    begin
      if TryStrToFloat(vVar,fValue)=true then
      begin
        CmdBox1.Writeln('--Special--');
        Chart1.Extent.XMin:= fValue;
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar='XMax') and (vVar<>'Nan') then
    begin
      if TryStrToFloat(vVar,fValue)=true then
      begin
        CmdBox1.Writeln('--Special--');
        Chart1.Extent.XMax:= fValue;
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar='YMin') and (vVar<>'Nan') then
    begin
      if TryStrToFloat(vVar,fValue)=true then
      begin
        CmdBox1.Writeln('--Special--');
        Chart1.Extent.YMin:= fValue;
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar='YMax') and (vVar<>'Nan') then
    begin
      if TryStrToFloat(vVar,fValue)=true then
      begin
        CmdBox1.Writeln('--Special--');
        Chart1.Extent.YMax:= fValue;
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar='error') and (vVar<>'Nan') then
    begin
      if TryStrToFloat(vVar,fValue)=true then
      begin
        //CmdBox1.Writeln('IsNumber');
        if (fValue<1) and (fValue>0) then
        begin
          G_Error:= fValue;
          AssignVariable(nVar,vVar,'double');
        end
        else CmdBox1.Writeln('0<error<1');
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar='decimal') and (vVar<>'Nan') then
    begin
      if TryStrToInt(vVar,iValue)=true then
      begin
        //CmdBox1.Writeln('IsNumber');
        if (iValue>=2) and (iValue<=8) then
        begin
          G_Decimal:= iValue;
          AssignVariable(nVar,vVar,'integer');
        end
        else CmdBox1.Writeln('2 <= decimal <= 8');//8...
      end
      else CmdBox1.Writeln('Evaluation Error');
    end
    else if (nVar<>'') and (vVar<>'Nan') then
    begin
      qPos:= Pos(#39,vVar);//Comilla
      cPos:= Pos('[',vVar);//Corchete
      if (qPos<>0) and (vVar[Length(vVar)]=#39) then
      begin
        //CmdBox1.Writeln('IsString');
        vVar:= Copy(vVar,2,Length(vVar)-2);
        try
          AssignVariable(nVar,vVar,'string');
        except
          CmdBox1.Writeln('Assignment Error');
        end;
      end
      else if (cPos<>0) and (vVar[Length(vVar)]=']') then
      begin
        //CmdBox1.Writeln('IsArray');
        try
          AssignVariable(nVar,vVar,'array');
        except
          CmdBox1.Writeln('Assignment Error');
        end;
      end
      else
      begin
        Parse.Expression:= vVar;
        try
          fpValue:= RoundTo(Parse.Evaluate(),-G_Decimal);
          spValue:= FloatToStr(fpValue);
          CmdBox1.Writeln(spValue);
          //CmdBox1.Writeln('IsNumber');
          AssignVariable(nVar,spValue,'double');
        except
          CmdBox1.Writeln('Assignment Error');
        end;
      end;
    end
    else
    begin
      Parse.Expression:= vVar;
      try
        fpValue:= RoundTo(Parse.Evaluate(),-G_Decimal);
        spValue:= FloatToStr(fpValue);
        CmdBox1.Writeln(spValue);
      except
        CmdBox1.Writeln('Evaluation Error');
      end;
    end;
  end;

  StrList.Destroy;

end;

function TFormMain.FunctionAnalysis(nFunc: string; StrList: TStringList; IE: string): string;
var value: Double;
    sValue: string;
    vRoots: dvector;
    Reader: CReader;
begin
  Reader:= CReader.create();
  if nFunc='plot2d' then
  begin
    if StrList.Count=4 then
    begin
      //Mejorar
      IE:= nFunc+'('+StrList[0]+','+StrList[1]+','+StrList[2]+','+#39+StrList[3]+#39+')';
      //
      Parse.Expression:= IE;
      value:= Parse.Evaluate();
      CmdBox1.Writeln('----');
      Plot2D(G_f,G_a,G_b,G_cf);
      FunctionAnalysis:= 'Nan';
    end
    else
    begin
      CmdBox1.Writeln('The parameters do not match');
      FunctionAnalysis:= 'Nan';
    end;
  end
  else if nFunc='root' then
  begin
    if StrList.Count=3 then
    begin
      Parse.Expression:= IE;
      value:= Parse.Evaluate();
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      CmdBox1.Writeln(G_ConvergenceCriteria);
      Plot2D(G_f,G_a,G_b,'clBlue');
      if sValue<>'Nan' then DrawPoint(value,G_f);
      FunctionAnalysis:= sValue;
    end
    else if StrList.Count=4 then
    begin
      if (StrList[3]='true') or (StrList[3]='false') then
      begin
        Parse.Expression:= 'a'+IE;
        value:= Parse.Evaluate();
        CmdBox1.Writeln(G_Roots);
        Plot2D(G_f,G_a,G_b,'clBlue');
        vRoots:= Reader.StringToDVector(G_Roots);
        if G_Roots<>'[]' then DrawPoints(vRoots,G_f);
        FunctionAnalysis:= G_Roots;
      end
      else
      begin
        Parse.Expression:= 'z'+IE;
        value:= Parse.Evaluate();
        sValue:= FloatToStr(value);
        CmdBox1.Writeln(sValue);
        CmdBox1.Writeln(G_ConvergenceCriteria);
        Plot2D(G_f,G_a,G_b,'clBlue');
        if sValue<>'Nan' then DrawPoint(value,G_f);
        FunctionAnalysis:= sValue;
      end;
    end
    else if StrList.Count=5 then
    begin
      if (StrList[3]='true') or (StrList[3]='false') then
      begin
        Parse.Expression:= 'az'+IE;
        value:= Parse.Evaluate();
        CmdBox1.Writeln(G_Roots);
        Plot2D(G_f,G_a,G_b,'clBlue');
        vRoots:= Reader.StringToDVector(G_Roots);
        if G_Roots<>'[]' then DrawPoints(vRoots,G_f);
        FunctionAnalysis:= G_Roots;
      end
      else
      begin
        Parse.Expression:= 'za'+IE;
        value:= Parse.Evaluate();
        CmdBox1.Writeln(G_Roots);
        Plot2D(G_f,G_a,G_b,'clBlue');
        vRoots:= Reader.StringToDVector(G_Roots);
        if G_Roots<>'[]' then DrawPoints(vRoots,G_f);
        FunctionAnalysis:= G_Roots;
      end;
    end
    else
    begin
      CmdBox1.Writeln('The parameters do not match');
      FunctionAnalysis:= 'Nan';
    end;
  end
  else if nFunc='intersection' then
  begin
    if StrList.Count=6 then
    begin
      //Mejorar
      IE:= nFunc+ '(' +StrList[0]+','+StrList[1]+','+StrList[2]+','+StrList[3]+',';
      IE:= IE + #39+StrList[4]+#39+ ',' +#39+StrList[5]+#39+ ')';
      //
      Parse.Expression:= IE;
      value:= Parse.Evaluate();
      CmdBox1.Writeln(G_Roots);
      Plot2D(G_f,G_a,G_b,G_cf);
      Plot2D(G_g,G_a,G_b,G_cg);
      vRoots:= Reader.StringToDVector(G_Roots);
      if G_Roots<>'[]' then DrawPoints(vRoots,G_f);
      FunctionAnalysis:= G_Roots;
    end
    else if StrList.Count=7 then
    begin
      //Mejorar
      IE:= nFunc+ '(' +StrList[0]+','+StrList[1]+','+StrList[2]+','+StrList[3]+',';
      IE:= IE + #39+StrList[4]+#39+ ',' +#39+StrList[5]+#39+ ',' +StrList[6]+ ')';
      //
      Parse.Expression:= 'z'+IE;
      value:= Parse.Evaluate();
      CmdBox1.Writeln(G_Roots);
      Plot2D(G_f,G_a,G_b,G_cf);
      Plot2D(G_g,G_a,G_b,G_cg);
      vRoots:= Reader.StringToDVector(G_Roots);
      if G_Roots<>'[]' then DrawPoints(vRoots,G_f);
      FunctionAnalysis:= G_Roots;
    end
    else
    begin
      CmdBox1.Writeln('The parameters do not match');
      FunctionAnalysis:= 'Nan';
    end;
  end
  else if nFunc='integral' then
  begin
    if StrList.Count=3 then
    begin
      Parse.Expression:= IE;
      value:= Parse.Evaluate();
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      Plot2D(G_f,G_a,G_b,'clBlue');
      DrawArea(G_f,'0',G_a,G_b);
      FunctionAnalysis:= sValue;
    end
    else if StrList.Count=4 then
    begin
      Parse.Expression:= 'z'+IE;
      value:= Parse.Evaluate();
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      Plot2D(G_f,G_a,G_b,'clBlue');
      DrawArea(G_f,'0',G_a,G_b);
      FunctionAnalysis:= sValue;
    end
    else
    begin
      CmdBox1.Writeln('The parameters do not match');
      FunctionAnalysis:= 'Nan';
    end;
  end
  else if nFunc='area' then
  begin
    if StrList.Count=4 then
    begin
      Parse.Expression:= IE;
      value:= Parse.Evaluate();
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      Plot2D(G_f,G_a,G_b,'clBlue');
      Plot2D(G_g,G_a,G_b,'clRed');
      DrawArea(G_f,G_g,G_a,G_b);
      FunctionAnalysis:= sValue;
    end
    else if StrList.Count=5 then
    begin
      Parse.Expression:= 'z'+IE;
      value:= Parse.Evaluate();
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      Plot2D(G_f,G_a,G_b,'clBlue');
      Plot2D(G_g,G_a,G_b,'clRed');
      DrawArea(G_f,G_g,G_a,G_b);
      FunctionAnalysis:= sValue;
    end
    else
    begin
      CmdBox1.Writeln('The parameters do not match');
      FunctionAnalysis:= 'Nan';
    end;
  end
  else if nFunc='edo' then
  begin
    if StrList.Count=4 then
    begin
      Parse.Expression:= IE;
      value:= Parse.Evaluate();
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      PlotEDO;
      FunctionAnalysis:= sValue;
    end
    else if StrList.Count=5 then
    begin
      Parse.Expression:= 'z'+IE;
      value:= Parse.Evaluate();
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      PlotEDO;
      FunctionAnalysis:= sValue;
    end
    else
    begin
      CmdBox1.Writeln('The parameters do not match');
      FunctionAnalysis:= 'Nan';
    end;
  end
  else if nFunc='eval' then
  begin
    if StrList.Count=1 then
    begin
      Parse.Expression:= IE;
      value:= Parse.Evaluate();
      Parse.Expression:= G_f;
      value:= RoundTo(Parse.Evaluate(),-G_Decimal);
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      FunctionAnalysis:= sValue;
    end
    else
    begin
      CmdBox1.Writeln('The parameters do not match');
      FunctionAnalysis:= 'Nan';
    end;
  end
  else
  begin
    Parse.Expression:= IE;
    try
      value:= RoundTo(Parse.Evaluate(),-G_Decimal);
      sValue:= FloatToStr(value);
      CmdBox1.Writeln(sValue);
      FunctionAnalysis:= sValue;
    except
      CmdBox1.Writeln('Evaluation Error');
      FunctionAnalysis:= 'Nan';
    end;
  end;
end;

///Create
procedure TFormMain.FormCreate(Sender: TObject);
begin
  CmdBox1.TextColors(clBlack,clWhite);
  CmdBox1.StartRead(clBlack,clWhite,'>>>',clBlack,clWhite);
  //Chart Begin
  G_a:= -10;
  G_b:= 10;
  G_c:= -5;
  G_d:= 5;
  //Chart End
  G_ConvergenceCriteria:= '--Converge--';
  G_Decimal:= 4;
  G_Error:= 0.001;
  Parse:= TParseMath.create;
  Parse.AddVariable('error',0.001);
  Parse.AddVariable('decimal',4);
  //VarablesGrid Begin
  SetLength(VariablesGrid,2,3);
  VariablesGrid[0,0]:= 'error';
  VariablesGrid[0,1]:= '0.001';
  VariablesGrid[0,2]:= 'double';
  VariablesGrid[1,0]:= 'decimal';
  VariablesGrid[1,1]:= '4';
  VariablesGrid[1,2]:= 'integer';
  StringGrid1.Cells[0,1]:= 'error';
  StringGrid1.Cells[1,1]:= '0.001';
  StringGrid1.Cells[2,1]:= 'double';
  StringGrid1.Cells[0,2]:= 'decimal';
  StringGrid1.Cells[1,2]:= '4';
  StringGrid1.Cells[2,2]:= 'integer';
  //VarablesGrid End
  h:= 0.001;
  f_h:= 0.001;
  a_h:= 0.01;
  FunctionList:=TList.Create;
  FunctionData:=TStringList.Create;
  AreaList:=TList.Create;
  FParse:= TParseMath.create;
  FParse.AddVariable('x',0);
end;

//Destroy
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FunctionList.Destroy;
  FunctionData.Destroy;
  AreaList.Destroy;
  FParse.destroy;
end;

procedure TFormMain.CmdBox1Input(ACmdBox: TCmdBox; Input: string);
begin
  case Input of
       'help': begin
         CmdBox1.Writeln('clearcmd: Clear CmdBox');
         CmdBox1.Writeln('clearhistory: Clear History');
         CmdBox1.Writeln('plot: Unhide Chart');
         CmdBox1.Writeln('noplot: Hide Chart');
         CmdBox1.Writeln('clearplot: Clear Plot');
         CmdBox1.Writeln('clearfunctions: Clear Funcions');
         CmdBox1.Writeln('clearpoints: Clear Points');
         CmdBox1.Writeln('clearareas: Clear Areas');
         CmdBox1.Writeln('rchartsize: Restart Chart(Heigth,Width)');
         CmdBox1.Writeln('rchart: Restart Chart');
         CmdBox1.Writeln('help-funcs: Help_Functions');
       end;
       'help-funcs': begin
         CmdBox1.Writeln('plot2d(f,a,b,color)');
         CmdBox1.Writeln('root(f,a,b)');
         CmdBox1.Writeln('root(f,a,b,method)');
         CmdBox1.Writeln('root(f,a,b,boolean)');
         CmdBox1.Writeln('root(f,a,b,method,boolean)');
         CmdBox1.Writeln('root(f,a,b,boolean,method)');
         CmdBox1.Writeln('polyroot([x0,x1,x2,...,xn])');
         CmdBox1.Writeln('polynomial([x0,x1,x2,...,xn],[y0,y1,y2,...,yn])');
         CmdBox1.Writeln('senl([x,y,z,...],[f0,f1,f2,...],[x0,y0,z0,...])');
         CmdBox1.Writeln('intersection(f,g,a,b,colorf,colorg)');
         CmdBox1.Writeln('intersection(f,g,a,b,colorf,colorg,method)');
         CmdBox1.Writeln('integral(f,a,b)');
         CmdBox1.Writeln('integral(f,a,b,method)');
         CmdBox1.Writeln('area(f,g,a,b)');
         CmdBox1.Writeln('area(f,g,a,b,method)');
         CmdBox1.Writeln('edo(df,x0,y0,xn)');
         CmdBox1.Writeln('edo(df,x0,y0,xn,method)');
         CmdBox1.Writeln('help-root');
         CmdBox1.Writeln('help-integral');
         CmdBox1.Writeln('help-edo');
       end;
       'help-root': begin
         CmdBox1.Writeln('Boolean :: true(AllRoots):false(OneRoot)');
         CmdBox1.Writeln('0: Bisection');
         CmdBox1.Writeln('1: FalsePosition');
         CmdBox1.Writeln('2: Secant');
         CmdBox1.Writeln('3: Secant ConvergenceCriteria');
         CmdBox1.Writeln('other: Default (Bolzano+Secant)');
       end;
       'help-integral': begin
         CmdBox1.Writeln('0: Trapezoidal');
         CmdBox1.Writeln('1: Simpson13');
         CmdBox1.Writeln('other: Simpson38');
       end;
       'help-edo': begin
         CmdBox1.Writeln('0: Euler');
         CmdBox1.Writeln('1: EulerHeun');
         CmdBox1.Writeln('2: RungeKutta (RK4)');
         CmdBox1.Writeln('3: RKDormandPrince');
         CmdBox1.Writeln('4: RKFehlberg');
         CmdBox1.Writeln('5: RKCashKarp');
         CmdBox1.Writeln('other: RKDormandPrince');
       end;
       'rchartsize': begin
         Chart1.Extent.XMin:= -10;
         Chart1.Extent.XMax:= 10;
         Chart1.Extent.YMin:= -5;
         Chart1.Extent.YMax:= 5;
       end;
       'rchart': begin
         Chart1.Extent.XMin:= -10;
         Chart1.Extent.XMax:= 10;
         Chart1.Extent.YMin:= -5;
         Chart1.Extent.YMax:= 5;
         ClearPlot;
       end;
       'exit': Application.Terminate;
       'clearcmd': CmdBox1.Clear;
       'clearhistory': CmdBox1.ClearHistory;
       'plot': Chart1.Visible:= True;
       'noplot': Chart1.Visible:= False;
       'clearplot': ClearPlot;
       'clearfunctions': ClearFunctions;
       'clearpoints': ClearPoints;
       'clearareas': ClearAreas;
       else
         begin
           InputAnalysis(Input);
         end;
  end;
  CmdBox1.StartRead(clBlack,clWhite,'>>>',clBlack,clWhite);
end;

end.

