unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, TAGraph,
  TASeries, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Chart1: TChart;
    GroupBox2: TGroupBox;
    HeelSeries: TLineSeries;
    ListBox2: TListBox;
    ToeSeries: TLineSeries;
    Chart2: TChart;
    Chart2LineSeries1: TLineSeries;
    Chart3: TChart;
    Chart3LineSeries1: TLineSeries;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    ScrollBar1: TScrollBar;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateShiftedToeSeries;
    procedure correlate;
    procedure normalize;
    procedure UpdateCalculations(NewShift: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Time, Ankle, Heel, Hip, Knee, Toe: array [0..10000] of Extended;
  Rxy, normalizedCC: array [0..20001] of Extended;
  sample: integer;
  ShiftVal: integer = 0; // Current shift value

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chart1.Title.Visible := True;
  Chart2.Title.Visible := True;
  Chart3.Title.Visible := True;
  Chart1.Legend.Visible := True;
  Chart1.Title.Text.Text := 'Heel and Toe (Toe is shifted)';
  Chart2.Title.Text.Text := 'Cross-Correlation';
  Chart3.Title.Text.Text := 'Normalized Cross-Correlation';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  filename: TextFile;
begin
  if OpenDialog1.Execute then
  begin
    i := 0;
    AssignFile(filename, OpenDialog1.FileName);
    Reset(filename);
    while (not Eof(filename)) and (i <= High(Heel)) do
    begin
      ReadLn(filename, Time[i], Heel[i], Toe[i], Hip[i], Knee[i], Ankle[i]);
      Inc(i);
    end;
    sample := i;
    CloseFile(filename);

    HeelSeries.Clear;

    if sample > 0 then
    begin
      for i := 0 to sample - 1 do
      begin
        HeelSeries.AddXY(Time[i], Heel[i]);
      end;

      ShiftVal := 0;
      ScrollBar1.Min := -(sample - 1);
      ScrollBar1.Max := (sample - 1);
      ScrollBar1.Position := ShiftVal;
      ScrollBar1.PageSize := sample div 10;
      Edit1.Text := IntToStr(ShiftVal);

      UpdateShiftedToeSeries;
      correlate;
      normalize;
    end;
  end;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  UpdateCalculations(ScrollBar1.Position);
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  NewShift: Integer;
begin
  if TryStrToInt(Trim(Edit1.Text), NewShift) then
    UpdateCalculations(NewShift);
end;

procedure TForm1.UpdateCalculations(NewShift: Integer);
begin
  if (sample <= 0) or (NewShift = ShiftVal) then Exit;

  if NewShift < ScrollBar1.Min then NewShift := ScrollBar1.Min;
  if NewShift > ScrollBar1.Max then NewShift := ScrollBar1.Max;
  ShiftVal := NewShift;

  if ScrollBar1.Position <> ShiftVal then
    ScrollBar1.Position := ShiftVal;
  if Edit1.Text <> IntToStr(ShiftVal) then
    Edit1.Text := IntToStr(ShiftVal);

  UpdateShiftedToeSeries;

  correlate;
  normalize;
end;

procedure TForm1.UpdateShiftedToeSeries;
var
  i, idx: Integer;
  val: Extended;
begin
  if sample <= 0 then Exit;

  ToeSeries.Clear;
  for i := 0 to sample - 1 do
  begin
    idx := i - ShiftVal;
    if (idx >= 0) and (idx < sample) then
      val := Toe[idx]
    else
      val := 0.0;
    ToeSeries.AddXY(Time[i], val);
  end;
end;

procedure TForm1.correlate;
var
  L, n, yIndex: Integer;
  sigmaL: Extended;
  LagMin, LagMax, offset: Integer;
begin
  if sample <= 0 then Exit;

  Chart2LineSeries1.Clear;

  LagMin := -(sample - 1);
  LagMax := sample - 1;
  offset := sample - 1;

  for L := LagMin to LagMax do
  begin
    sigmaL := 0.0;
    for n := 0 to sample - 1 do
    begin
      yIndex := n - L + ShiftVal;
      if (yIndex >= 0) and (yIndex < sample) then
        sigmaL := sigmaL + Heel[n] * Toe[yIndex];
    end;
    Rxy[L + offset] := sigmaL;
    Chart2LineSeries1.AddXY(L, Rxy[L + offset]);
  end;
end;

procedure TForm1.normalize;
var
  L, n: Integer;
  SumSqHeel, SumSqToe, NormDenom: Extended;
  LagMin, LagMax, offset: Integer;
begin
  if sample <= 0 then Exit;

  Chart3LineSeries1.Clear;
  SumSqHeel := 0.0;
  SumSqToe := 0.0;
  for n := 0 to sample - 1 do
  begin
    SumSqHeel := SumSqHeel + Sqr(Heel[n]);
    SumSqToe := SumSqToe + Sqr(Toe[n]);
  end;

  NormDenom := Sqrt(SumSqHeel * SumSqToe);

  LagMin := -(sample - 1);
  LagMax := sample - 1;
  offset := sample - 1;

  for L := LagMin to LagMax do
  begin
    if NormDenom > 1E-9 then
      normalizedCC[L + offset] := Rxy[L + offset] / NormDenom
    else
      normalizedCC[L + offset] := 0.0;

    Chart3LineSeries1.AddXY(L, normalizedCC[L + offset]);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  L: Integer;
  s: string;
  LagMin, LagMax, offset: Integer;
begin
  // This button now populates the ListBox with correlation results
  if sample <= 0 then Exit;

  LagMin := -(sample - 1);
  LagMax := sample - 1;
  offset := sample - 1;

  ListBox1.Clear;
  ListBox1.Items.Add('Lag, Correlation Rxy');

  for L := LagMin to LagMax do
  begin
    s := Format('L = %4d  Rxy = %s', [L, FloatToStrF(Rxy[L + offset], ffFixed, 15, 6)]);
    ListBox1.Items.Add(s);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  L: Integer;
  s: string;
  LagMin, LagMax, offset: Integer;
begin
  if sample <= 0 then Exit;

  LagMin := -(sample - 1);
  LagMax := sample - 1;
  offset := sample - 1;

  ListBox2.Clear;
  ListBox2.Items.Add('Lag, Normalized Coefficient');

  for L := LagMin to LagMax do
  begin
    s := Format('L = %4d  Coefficient = %s', [L, FloatToStrF(normalizedCC[L + offset], ffFixed, 15, 9)]);
    ListBox2.Items.Add(s);
  end;
end;

end.
