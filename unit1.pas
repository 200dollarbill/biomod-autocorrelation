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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure normalize;
    procedure correlate;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Time, Ankle, Heel, Hip, Knee, Toe: array [-10000..10000] of Extended;
  Rxy, normalizedAvg, normalizedCC: array [0..10000] of Extended;
  sample: integer;

implementation

{$R *.lfm}

{ TForm1 }

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
    while not Eof(filename) do
    begin
      ReadLn(filename, Time[i], Heel[i], Toe[i], Hip[i], Knee[i], Ankle[i]);
      Inc(i);
      sample := i;
    end;
    CloseFile(filename);

    HeelSeries.Clear;
    ToeSeries.Clear;

    if sample > 0 then
    begin
      for i := 0 to sample - 1 do
      begin
        HeelSeries.AddXY(Time[i], Heel[i]);
        ToeSeries.AddXY(Time[i], Toe[i]);
      end;
    end;


  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     correlate;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
          normalize;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chart1.Title.Visible := True;
  Chart2.Title.Visible := True;
  Chart3.Title.Visible := True;
  Chart1.Legend.Visible := True;
  Chart1.Title.Text.Text := 'Heel and Toe';
  Chart2.Title.Text.Text := 'Correlation';
  Chart3.Title.Text.Text := 'Corellation Normalized';
end;

procedure TForm1.correlate;
var
  L, n: Integer;
  sigmaL: Extended;
  s: string;
begin

  Chart2LineSeries1.Clear;
  ListBox1.Items.Clear;
  ListBox1.Items.Add('Correlation');
  ListBox1.Items.Add('');

  for L := 0 to sample - 1 do
  begin
    sigmaL := 0.0;
    for n := L to sample - 1 do
      sigmaL := sigmaL + Heel[n] * Toe[n - L];

    Rxy[L] := sigmaL;

    Chart2LineSeries1.AddXY(L, Rxy[L]);

    s := Format('n = %d  Correlation Rxy = %s', [L, FloatToStrF(Rxy[L], ffFixed, 15, 6)]);
    ListBox1.Items.Add(s);
  end;
end;

procedure TForm1.normalize;
var
  L, n: Integer;
  rxx0: Extended;
  s: string;
begin


  Chart3LineSeries1.Clear;

  ListBox2.Items.Add('');
  ListBox2.Items.Add('Normalization');
  ListBox2.Items.Add('');

  rxx0 := 0.0;
  for n := 0 to sample - 1 do
    rxx0 := rxx0 + Heel[n] * Heel[n];

  for L := 0 to sample - 1 do
  begin
    if (sample - L) > 0 then
      normalizedAvg[L] := Rxy[L] / (sample - L)
    else
      normalizedAvg[L] := 0.0;

    if rxx0 > 0 then
      normalizedCC[L] := Rxy[L] / rxx0
    else
      normalizedCC[L] := 0.0;

    Chart3LineSeries1.AddXY(L, normalizedAvg[L]);

    s := Format('n = %d  Average correlation = %s  Corellation Coefficient = %s',
              [L,
               FloatToStrF(normalizedAvg[L], ffFixed, 15, 6),
               FloatToStrF(normalizedCC[L], ffFixed, 15, 6)]);
    ListBox2.Items.Add(s);
  end;
end;

end.
