unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLProc,
  ExtCtrls, MultiPanel, MultiplexSlider, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    MultiPanel1: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Paint(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
begin
 canvas.Pen.Color:=cllime;
 canvas.Pen.Width:=5;
 canvas.Line(0,0,width,height);

 canvas.Pen.Color:=clred;
 canvas.Pen.Width:=4;
 canvas.Line(0,50,width,50);
 //DebugLn('FormPaint');

 MultiPanel1.FParentBmp.Canvas.Line(0,10,width,10);
 Multipanel1.Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  MultiPanel1.AnimationSpeed:=0.005;
  MultiPanel1.Appear:=true;
 // MultiPanel2.Appear:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //MultiPanel1.Visible:=false;
  MultiPanel1.AnimationSpeed:=0.005;
  MultiPanel1.Disappear:=true;
   //MultiPanel2.Disappear:=true;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if MultiPanel1.Visible then MultiPanel1.Visible:=false else MultiPanel1.Visible:=true;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
 //MultiPanel3.Disappear:=true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 //MultiPanel1.ParentAsBkgrd:=false;
end;

procedure TForm1.FormShow(Sender: TObject);
begin

end;

procedure TForm1.Panel1Paint(Sender: TObject);
begin
 //Panel1.canvas.Pen.Color:=clred;
 //Panel1.canvas.Pen.Width:=5;
 //Panel1.canvas.Line(0,0,width,height);
end;

end.

