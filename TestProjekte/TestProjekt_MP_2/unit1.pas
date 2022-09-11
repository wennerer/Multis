unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MultiPanel, MultiButton, MultiSeperator, MultiButtonStyleManager,
  MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    aTimer     : TTimer;
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel1: TMultiPanel;
    aMultiPanel: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    MultiSeperator1: TMultiSeperator;
    MultiSeperator2: TMultiSeperator;
    procedure FormCreate(Sender: TObject);
    procedure AppearMultiPanel1(Sender: TObject);
    procedure ButtonsClick(Sender: TObject);
    procedure MultiButton1MouseEnter(Sender: TObject);
    procedure MultiButton1MouseLeave(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
 aTimer            := TTimer.Create(self);
 aTimer.Interval   := 500;
 aTimer.OnTimer    := @AppearMultiPanel1;
 aTimer.Enabled    := true;
 MultiPanel1.AnimationSpeed:= 0.005;
end;

procedure TForm1.AppearMultiPanel1(Sender: TObject);
begin
  aTimer.Enabled:=false;
  MultiPanel1.Appear:= true;
end;

procedure TForm1.ButtonsClick(Sender: TObject);
begin
 case (Sender as TMultiButton).Tag of
 1  : begin
       aMultiPanel.Visible:= false;
       MultiPanel1.AnimationSpeed:= 0.05;
       MultiPanel1.Disappear:= true;

      end;
 end;
end;

procedure TForm1.MultiButton1MouseEnter(Sender: TObject);
begin
 if assigned(aMultiPanel) then
  begin
   aMultiPanel.Visible:= true;
   exit;
  end;
 aMultiPanel    := TMultiPanel.Create(self);
 aMultiPanel.Parent:= self;
 aMultiPanel.Caption:= 'Disappear';
 aMultiPanel.CaptionLayout:=tlTop;
 aMultiPanel.CaptionVerMargin:= 10;
 aMultiPanel.ColorStart:=clWhite;
 aMultiPanel.ColorEnd:= clCream;
 aMultiPanel.Style:=mpsCustom;
 aMultiPanel.LoadFromFile('Sprechblase');
 aMultiPanel.SetBounds(425,245,105,50);
end;

procedure TForm1.MultiButton1MouseLeave(Sender: TObject);
begin
 aMultiPanel.Visible:= false;
end;

end.

