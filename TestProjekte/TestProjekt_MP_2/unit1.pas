unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLIntf, MultiPanel, MultiButton, MultiplexSlider, MultiButtonStyleManager,
  MultiSeperator;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButton7: TMultiButton;
    MultiButton8: TMultiButton;
    MultiButton9: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiPanel4: TMultiPanel;
    MultiPanel5: TMultiPanel;
    MultiSeperator1: TMultiSeperator;
    procedure FormCreate(Sender: TObject);
    procedure MultiPanel2Compressed(Sender: TObject);
    procedure MultiPanel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowCustomDrawnPanel(Sender: TObject);
    procedure ButtonsClick(Sender: TObject);
    procedure Sunshine;
    procedure Rain;
  private
    aTimer : TTimer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }



procedure TForm1.FormCreate(Sender: TObject);
begin
 aTimer          := TTimer.Create(self);
 aTimer.Interval := 500;
 aTimer.OnTimer  := @ShowCustomDrawnPanel;
 aTimer.Enabled  := true;
 MultiPanel1.AnimationSpeed := 0.005;
 MultiPanel5.ParentAsBkgrd:= false;
end;

procedure TForm1.MultiPanel2Compressed(Sender: TObject);
begin
 MultiPanel2.BorderSpacing.Top   := 10;
 MultiPanel2.BorderSpacing.Right := 10;
 MultiButton2.Visible:=false;
 MultiButton3.Visible:=false;
 MultiButton4.Visible:=false;
 MultiPanel2.Style:= mpsEllipse;
 MultiPanel2.ImageIndex:= 0
end;

procedure TForm1.MultiPanel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if MultiPanel2.ImageIndex = 0 then
 begin
  MultiPanel2.BorderSpacing.Top   := 0;
  MultiPanel2.BorderSpacing.Right := 0;
  MultiPanel2.ImageIndex:= -1;
  MultiPanel2.Style:= mpsRect;
  MultiButton2.Visible:=true;
  MultiButton3.Visible:=true;
  MultiButton4.Visible:=true;
 end;

end;


procedure TForm1.ShowCustomDrawnPanel(Sender: TObject);
begin
 aTimer.Enabled:= false;
 MultiPanel1.Appear:= true;
end;

procedure TForm1.ButtonsClick(Sender: TObject);
begin
 case (Sender as TMultiButton).Tag of
 1  : MultiPanel1.Disappear := true;
 5  : Sunshine;
 6  : Rain;
 end;
end;

procedure TForm1.Sunshine;
begin
 //MultiPanel1.Visible:= false;
 MultiPanel5.ImageIndex:= 1;
 MultiPanel5.Appear:= true;
end;

procedure TForm1.Rain;
begin
 MultiPanel5.ImageIndex:= 2;
 MultiPanel5.Appear:= true;
end;




end.

