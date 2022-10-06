unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LazLoggerBase, MultiPanel, MultiButton, MultiSeperator,
  MultiButtonStyleManager, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    aTimer     : TTimer;
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton10: TMultiButton;
    MultiButton11: TMultiButton;
    MultiButton12: TMultiButton;
    MultiButton13: TMultiButton;
    MultiButton14: TMultiButton;
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
    aMultiPanel: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiPanel4: TMultiPanel;
    MultiPanel5: TMultiPanel;
    MultiPanel6: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    MultiplexSlider2: TMultiplexSlider;
    MultiplexSlider3: TMultiplexSlider;
    MultiSeperator1: TMultiSeperator;
    MultiSeperator2: TMultiSeperator;
    MultiSeperator3: TMultiSeperator;
    procedure FormCreate(Sender: TObject);
    procedure AppearMultiPanel1(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure MultiButton1MouseEnter(Sender: TObject);
    procedure MultiButton1MouseLeave(Sender: TObject);
    procedure MultiButton8Click(Sender: TObject);
    procedure MultiButton8MouseEnter(Sender: TObject);
    procedure MultiButton8MouseLeave(Sender: TObject);
    procedure MultiButton9MouseEnter(Sender: TObject);
    procedure MultiPanel5Compressed(Sender: TObject);
    procedure MultiPanel5MouseEnter(Sender: TObject);
    procedure MultiplexSlider1Change(const aValue: integer);
    procedure MultiplexSlider2Change(const aValue: integer);
    procedure MultiplexSlider3Change(const aValue: integer);
    procedure Trigger(Sender: TObject);
    procedure Style(Sender: TObject);
    procedure Custom(Sender: TObject);
    procedure Switch(Sender: TObject);
  private
    InPanel5 : boolean;



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
 MultiplexSlider1.Knob1Settings.KnobPosition:= 3;
 MultiplexSlider2.Knob1Settings.KnobPosition:= 20;
 MultiplexSlider3.Knob1Settings.KnobPosition:=  5;
 MultiPanel6.DropDownMenu.Hotspot:= rect(0,0,10,160);
end;

procedure TForm1.AppearMultiPanel1(Sender: TObject);
begin
  aTimer.Enabled:=false;
  MultiPanel1.Appear:= true;
end;

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
 aMultiPanel.Visible:= false;
 MultiPanel1.Disappear:= true;
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



//Submenu
procedure TForm1.MultiButton8Click(Sender: TObject);
begin
 MultiPanel5.DropDownMenu.Stretched.Active:=true;
 MultiPanel4.DropDownMenu.Active := false;
end;

procedure TForm1.MultiButton8MouseEnter(Sender: TObject);
begin
 MultiPanel5.DropDownMenu.Stretched.Active:=true;
 MultiPanel4.DropDownMenu.Active := false;
end;


procedure TForm1.MultiButton8MouseLeave(Sender: TObject);
begin
 MultiPanel4.DropDownMenu.Active := true;
 MultiPanel5.DropDownMenu.Compressed.Active:=true;
 INPanel5 := false;
end;

procedure TForm1.MultiButton9MouseEnter(Sender: TObject);
begin
 MultiPanel4.DropDownMenu.Active := false;
 MultiPanel5.DropDownMenu.Stretched.Active:=true;
 InPanel5 := true;
end;

procedure TForm1.MultiPanel5MouseEnter(Sender: TObject);
begin
 MultiPanel4.DropDownMenu.Active := false;
 MultiPanel5.DropDownMenu.Stretched.Active:=true;
 InPanel5 := true;
end;

procedure TForm1.FormMouseEnter(Sender: TObject);
begin
 if InPanel5 then
  begin
   MultiPanel4.DropDownMenu.Active := true;
   MultiPanel5.DropDownMenu.Compressed.Active:=true;
   InPanel5 := false;
  end;
end;

procedure TForm1.MultiPanel5Compressed(Sender: TObject);
begin
 MultiPanel4.DropDownMenu.Active := true;
 if InPanel5 then MultiPanel4.DropDownMenu.Compressed.Active:=true;
 MultiPanel5.SendToBack;
end;

//Set the trigger
procedure TForm1.Trigger(Sender: TObject);
var lv : Integer;
begin
 for lv := 0 to pred(ComponentCount) do
  begin
   if (Components[lv] is TMultiPanel) then
    begin
     TMultiPanel(Components[lv]).DropDownMenu.Trigger:=TTrigger((Sender as TMultiButton).Tag);
    end;
  end;
end;

//Set the speed
procedure TForm1.MultiplexSlider1Change(const aValue: integer);
var lv : Integer;
begin
 for lv := 0 to pred(ComponentCount) do
  begin
   if (Components[lv] is TMultiPanel) then
    begin
     TMultiPanel(Components[lv]).DropDownMenu.Speed := aValue;
    end;
  end;
end;

procedure TForm1.MultiplexSlider2Change(const aValue: integer);
var lv : Integer;
begin
 for lv := 0 to pred(ComponentCount) do
  begin
   if (Components[lv] is TMultiPanel) then
    begin
     TMultiPanel(Components[lv]).DropDownMenu.Step := aValue;
    end;
  end;
end;

procedure TForm1.MultiplexSlider3Change(const aValue: integer);
var lv : Integer;
    d  : double;
begin
 d := aValue / 10000;
 for lv := 0 to pred(ComponentCount) do
  begin
   if (Components[lv] is TMultiPanel) then
    begin
     TMultiPanel(Components[lv]).AnimationSpeed := d;
    end;
  end;
end;

//set the style of MultiPanel1
procedure TForm1.Style(Sender: TObject);
begin
 case (Sender as TMultiButton).Tag of
  0 : MultiPanel1.Style:= TMPanelStyle((Sender as TMultiButton).Tag);
  1 : MultiPanel1.Style:= TMPanelStyle((Sender as TMultiButton).Tag);
  2 : MultiPanel1.Style:= TMPanelStyle((Sender as TMultiButton).Tag);
 end;
 MultiPanel1.Caption:= 'I am a MultiPanel';
end;

procedure TForm1.Custom(Sender: TObject);
begin
 case (Sender as TMultiButton).Tag of
  4 : MultiPanel1.LoadFromFile('Wolke');
  5 : MultiPanel1.LoadFromFile('Sprechblase');
 end;
 MultiPanel1.Style:= mpsCustom;
 MultiPanel1.Caption:= 'I am a MultiPanel';
end;

//un-visible,dis-appear
procedure TForm1.Switch(Sender: TObject);
begin
 case (Sender as TMultiButton).Tag of
  11 : MultiPanel1.Visible:= true;
  12 : MultiPanel1.Visible:= false;
  13 : MultiPanel1.Appear:= true;
  14 : MultiPanel1.Disappear:= true;
 end;
end;

end.

