unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MultiPanel,
  MultiLayer, MultiButton, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButton7: TMultiButton;
    MultiButton8: TMultiButton;
    MultiButton9: TMultiButton;
    MultiLayer1: TMultiLayer;
    MultiLayer2: TMultiLayer;
    MultiLayer3: TMultiLayer;
    MultiLayer4: TMultiLayer;
    MultiLayer5: TMultiLayer;
    MultiLayer6: TMultiLayer;
    MultiLayer7: TMultiLayer;
    MultiLayer8: TMultiLayer;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiPanel4: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    MultiplexSlider2: TMultiplexSlider;
    MultiplexSlider3: TMultiplexSlider;
    procedure FormCreate(Sender: TObject);
    procedure MultiButtonsClick(Sender: TObject);
    procedure MultiButton7Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiButtonsClick(Sender: TObject);
begin
 case (Sender as TMultiButton).Name of
  'MultiButton1' : MultiLayer2.Visible:= true;
  'MultiButton2' : MultiLayer1.Visible:= true;
  'MultiButton3' : MultiLayer4.Visible:= true;
  'MultiButton4' : MultiLayer3.Visible:= true;
  'MultiButton5' : MultiLayer6.Visible:= true;
  'MultiButton6' : MultiLayer5.Visible:= true;
 end;
end;

procedure TForm1.MultiButton7Click(Sender: TObject);
var lv : integer;
begin
 for lv := 0 to pred(ComponentCount) do
  begin
   if (Components[lv] is TMultiPanel) then
    if TMultiPanel(Components[lv]).Visible then TMultiPanel(Components[lv]).Disappear := true
    else TMultiPanel(Components[lv]).Appear := true;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var lv : integer;
begin
 for lv := 0 to pred(ComponentCount) do
  if (Components[lv] is TMultiButton) then
   TMultiButton(Components[lv]).Caption := 'Switch MultiLayer at runtime';
end;

end.

