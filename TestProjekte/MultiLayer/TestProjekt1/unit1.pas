unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel, MultiLayer,
  MultiButton, MultiplexSlider, MultiSeperator, MultiButtonStyleManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
    MultiButton10: TMultiButton;
    MultiButton11: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButton7: TMultiButton;
    MultiButton8: TMultiButton;
    MultiButton9: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiLayer1: TMultiLayer;
    MultiLayer2: TMultiLayer;
    MultiLayer3: TMultiLayer;
    MultiLayer4: TMultiLayer;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    MultiplexSlider2: TMultiplexSlider;
    procedure MultiButton7Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiButton7Click(Sender: TObject);
begin
 if MultiLayer1.Visible then
 begin
  MultiLayer1.Visible := false;
  MultiLayer2.Visible := true;
  MultiLayer3.Visible := false;
  MultiLayer4.Visible := true;
 end else
 begin
  MultiLayer1.Visible := true;
  MultiLayer2.Visible := false;
  MultiLayer3.Visible := true;
  MultiLayer4.Visible := false;
 end;
end;

end.

