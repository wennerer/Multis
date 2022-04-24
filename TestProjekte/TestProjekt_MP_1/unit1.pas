unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MultiPanel, MultiButton, MultiButtonStyleManager, MultiSeperator,
  MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton10: TMultiButton;
    MultiButton11: TMultiButton;
    MultiButton12: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButton7: TMultiButton;
    MultiButton8: TMultiButton;
    MultiButton9: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiButtonStyleManager2: TMultiButtonStyleManager;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    MultiPanel4: TMultiPanel;
    MultiPanel5: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    MultiSeperator1: TMultiSeperator;
    procedure FormCreate(Sender: TObject);
    procedure MultiButtonClick(Sender: TObject);
    procedure MultiPanel2Compressed(Sender: TObject);
    procedure MultiPanel2Streched(Sender: TObject);
    procedure MultiPanel4Compressed(Sender: TObject);
    procedure MultiPanel4Streched(Sender: TObject);

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

end;

procedure TForm1.MultiButtonClick(Sender: TObject);
begin
  if Sender = Multibutton2 then MultiPanel3.Caption:='Tomate';
  if Sender = Multibutton3 then MultiPanel3.Caption:='Kirsche';
  if Sender = Multibutton4 then MultiPanel3.Caption:='Pilz';
  if Sender = Multibutton5 then MultiPanel3.Caption:='Baum';
  if Sender = Multibutton6 then MultiPanel3.Caption:='Auswahl 1';
  if Sender = Multibutton7 then MultiPanel3.Caption:='Auswahl 2';
  if Sender = Multibutton8 then MultiPanel3.Caption:='Auswahl 3';
  if Sender = Multibutton9 then MultiPanel3.Caption:='Auswahl 4';
end;

procedure TForm1.MultiPanel2Compressed(Sender: TObject);
begin
 MultiPanel2.ImageIndex:=7;
end;

procedure TForm1.MultiPanel2Streched(Sender: TObject);
begin
 MultiPanel2.ImageIndex:=10;
end;

procedure TForm1.MultiPanel4Compressed(Sender: TObject);
begin
 MultiPanel4.ImageIndex:=12;
end;

procedure TForm1.MultiPanel4Streched(Sender: TObject);
begin
 MultiPanel4.ImageIndex:=11;
end;


end.

