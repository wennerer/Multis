unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, MultiPanel,
  MultiButton, MultiButtonStyleManager, MultiSeperator, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    MultiPanel3: TMultiPanel;
    procedure FormCreate(Sender: TObject);
    procedure MultiPanel2Compressed(Sender: TObject);
    procedure MultiPanel2Streched(Sender: TObject);

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

procedure TForm1.MultiPanel2Compressed(Sender: TObject);
begin
 MultiPanel2.ImageIndex:=7;
end;

procedure TForm1.MultiPanel2Streched(Sender: TObject);
begin
 MultiPanel2.ImageIndex:=8;
end;


end.

