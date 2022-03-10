unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, MultiPanel,
  MultiButton, MultiButtonStyleManager, MultiSeperator;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButton6: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel1: TMultiPanel;
    MultiPanel2: TMultiPanel;
    procedure FormCreate(Sender: TObject);

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


end.

