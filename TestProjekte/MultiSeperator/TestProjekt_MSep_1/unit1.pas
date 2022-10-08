unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Menus, MultiButton, MultiSeperator, MultiButtonStyleManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButton5: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiSeperator1: TMultiSeperator;
    MultiSeperator2: TMultiSeperator;
    MultiSeperator3: TMultiSeperator;
    MultiSeperator4: TMultiSeperator;
    MultiSeperator5: TMultiSeperator;
    MultiSeperator6: TMultiSeperator;
    Panel1: TPanel;
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Image1Click(Sender: TObject);
begin

end;

end.

