unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    SpeedButton1: TSpeedButton;
    procedure FormPaint(Sender: TObject);

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
 canvas.TextOut(10,250,'In order for the images to change at runtime, I had to change the DPI Awareness');
 canvas.TextOut(10,270,'setting under Project, Project Options!');
end;

end.

