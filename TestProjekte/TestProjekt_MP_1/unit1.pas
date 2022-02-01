unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel,
  MultiButton, MultiButtonStyleManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MultiButton1: TMultiButton;
    MultiButton2: TMultiButton;
    MultiButton3: TMultiButton;
    MultiButton4: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel1: TMultiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MultiButton1Click(Sender: TObject);
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

procedure TForm1.FormPaint(Sender: TObject);
begin
  caption:= inttostr(MultiPanel1.Height);
end;

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
  //showmessage('');
  invalidate;
end;

end.

