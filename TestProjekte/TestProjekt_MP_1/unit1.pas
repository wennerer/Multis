unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiPanel,
  MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiPanel1: TMultiPanel;
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
  //showmessage('');
end;

end.

