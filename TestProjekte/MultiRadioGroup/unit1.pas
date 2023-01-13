unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, MultiRadioGroup, MultiPanel, MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    MultiRadioGroup1: TMultiRadioGroup;
    RadioGroup1: TRadioGroup;
    StringGrid1: TStringGrid;
    procedure RadioGroup1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  showmessage('');
end;

end.

