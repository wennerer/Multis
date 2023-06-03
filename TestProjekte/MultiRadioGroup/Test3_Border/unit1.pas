unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiRadioGroup,
  MultiCheckGroup;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiCheckGroup1: TMultiCheckGroup;
    MultiRadioGroup1: TMultiRadioGroup;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

