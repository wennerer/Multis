unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MultiSeperator,
  MultiButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
    MultiSeperator1: TMultiSeperator;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

