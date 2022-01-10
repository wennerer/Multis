unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, MultiButton,
  MultiSeperator;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton1: TMultiButton;
    MultiSeperator1: TMultiSeperator;
    Panel1: TPanel;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

