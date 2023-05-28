unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MultiPanel,
  MultiButton, MultiButtonStyleManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiButton_Linear: TMultiButton;
    MultiButton_20: TMultiButton;
    MultiButton_50: TMultiButton;
    MultiButton_70: TMultiButton;
    MultiButton_Mono: TMultiButton;
    MultiButton_Stereo: TMultiButton;
    MultiButton_MPX: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel_Settings: TMultiPanel;
    MultiPanel_Pre: TMultiPanel;
    MultiPanel_Audio: TMultiPanel;

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


end.

