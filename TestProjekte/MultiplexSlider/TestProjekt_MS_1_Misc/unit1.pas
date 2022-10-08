unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiplexSlider1: TMultiplexSlider;
    MultiplexSlider10: TMultiplexSlider;
    MultiplexSlider11: TMultiplexSlider;
    MultiplexSlider12: TMultiplexSlider;
    MultiplexSlider13: TMultiplexSlider;
    MultiplexSlider14: TMultiplexSlider;
    MultiplexSlider15: TMultiplexSlider;
    MultiplexSlider16: TMultiplexSlider;
    MultiplexSlider17: TMultiplexSlider;
    MultiplexSlider18: TMultiplexSlider;
    MultiplexSlider19: TMultiplexSlider;
    MultiplexSlider2: TMultiplexSlider;
    MultiplexSlider3: TMultiplexSlider;
    MultiplexSlider4: TMultiplexSlider;
    MultiplexSlider5: TMultiplexSlider;
    MultiplexSlider6: TMultiplexSlider;
    MultiplexSlider7: TMultiplexSlider;
    MultiplexSlider8: TMultiplexSlider;
    MultiplexSlider9: TMultiplexSlider;
    Panel1: TPanel;
    procedure MultiplexSlider5ChangeStr3x(const StrVal1, StrVal2,
      StrVal3: string);
    procedure MultiplexSliderISChangeStr(const aStrValue: string);

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.MultiplexSliderIsChangeStr(const aStrValue: string);
begin
 Panel1.Caption:= aStrValue;
end;

procedure TForm1.MultiplexSlider5ChangeStr3x(const StrVal1, StrVal2,
  StrVal3: string);
begin
 Panel1.Caption:= StrVal1+#13+StrVal2+#13+StrVal3;
end;


end.

