unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLIntf,
  MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    MultiplexSlider1: TMultiplexSlider;
    MultiplexSlider2: TMultiplexSlider;
    MultiplexSlider3: TMultiplexSlider;
    MultiplexSlider4: TMultiplexSlider;
    Panel1: TPanel;
    procedure MultiplexSlider1Change3x(const Val1, Val2, Val3: integer);
    procedure MultiplexSlider1ChangeStr3x(const StrVal1, StrVal2,
      StrVal3: string);
    procedure MultiplexSliderChange(const aValue: integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiplexSliderChange(const aValue: integer);
begin
 Panel1.Color:= RGB(MultiplexSlider2.Knob1Settings.KnobPosition,MultiplexSlider3.Knob1Settings.KnobPosition,
                    MultiplexSlider4.Knob1Settings.KnobPosition);
 Panel1.Caption:= inttostr(MultiplexSlider2.Knob1Settings.KnobPosition)+'/' +
                  inttostr(MultiplexSlider3.Knob1Settings.KnobPosition)+'/' +
                  inttostr(MultiplexSlider4.Knob1Settings.KnobPosition);
end;

procedure TForm1.MultiplexSlider1Change3x(const Val1, Val2, Val3: integer);
begin
 Panel1.Color:= RGB(val1,val2,val3);
end;

procedure TForm1.MultiplexSlider1ChangeStr3x(const StrVal1, StrVal2,
  StrVal3: string);
begin
 Panel1.Caption:= strval1+'/'+strval2+'/'+strval3;
end;

end.

