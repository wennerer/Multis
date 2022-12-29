{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Multis;

{$warn 5023 off : no warning about unused units}
interface

uses
  MultiButtonStyleManager, MultiButton, ptin, rs_mbstylemanager, 
  MultiplexSlider, infmultis, MultiSeperator, CustomPen, MultiPanel, helpmenu, 
  PathTo, MultiLayer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MultiButtonStyleManager', @MultiButtonStyleManager.Register);
  RegisterUnit('MultiButton', @MultiButton.Register);
  RegisterUnit('MultiplexSlider', @MultiplexSlider.Register);
  RegisterUnit('MultiSeperator', @MultiSeperator.Register);
  RegisterUnit('MultiPanel', @MultiPanel.Register);
  RegisterUnit('helpmenu', @helpmenu.Register);
  RegisterUnit('MultiLayer', @MultiLayer.Register);
end;

initialization
  RegisterPackage('Multis', @Register);
end.
