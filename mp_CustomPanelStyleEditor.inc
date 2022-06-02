(*This is a part of the TMultiPanel*)

{%MultiPanel ,this is a part of MultiPanel last change 24.01.2022}


{ TCustomPanelStyle }

procedure TCustomPanelStyle.Edit;
begin
  inherited Edit; showmessage('');
end;

function TCustomPanelStyle.GetValue: string;
begin
  //Result:=inherited GetValue;
  result := 'TCustomPanelStyle';
end;

function TCustomPanelStyle.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := [paMultiSelect, paDialog];
end;
