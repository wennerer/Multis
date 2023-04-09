{%MultiEventLine}

{ TMultiEvent }

constructor TMultiEvent.Create(ACollection: TCollection);
begin
 inherited Create(ACollection);
 FEvents             := ACollection;
 FDisplayName         := 'Event';
 FEnabled             := true;
 FVisible             := true;
 FColor               := RGB(random(255),random(255),random(255));
 FDiameter            := random(15)+10;
 FLeft                := random(75);
 FTop                 := random(75);
end;




procedure TMultiEvent.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TMultiEvent.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

function TMultiEvent.GetDisplayName: string;
begin
  Result:=inherited GetDisplayName;
end;

procedure TMultiEvent.SetDisplayName(const Value: string);
begin
  //inherited SetDisplayName(Value);
 FDisplayName := Value;
end;

function TMultiEvent.GetOwner: TPersistent;
begin
  //Result:=inherited GetOwner;
 Result := FEvents;
end;

