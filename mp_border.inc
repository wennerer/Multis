(*This is a part of the TMultiPanel*)

{%MultiPanel ,this is a part of MultiPanel last change 24.01.2022}


{ TBorder }

constructor TBorder.create(aOwner: TCustomPanel);
begin
 FOwner:=aOwner;
 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TBorder.SetOuterColor(AValue: TColor);
begin
  if FOuterColor=AValue then Exit;
  FOuterColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TBorder.SetInnerColor(AValue: TColor);
begin
  if FInnerColor=AValue then Exit;
  FInnerColor:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TBorder.SetBetween(AValue: integer);
begin
  if FBetween=AValue then Exit;
  FBetween:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TBorder.SetInnerWidth(AValue: integer);
begin
  if FInnerWidth=AValue then Exit;
  FInnerWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TBorder.SetOuterWidth(AValue: integer);
begin
  if FOuterWidth=AValue then Exit;
  FOuterWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

