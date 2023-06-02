{%MultiEventLine}

{ TLine }

constructor TLine.create(AOwner: TCustomControl);
begin
 FOwner:=aOwner;
 inherited create;
end;

procedure TLine.SetColorEnd(AValue: TColor);
begin
 if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TLine.SetColorStart(AValue: TColor);
begin
 if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TLine.SetGradient(AValue: TGradientCourse);
begin
 if FGradient=AValue then Exit;
 FGradient:=AValue;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TLine.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TLine.SetHorizontalMargin(AValue: integer);
begin
  if FHorizontalMargin=AValue then Exit;
  FHorizontalMargin:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TLine.SetVertMargin(AValue: integer);
begin
  if FVertMargin=AValue then Exit;
  FVertMargin:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TLine.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

