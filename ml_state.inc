{%MultiEventLine}


{ TStates }
constructor TStates.Create(aOwner : TCustomControl);
begin
 FOwner := aOwner;
 FInProcess                   := TStateProperties.Create(FOwner);
 FInProcess.FColorStart       := clLime;
 FInProcess.FColorEnd         := clRed ;


 FDone := TStateProperties.Create(FOwner);
 FDone.FColorStart := clYellow;
end;

destructor TStates.Destroy;
begin
 FInProcess.Free;
 FDone.Free;
 inherited Destroy;
end;



{ TStateProperties }

procedure TStateProperties.SetColorEnd(AValue : TColor);
begin
  if FColorEnd = AValue then Exit;
  FColorEnd := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetColorStart(AValue : TColor);
begin
  if FColorStart = AValue then Exit;
  FColorStart := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetEnabled(AValue : boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetFont(AValue : TFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetGradient(AValue : TGradientCourse);
begin
  if FGradient = AValue then Exit;
  FGradient := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetImageIndex(AValue : TImageIndex);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetImageList(AValue : TCustomImageList);
begin
  if FImageList = AValue then Exit;
  FImageList := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetRRRadius(AValue : integer);
begin
  if FRRRadius = AValue then Exit;
  FRRRadius := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetSize(AValue : integer);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStateProperties.SetVisible(AValue : Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

constructor TStateProperties.Create(aOwner : TCustomControl);
begin
  FOwner := aOwner;
end;


