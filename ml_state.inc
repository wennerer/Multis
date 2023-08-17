{%MultiEventLine}


{ TStates }
constructor TStates.Create(aOwner : TCustomControl);
begin
 FOwner := aOwner;
 FInProcess := TStateProperties.Create(FOwner);
 FDone := TStateProperties.Create(FOwner);
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
end;

procedure TStateProperties.SetColorStart(AValue : TColor);
begin
  if FColorStart = AValue then Exit;
  FColorStart := AValue;
end;

procedure TStateProperties.SetEnabled(AValue : boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
end;

procedure TStateProperties.SetFont(AValue : TFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
end;

procedure TStateProperties.SetGradient(AValue : TGradientCourse);
begin
  if FGradient = AValue then Exit;
  FGradient := AValue;
end;

procedure TStateProperties.SetImageIndex(AValue : TImageIndex);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
end;

procedure TStateProperties.SetImageList(AValue : TCustomImageList);
begin
  if FImageList = AValue then Exit;
  FImageList := AValue;
end;

procedure TStateProperties.SetRRRadius(AValue : integer);
begin
  if FRRRadius = AValue then Exit;
  FRRRadius := AValue;
end;

procedure TStateProperties.SetSize(AValue : integer);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
end;

procedure TStateProperties.SetVisible(AValue : Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
end;

constructor TStateProperties.Create(aOwner : TCustomControl);
begin

end;


