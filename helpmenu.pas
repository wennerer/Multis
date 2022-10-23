{ <Adds a new item "Mutis-Help" to the Help menu>
  <Version 1.0.0.1>
  Copyright (C) <23.10.2022> <Bernd Hübner>
  for some improvements see https://www.lazarusforum.de/viewtopic.php?f=29&t=13252

  This library is free software; you can redistribute it and/or modify it under the
  terms of the GNU Library General Public License as published by the Free Software
  Foundation; either version 2 of the License, or (at your option) any later
  version with the following modification:

  As a special exception, the copyright holders of this library give you permission
  to link this library with independent modules to produce an executable,
  regardless of the license terms of these independent modules,and to copy and
  distribute the resulting executable under terms of your choice, provided that you
  also meet, for each linked independent module, the terms and conditions of the
  license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this
  exception to your version of the library, but you are not obligated to do so. If
  you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along
  with this library; if not, write to the Free Software Foundation, Inc., 51
  Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit helpmenu;

{$mode objfpc}{$H+}

interface 

uses 
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, LCLIntf, StdCtrls, ExtCtrls,
 LMessages, MenuIntf, Laz2_DOM, Laz_XMLRead, fileutil, rs_mbstylemanager;

type
  TInstall_Event = class(TObject)

  private
   procedure aButtonClick(Sender: TObject);

end;

procedure Register;



implementation
var  Install_Event : TInstall_Event;


procedure StartHelp({%H-}Sender : TObject);
begin
 if not OpenDocument(Application.Location+PathDelim+'docs'+PathDelim+'multis-help'+PathDelim+'DescriptionMultis_'+rs_lang +'.pdf')
  then showmessage(rs_muhelperror);
end;


function PathToXML(var success : boolean) : string;
var s  : string;
    lv : integer;
begin
  s := '';
  s := Application.Location;
  Delete(s,length(s),1);
  for lv := length(s) downto 0 do
   if copy(s,lv,1) = PathDelim then break;
  s := copy(s,1,lv)+'config_lazarus'+PathDelim+'packagefiles.xml';
  if fileexists(s) then result := s
  else
   begin
    showmessage(rs_packfileerror);
    success := false;
   end;
end;

function PathToMultis(aXML : string;var success : boolean) : string;
var Document   : TXMLDocument;
    i,j,k,l,lv : Integer;
    s          : string;
begin
  ReadXMLFile(Document, aXML);

  for i := 0 to (Document.DocumentElement.ChildNodes.Count - 1) do
   begin
    for j := 0 to (Document.DocumentElement.ChildNodes.Item[i].ChildNodes.Count - 1) do
      for k := 0 to (Document.DocumentElement.ChildNodes.Item[i].ChildNodes.Item[j].ChildNodes.Count - 1) do
        if Document.DocumentElement.ChildNodes.Item[i].ChildNodes.Item[j].ChildNodes.Item[k].Attributes[0].NodeValue = 'Multis'
         then begin
          for l := 0 to (Document.DocumentElement.ChildNodes.Item[i].ChildNodes.Item[j].ChildNodes.Count - 1) do
           if Document.DocumentElement.ChildNodes.Item[i].ChildNodes.Item[j].ChildNodes.Item[l].NodeName = 'Filename' then
            s:=Document.DocumentElement.ChildNodes.Item[i].ChildNodes.Item[j].ChildNodes.Item[l].Attributes[0].NodeValue;
         end;//if Multis
   end;//i

  if fileexists(s) then
   begin
    for lv := length(s) downto 0 do
    if copy(s,lv,1) = PathDelim then break;
    s := copy(s,1,lv)+PathDelim+'help'+PathDelim;
    result := s
   end
   else
    begin
     showmessage(rs_muhelperror);
     success := false;
    end;

  Document.Free;
end;

procedure CopyMultisHelp( aPath : string;success:boolean);
begin
  if not success then exit;
  if not DirectoryExists(Application.Location+PathDelim+'docs'+PathDelim+'multis-help') then
    //CreateDir(Application.Location+PathDelim+'docs'+PathDelim+'multis-help');
    ForceDirectories(Application.Location+PathDelim+'docs'+PathDelim+'multis-help');

  copyfile(aPath+'DescriptionMultis_de.pdf',Application.Location+PathDelim+'docs'+PathDelim+'multis-help'+PathDelim+'DescriptionMultis_de.pdf');
  copyfile(aPath+'DescriptionMultis_en.pdf',Application.Location+PathDelim+'docs'+PathDelim+'multis-help'+PathDelim+'DescriptionMultis_en.pdf');


end;

procedure TInstall_Event.aButtonClick(Sender: TObject);
begin
(((Sender as TButton).Parent) as TCustomForm).Close;
end;

procedure Query(var sucess:boolean);
var QueryForm  : TCustomForm;
    R          : TRect;
    aTextStyle : TTextStyle;
    aImage     : TImage;
    aRadio     : TRadioGroup;
    aButton    : TButton;
begin
 try
   QueryForm                := TForm.Create(Application);
   QueryForm.Width          := 715;
   QueryForm.Height         := 250;
   QueryForm.Left           := (QueryForm.Monitor.Width div 2) - (QueryForm.Width div 2);
   QueryForm.Top            := (QueryForm.Monitor.Height div 2) -(QueryForm.Height div 2);
   QueryForm.BorderStyle    := bsSingle;
   QueryForm.Caption        := 'Multis Help';

   R                        := rect(10,20,QueryForm.Width-20,80);
   aTextStyle.Layout        := tlCenter;
   aTextStyle.Alignment     := taCenter;
   aTextStyle.SingleLine    := false;
   aTextStyle.Wordbreak     := true;

   aImage                   := TImage.Create(QueryForm);
   aImage.Parent            := QueryForm;
   aImage.SetBounds(0,0,QueryForm.Width,80);
   aImage.Canvas.Brush.Color:= clForm;
   aImage.Canvas.FillRect(Rect(0,0,aImage.Width,80));
   aImage.canvas.Font.Color:=clBlue;
   //aImage.Canvas.Font.Height:= 18; //this not works?
   aImage.Canvas.TextRect(R,0,0,rs_install1+#13+rs_install2,aTextStyle);

   aRadio                   := TRadioGroup.Create(QueryForm);
   aRadio.Parent            := QueryForm;
   aRadio.SetBounds((QueryForm.Width div 2)-100,100,200,60);
   aRadio.Items.Add(rs_yes1);
   aRadio.Items.Add(rs_no1);
   aRadio.ItemIndex := 0;

   aButton                  := TButton.Create(QueryForm);
   aButton.Parent           := QueryForm;
   aButton.SetBounds(20,200,QueryForm.Width -40,25);
   aButton.Caption          := rs_carryon;
   aButton.OnClick          := @Install_Event.aButtonClick;

   QueryForm.ShowModal;

   if aRadio.ItemIndex = 0 then sucess := true else sucess := false;

 finally
   QueryForm.Free;
 end;
end;

procedure Register;
var okay : boolean;
begin
  {$I helpmenu.lrs}
  okay := true;
  Query(okay);
  if not okay then exit;
  CopyMultisHelp(PathToMultis(PathToXML(okay),okay),okay);
  if not okay then exit;
  RegisterIDEMenuCommand(itmHelpTools, 'MultisHelp',rs_muhelp,nil,@StartHelp,nil,'help');

end;


end.
