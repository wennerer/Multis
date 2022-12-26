{ <determines the path to ...>

  Copyright (C) <Version 1.0.0.1 31.10.2022> <Bernd Hübner>

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option)
  any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with
  independent modules to produce an executable, regardless of the license terms of these independent modules,and to
  copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each
  linked independent module, the terms and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this library, you may extend this exception to
  your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
  details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to
  the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit PathTo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Dialogs, DOM, XMLWrite, XMLRead, XPath, rs_mbstylemanager;


//function PathToPrimaryConfig :string;
//function PathToSecondaryConfig : string;
//function PathToConfig :string;
//function PathToConfigFile(aFilename : string) : string;
//function PathViaDialog : string;
function MenuDialog : boolean;

function ReadPathToMultis(aFilename :string) : string;

procedure WriteMultisXMl(New,Help,Filename:string);
function ReadNew(Filename:string):string;
function ReadHelp(Filename:string) : string;

implementation
(*
function PathToPrimaryConfig: string;
var  f : TextFile;
     s : string;
     i : integer;
begin
 //means Error
 result := ('-1');

 //custom config
 if FileExists(Application.Location + 'lazarus.cfg') then
  begin
   AssignFile(f,Application.Location + 'lazarus.cfg');
   Reset(f);
   Readln(f,s);
   CloseFile(f);
   i := Pos('=',s);
   delete(s,1,i);
   s:=trim(s);
   result := s+PathDelim;
   exit;
  end;

 {$IFDEF Unix}
  s:=getenvironmentvariable('HOME')+PathDelim+'.lazarus';
  if DirectoryExists(s) then
   begin
    result:=s+PathDelim;
    exit;
   end;
 {$ENDIF}

 {$IFDEF WINDOWS}
 //Primary config
  if getenvironmentvariable('LocalAppdata') <> '' then
   s:=getenvironmentvariable('LocalAppdata')+PathDelim+ 'lazarus';
   if DirectoryExists(s) then
   begin
    result:=s+PathDelim;
    exit;
   end;

 //XP German Primary config
  s:=getenvironmentvariable('SystemDrive')+getenvironmentvariable('HomePath')
     +PathDelim+'Lokale Einstellungen'+PathDelim+ 'Anwendungsdaten'
     +PathDelim+ 'lazarus';
  if DirectoryExists(s) then
   begin
    result:=s+PathDelim;
    exit;
   end;
 //XP English Primary config
  s:=getenvironmentvariable('SystemDrive')+getenvironmentvariable('HomePath')
     +PathDelim+'Local Settings'+PathDelim+ 'Application'
     +PathDelim+ 'lazarus';
  if DirectoryExists(s) then
   begin
    result:=s+PathDelim;
    exit;
   end;

 {$ENDIF}


end;   *)

(*
function PathToSecondaryConfig: string;
var s : string;
begin

 {$IFDEF Unix}
  s:= PathDelim+'etc'+PathDelim+'lazarus';
  if DirectoryExists(s) then
   begin
    result := s+PathDelim;
    exit;
   end;
 {$ENDIF}


 {$IFDEF Windows}
  s:=getenvironmentvariable('SystemDrive')+PathDelim+'lazarus';
  if DirectoryExists(s) then
   begin
    result:=s+PathDelim;
    exit;
   end;
 {$ENDIF}

 result:= Application.Location;

end;  *)

(*
function PathToConfig: string;
var PathToConfigDir : string;
begin
  PathToConfigDir := PathToPrimaryConfig;
  if PathToConfigDir = '-1' then PathToConfigDir := PathToSecondaryConfig;
  Result := PathToConfigDir;
end; *)

(*
function PathToConfigFile(aFilename: string): string;
var s : string;
begin
 //means Error
 result := ('-1');

 s:= PathToPrimaryConfig+aFilename;
 if FileExists(s) then
  begin
   Result:= s;
   Exit;
  end;

 s:= PathToSEcondaryConfig+aFilename;
 if FileExists(s) then
  begin
   Result:= s;
   Exit;
  end;

end;  *)


function PathViaDialog: string;
var TaskDialog1 : TTaskDialog;
    DirDialog   : TSelectDirectoryDialog;
begin
 TaskDialog1   := TTaskDialog.Create(nil);

 try
  TaskDialog1.Caption      := 'The Multis Package';
  TaskDialog1.Title        := 'Multis Help';
  TaskDialog1.Text         := 'The path to the multis package could not be found. Please enter the path to multis/help!';
  TaskDialog1.MainIcon     := tdiWarning;
  TaskDialog1.CommonButtons:= [tcbCancel];
  TaskDialog1.Flags        := [tfUseCommandLinks];

  with TTaskDialogButtonItem(TaskDialog1.Buttons.Add) do
    begin
      Caption     := 'Select a Directory';
      ModalResult := mrOk;
    end;


  //From here it is executed when a button is pressed
   if TaskDialog1.Execute then
    begin
     Result := '-1';
     if TaskDialog1.ModalResult = mrOk     then
      begin
       DirDialog   := TSelectDirectoryDialog.Create(nil);
       try
        if DirDialog.Execute then
         Result := DirDialog.FileName;

       finally
        DirDialog.Free;
       end;
      end;//mrOk

    end;//TaskDialog1.Execute

 finally
  TaskDialog1.Free;
 end;
end;


function MenuDialog : boolean;
var TaskDialog1: TTaskDialog;
begin
 TaskDialog1   := TTaskDialog.Create(nil);
 try
  TaskDialog1.Caption      := 'The Multis Package';
  TaskDialog1.Title        := rs_muhelp;
  TaskDialog1.Text         := rs_installmenu;
  TaskDialog1.CommonButtons:= [tcbOk];
  TaskDialog1.MainIcon     := tdiQuestion;
  TaskDialog1.FooterText   := 'https://github.com/wennerer/Multis.git';
  TaskDialog1.FooterIcon   := tdiInformation;

  TaskDialog1.RadioButtons.Add;
  TaskDialog1.RadioButtons.Items[0].Caption:=rs_yes1;

  TaskDialog1.RadioButtons.Add;
  TaskDialog1.RadioButtons.Items[1].Caption:=rs_no1;


  //From here it is executed when a button is pressed
   if TaskDialog1.Execute then
    begin
     Result := false;
     if TaskDialog1.ModalResult = mrOk     then
      if TaskDialog1.RadioButton.Index = 0 then
       Result := true;
    end;

 finally
  TaskDialog1.Free;
 end;
end;



function ReadPathToMultis(aFilename :string): string;
var
  Xml: TXMLDocument;
  XPathResult: TXPathVariable;
  APtr:Pointer;
  Path : string;
  i : integer;
begin
  Path:= '';
  ReadXMLFile(Xml, aFilename);
  XPathResult := EvaluateXPathExpression('/CONFIG/UserPkgLinks//*[Name[@Value="Multis"]]/Filename/@*', Xml.DocumentElement);
  For APtr in XPathResult.AsNodeSet do
    Path := Path + string(TDOMNode(APtr).NodeValue);
  XPathResult.Free;
  Xml.Free;

  i:=Pos('multis.lpk',Path);
  Path := Copy(Path, 1, pred(i));
  Result := Path;
end;

procedure WriteMultisXMl(New, Help, Filename: string);
var
  Doc: TXMLDocument;                                         // Variable für das Dokument
  RootNode, parentNode, nofilho: TDOMNode;                   // Variable für die Elemente (Knoten)
begin
  try
    // Erzeuge ein Dokument
    Doc := TXMLDocument.Create;

    // Erzeuge einen Wurzelknoten
    RootNode := Doc.CreateElement('package');
    Doc.Appendchild(RootNode);
    // Create a parent node
    RootNode:= Doc.DocumentElement;
    parentNode := Doc.CreateElement('multis');
    TDOMElement(parentNode).SetAttribute('id', '001');
    RootNode.Appendchild(parentNode);

    // Create a child node
    parentNode := Doc.CreateElement('new');
    nofilho := Doc.CreateTextNode(Unicodestring(New));
    parentNode.Appendchild(nofilho);
    RootNode.ChildNodes.Item[0].AppendChild(parentNode);

    // Create a child node
    parentNode := Doc.CreateElement('help');
    nofilho := Doc.CreateTextNode(Unicodestring(Help));
    parentNode.Appendchild(nofilho);
    RootNode.ChildNodes.Item[0].AppendChild(parentNode);

   (*  // Create a child node
    parentNode := Doc.CreateElement('filename');
    nofilho := Doc.CreateTextNode(Unicodestring(Filename));
    parentNode.Appendchild(nofilho);
    RootNode.ChildNodes.Item[0].AppendChild(parentNode); *)

    writeXMLFile(Doc,Filename+'multis.xml');
  finally
    Doc.Free;
  end;
end;

function ReadNew(Filename: string): string;
var
  Xml: TXMLDocument;
  XPathResult: TXPathVariable;
begin
  ReadXMLFile(Xml, Filename);
  XPathResult := EvaluateXPathExpression('/package/multis/new', Xml.DocumentElement);
  Result:= String(XPathResult.AsText);
  XPathResult.Free;
  Xml.Free;
end;


function ReadHelp(Filename:string) : string;
var
  Xml: TXMLDocument;
  XPathResult: TXPathVariable;
begin
  ReadXMLFile(Xml,Filename);
  XPathResult := EvaluateXPathExpression('/package/multis/help', Xml.DocumentElement);
  Result:=String(XPathResult.AsText);
  XPathResult.Free;
  Xml.Free;
end;

end.

