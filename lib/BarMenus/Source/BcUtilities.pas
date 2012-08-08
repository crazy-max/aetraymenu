{ Unit: BcUtils
  ===========================================================================

  Copyright (C) 2001-2002 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2001-2002 Jouni Airaksinen. All Rights Reserved.
  http://Mintus.Codefield.com/ -- Mintus@Codefield.com


  Parts copied from,
    Borland Delphi Visual Component Library
    Copyright (c) Borland Corporation

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}

unit BcUtilities;

interface

uses Graphics;

type
  TColorQuad = record
    Red, Green, Blue, Alpha: Byte;
  end;

  TSmallColorQuad = record
    Red, Green, Blue: Byte;
  end;

  TLargeColorQuad = record
    Red, Green, Blue, Alpha: Longint;
  end;

  T24bitScanLineElement = record
    Blue, Green, Red: Byte;
  end;

  T32bitScanLineElement = record
    Blue, Green, Red, Alpha: Byte;
  end;

  P24bitQuadScanLine = ^T24bitQuadScanLine;
  T24bitQuadScanLine = array[0..High(Word) div 3] of T24bitScanLineElement;

  P32bitQuadScanLine = ^T32bitQuadScanLine;
  T32bitQuadScanLine = array[0..High(Word) div 3] of T32bitScanLineElement;


procedure Error(ResStr: PResStringRec); { source VCL Menus.pas }
function ColorTo24bitScanLineElement(Color: TColor): T24bitScanLineElement;
function ColorTo32bitScanLineElement(Color: TColor): T32bitScanLineElement;
function RGB(Red, Green, Blue: Byte; Alpha: Byte = $00): TColor;
function Min(Value1, Value2: Integer): Integer;
function Max(Value1, Value2: Integer): Integer;

implementation

uses
  Menus;

{ This procedure from VCL sources }
procedure Error(ResStr: PResStringRec);
  function ReturnAddr: Pointer;
  asm
    mov eax, [esp+8]
  end;
begin
  raise EMenuError.CreateRes(ResStr) at ReturnAddr;
end;

function ColorTo24bitScanLineElement(Color: TColor): T24bitScanLineElement;
begin
  Result.Red := TColorQuad(Color).Red;
  Result.Green := TColorQuad(Color).Green;
  Result.Blue := TColorQuad(Color).Blue;
end;

function ColorTo32bitScanLineElement(Color: TColor): T32bitScanLineElement;
begin
  Result.Red := TColorQuad(Color).Red;
  Result.Green := TColorQuad(Color).Green;
  Result.Blue := TColorQuad(Color).Blue;
  Result.Alpha := TColorQuad(Color).Alpha;
end;

function RGB(Red, Green, Blue: Byte; Alpha: Byte = $00): TColor;
begin
  Result := (Alpha shl 24) or (Blue shl 16) or (Green shl 8) or Red;
end;

function Min(Value1, Value2: Integer): Integer;
begin
  if Value1 > Value2 then
    Result := Value2
  else
    Result := Value1;
end;

function Max(Value1, Value2: Integer): Integer;
begin
  if Value1 < Value2 then
    Result := Value2
  else
    Result := Value1;
end;

end.
