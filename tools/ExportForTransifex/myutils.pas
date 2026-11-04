unit MyUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure ChangeLineEndingsInFile(const AFileName:string; const ANewLineEnding: string);
function DetectLineEndingsInFile(const AFileName:string):string;
//function DetectLineEndings(const AStr:string):string;
function DetectLineEndings(const s : AnsiString;  out LineEndings : AnsiString) : boolean;  // returns False if string is empty or ambiguous
function DbgLineEndings(const s: string): string;

implementation

uses LazUtils,streamex,regexpr, Math;

procedure ChangeLineEndingsInFile(const AFileName:string; const ANewLineEnding: string);
var
  sl:TStringList;
  s:string;
begin
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    sl.LineBreak:=ANewLineEnding;
    sl.SaveToFile(AFileName);
  finally
    sl.free;
  end;

end;

function DetectLineEndingsInFile(const AFileName: string): string;
{var
  s:string;
  sl:TStringList;
begin
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    s:=sl.Text;

    if not DetectLineEndings(s, result) then
      result := '';
  finally
    sl.free;
  end;
end;       }
var
  f:file;
  s:string;
  a: array[0..1023] of byte;
  cnt:integer;
  len:integer;
  i:integer;
begin
  AssignFile(f, AFileName);
  reset(f);
  len:=  {min(SizeOf(a), FileSize(f))}1024;
  BlockRead(f, a, len, cnt);
  {s := UTF8Decode(StringOf(a));
  if not DetectLineEndings(s, result) then
    result := '';  }
  result :='';
  for i := 0 to cnt-1 do
  begin
    if (a[i] = 13) and ( a[i+1] = 10) then
      Result := #13#10
    else if (a[i] = 10) and ( a[i+1] = 13) then
      Result := #10#13
    else if (a[i] = 10) then
      Result := #10
    else if (a[i] = 13)then
      Result := #13;

    if result <> '' then
      break;
  end;
  CloseFile(f);
end;

function DetectLineEndings(const s: AnsiString; out LineEndings: AnsiString
  ): boolean;
const
        CR   = #13;
        LF   = #10;
        CRLF = CR + LF;
var
        i, Len : QWord;
        Pair   : array[0..1] of AnsiChar;
begin
        Len := Length(s);  if (Len = 0) then exit(False);
        LineEndings := '';
        {$push}  {$RangeChecks off}
        for i := 1 to Len do begin
                Move(s[i], Pair, 2);  // AnsiString is terminated by a #0, so this won't result in access violations
                if (Pair[0] >= #32) then continue;
                if (Pair = CRLF) then begin
                        if (LineEndings = CRLF) then continue            else
                        if (LineEndings = ''  ) then LineEndings := CRLF else exit(False);
                end else
                if (Pair[0] = CR) then begin
                        if (LineEndings = CR  ) then continue            else
                        if (LineEndings = ''  ) then LineEndings := CR   else exit(False);
                end else
                if (Pair[0] = LF) then begin
                        if (LineEndings =   LF) then continue            else
                        if (LineEndings =   '') then LineEndings :=   LF else exit(False);
                end;
        end;
        {$pop}
        Result := True;
end;

{function DetectLineEndings(const AStr:string):string;
var
regex: TRegexpr;
begin
  result:='';

  regex:=TRegexpr.Create;
  try
    regex.expression:='([\r\n]{1,2})';
    regex.ModifierS:=true;
    regex.ModifierM:=true;
    if regex.exec then
      result := regex.match[1];
  finally
    regex.free;
  end;
end;  }

function DbgLineEndings(const s: string): string;
var tmp:string;
begin
  tmp:=StringReplace(s, #13, '<CR>', [rfReplaceAll]);
  tmp:=StringReplace(tmp, #10, '<LF>', [rfReplaceAll]);
  writeln(tmp);
end;

end.

