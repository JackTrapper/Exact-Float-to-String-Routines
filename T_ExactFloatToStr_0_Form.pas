unit T_ExactFloatToStr_0_Form;

(* *****************************************************************************

  For Testing ExactFloatToStr and ParseFloat functions.

  Pgm. 12/24/2002 by John Herbster.

***************************************************************************** *)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Convert_b: TButton;
    Memo1: TMemo;
    ShowDebug_ck: TCheckBox;
    CallExVer_ck: TCheckBox;
    CkSmallest_b: TButton;
    CkDenormal2_b: TButton;
    CkSpecials_b: TButton;
    SmallestD_b: TButton;
    Pi_b: TButton;
    CkAnalyzeFloat_b: TButton;
    procedure Convert_bClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure CkSmallest_bClick(Sender: TObject);
    procedure CkDenormal2_bClick(Sender: TObject);
    procedure CkSpecials_bClick(Sender: TObject);
    procedure SmallestD_bClick(Sender: TObject);
    procedure Pi_bClick(Sender: TObject);
    procedure CvtToHex_bClick(Sender: TObject);
    procedure CkAnalyzeFloat_bClick(Sender: TObject);
  private
    procedure TestNumber(Value: extended);
  public
    procedure Log(const msg: string);
    procedure LogFmt(const Fmt: string; const Data: array of const);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses ExactFloatToStr_JH0;

function GetCpuClockCycleCount: Int64;
asm
    dw $310F  // opcode for RDTSC
end;

procedure TForm1.Log(const msg: string);
begin
  Memo1.Lines.Add(msg);
end;

procedure TForm1.LogFmt(const Fmt: string; const Data: array of const);
begin
  Log(Format(Fmt,Data));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowDebug_ck.Enabled := ExactFloatToStr_JH0.Debug;
  Edit1.Text := FloatToStr(1); 
end;

procedure TForm1.TestNumber(Value: extended);
var ExtX: packed record Man: Int64; Exp: word end absolute Value;
    cc: int64; ValE4K: extended; s: string;
begin
  If ShowDebug_ck.Checked
    then ExactFloatToStr_JH0.LogFmtX := LogFmt
    else ExactFloatToStr_JH0.LogFmtX := nil;
  If abs(Value) < 1E-4000
    then begin
      ValE4K := Value * 1E4000;
      LogFmt('Calling: Exp=$%4.4x, Man=$%16.16x, G=%g, Ge4K=%g',
           [ExtX.Exp,ExtX.Man,Value,ValE4K]);
      end
    else LogFmt('Calling: Exp=$%4.4x, Man=$%16.16x, G=%g',
                 [ExtX.Exp,ExtX.Man,Value]);
  Try{Except}
    cc := GetCpuClockCycleCount;
    If CallExVer_ck.Checked
      then s := ExactFloatToStrEx(Value)
      else s := ExactFloatToStr  (Value);
    cc := GetCpuClockCycleCount - cc;
    LogFmt('  Required %s clock cycles',[ExactFloatToStr(cc)]);
    Log(s);
  Except
    On e:Exception
      do LogFmt('Exception: %s',[e.Message]);
  End;
end;

procedure StrToFloatProc(const Str: string; var Value: extended);
var s: string; i,j: integer;
begin
  s := Str;
  j := 0;
  For i := 1 to length(s) do begin
    If s[i] in ['-','0'..'9','.','e','E']
      then begin inc(j); s[j] := s[i] end;
    end;
  SetLength(s,j);
  Value := StrToFloat(s);
end;

procedure TForm1.Convert_bClick(Sender: TObject);
var ext: extended;
begin
  Screen.Cursor := crHourGlass;
  Memo1.Lines.Add('');
  Try
    StrToFloatProc(Edit1.Text,ext);
    TestNumber(ext);
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  If Key <> ^M then EXIT;
  Key := #0;
  Convert_bClick(Sender);
end;

procedure TForm1.CkSmallest_bClick(Sender: TObject);
var ext: extended; i: integer;
var ExtX: packed record Man: Int64; Exp: word end absolute ext;
begin
  Memo1.Lines.Add('');
  Screen.Cursor := crHourGlass;
  Try
    ExtX.Exp := 0; ExtX.Man := $0000000000000001;
    For i := 1 to 2 do begin
      TestNumber(ext);
      ext := ext / 2;
      end;
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TForm1.CkDenormal2_bClick(Sender: TObject);
var ext: extended; i: integer;
var ExtX: packed record Man: Int64; Exp: word end absolute ext;
    ext2: extended;
begin
  Memo1.Lines.Add('');
  Screen.Cursor := crHourGlass;
  Try
    ExtX.Exp := 2; ExtX.Man := $8000000000000000;
    For i := 1 to 9 do begin
      ext2 := ext*1e4900;
      LogFmt('Test #%d: Exp=$%4.4x, Man=$%16.16x, G=%g, G2=%g',
              [i,ExtX.Exp,ExtX.Man,ext,ext2]);
      If (i in [2,3,4]) then begin
        TestNumber(ext);
        end;
      If i < 5
        then ext := ext / 2
        else ext := ext * 2;
      end;
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TForm1.CkSpecials_bClick(Sender: TObject);
var ext: extended; dbl: double;
    ExtX: packed record Man: Int64; Exp: word end absolute ext;
    DblX: int64 absolute dbl;
const
  NanX = 0/0;
  DblSgnX: Int64    = $8000000000000000; {1 bit}
  DblExpX: Int64    = $7FF0000000000000; {11 bits}
  DblManX: Int64    = $000FFFFFFFFFFFFF; {52 bits (+ 1 = 53)}
begin
  Screen.Cursor := crHourGlass;
  Try

  { Test infinities: }
    Log('');
    ExtX.Exp := $7FFF; ExtX.Man := $0000000000000000;
    Log('+Inf response = ' + ExactFloatToStr(ext));
    ExtX.Exp := $FFFF; ExtX.Man := $0000000000000000;
    Log('-Inf response = ' + ExactFloatToStr(ext));

  { Test indefinite: }
    Log('');
    ext := NanX;
    LogFmt('Exp=$%4.4x, Man=$%16.16x',[ExtX.Exp,ExtX.Man]);
    Log('Indefinite response = ' + ExactFloatToStr(ext));
    dbl := ext;
    ext := dbl;
    LogFmt('Dbl: Exp=$%3.3x, Man=$%13.13x',
                [(DblX shr (13*4)),(DblX and DblManX)]);
    LogFmt('Ext: Exp=$%4.4x, Man=$%16.16x',[ExtX.Exp,ExtX.Man]);
    Log('Indefinite dbl rsp. = ' + ExactFloatToStr(ext));

  { Test QNANs: }
    Log(''); 
    ExtX.Exp := $7FFF; ExtX.Man := $C100000000000000;
    Log('QNAN(1) response = ' + ExactFloatToStr(ext));
    ExtX.Exp := $7FFF; ExtX.Man := $8100000000000000;
    Log('SNAN(1) response = ' + ExactFloatToStr(ext));
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TForm1.SmallestD_bClick(Sender: TObject);
var d1, d2: double;
begin
  Memo1.Lines.Add('');
  d1 := 1;
  Repeat
    d2 := d1; d1 := d1 / 2;
    Until d1 = 0;
  Edit1.Text := FloatToStr(d2);
  TestNumber(d2);
end;

procedure TForm1.Pi_bClick(Sender: TObject);
var ext: extended; d: double;
var ExtX: packed record Man: Int64; Exp: word end absolute ext;
begin
  Memo1.Lines.Add('');
  ext := pi;
  Edit1.Text := FloatToStr(ext);
  TestNumber(ext);
  d := pi;
  Edit1.Text := FloatToStr(d);
  TestNumber(d);
end;

procedure TForm1.CvtToHex_bClick(Sender: TObject);
Var ext: extended;
var ExtX: packed record Man: Int64; Exp: word end absolute ext;
begin
end;

procedure TForm1.CkAnalyzeFloat_bClick(Sender: TObject);
var ext, ext2: extended; dbl: double; sgl: single; i: integer;
  { Equivalence a record to var ext: }
    ExtX: packed record Man: Int64; Exp: word end absolute ext;
    DblX: Int64 absolute dbl;  SglX: LongInt absolute sgl;  s: string;
begin
  Assert(SizeOf(ExtX) = SizeOf(ext));
  Assert(SizeOf(DblX) = SizeOf(dbl));
  Assert(SizeOf(SglX) = SizeOf(sgl));
  For i := 0 to 20 do begin
    Case i of
      0: begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Check simple numbers.');
        ext := 15;
       end;
      3: begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Check crossover into sgl denormal.');
      { Set ext = 2 * <single normal minimum>: }
        SglX := LongInt(2) shl 23;
        ext := sgl;
        end;
      7: begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Check crossover into dbl denormal.');
      { Set ext = 2 * <double normal minimum>: }
        DblX := Int64(2) shl 52;
        s := ParseFloat(dbl);
        ext := dbl;
        end;
      11: begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Check crossover into ext denormal.');
      { Set ext = 2 * <extended normal minimum>: }
        ExtX.Exp := 2;
        ExtX.Man := $8000000000000000;
        end;
      15: begin
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Check cross over into zero.');
      { Set ext = 2 * <external denormal minimum>: }
        ExtX.Exp := 0;
        ExtX.Man := $0000000000000002;
        end;
      { Divide the number to be analyzed by 2: }
      else{case} begin
        ext := ext / 2;
        Memo1.Lines.Add(' divide by 2 and check');
        end;
    end{cases};
    dbl := ext;
    sgl := ext;
  { Set ext2 to same ext value times 10^4900: }
    ext2 := ext*1e4900;
  { Save the analysis to memo box: }
    Memo1.Lines.Add(Format('  %2.2d: Nbr=%g ((Nbr x 1e4900)=%g)',[i, ext, ext2]));
    Memo1.Lines.Add('  ' + ParseFloat(ext) + ' ' + ParseFloat(dbl)
                   + ' ' + ParseFloat(sgl));
    end{i-loop};
end;

end.
