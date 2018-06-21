Unit ExactFloatToStr_JH0;

(* *****************************************************************************

  This module includes
    (a) functions for converting a floating binary point number to its
        *exact* decimal representation in an AnsiString;
    (b) functions for parsing the floating point types into sign, exponent,
        and mantissa; and
    (c) function for analyzing a extended float number into its type (zero,
        normal, infinity, etc.)

  Its intended use is for trouble shooting problems with floating point numbers.

  This code uses dynamic arrays, overloaded calls, and optional parameters.

  These routines are not very optimized for speed or space.  I plan to
      replace the individual bit-shifts and multiplies-by-ten with mul-
      tiple versions of same.  Consider making an object so that the
      arrays don't have to reallocated so often. And consider making an
      output buffer character array so that the Result will be allocated
      only once.

  Rev. 2003.01.01 by JFH to add the three ParseFloat functions.
  Rev. 12/26/2002 by JFH to bracket the DEBUG code with conditionals.
  Rev. 12/25/2002 by JFH to fix 1E20 (BinExp) problem and check for zero and
      other special values.
  Pgm. 12/24/2002 by John Herbster for Delphi programmers everywhere.

***************************************************************************** *)

{ Turn DeBUG on to make available detail debugging at expense of speed.}
{$Define notDEBUG}

Interface

Function ExactFloatToStr(const Value: extended): AnsiString;
{ This call uses the global DecimalSeparator and ThousandSeparator.
  (It can be slow for very large or very small extended numbers.) }

Function ExactFloatToStrEx(const Value: extended;
                DecimalPoint: char = '.'; ThousandsSep: char = ' '): AnsiString;
{ Use #0 for the ThousandsSep if *no* grouping breaks are desired. }

Function ParseFloat(const Value: extended): AnsiString; overload;
Function ParseFloat(const Value: double): AnsiString;   overload;
Function ParseFloat(const Value: single): AnsiString;   overload;
{ These calls parse a float value to its sign, exponent, and mantissa. }

Function FloatingBinPointToDecStr
    (const Value; const ValNbrBits, ValBinExp: integer; Negative: boolean;
     DecimalPoint: char = '.'; ThousandsSep: char = ' '): AnsiString;
{ This is the basic conversion engine. }

Type tTypeFloat = (tfUnknown,tfNormal,tfZero,tfDenormal,tfIndefinite,
                   tfInfinity,tfQuietNan,tfSignalingNan);

Procedure AnalyzeFloat(const Value: extended; var NumberType: tTypeFloat;
                var Negative: boolean; var Exponent: word; var Mantissa: int64);

Var LogFmtX:
        procedure (const Fmt: AnsiString; const Data: array of const) of object;

{$IfDef DEBUG}
Const Debug = true;
{$Else}
Const Debug = false;
{$EndIf}

Implementation

Uses SysUtils;

Type tSglWord = Word; {Consider Byte or Word}
     tDblWord = LongWord; {Consider Word or LongWord}

Const SizeOfAryElem = SizeOf(tSglWord);
      BitsInBufElem = SizeOfAryElem*8;
      DecDigits: array [0..9] of char = '0123456789';

{$IfDef DEBUG}

Procedure LogFmt(const Fmt: AnsiString; const Data: array of const);
begin
  If Assigned(LogFmtX)
    then LogFmtX(Fmt,Data);
end;

{$EndIf}

Procedure MultiplyAndAdd
    (Multiplican, Multiplier, CryIn: tSglWord;
     var CryOut, Product: tSglWord);
var Tmp: packed record case byte of 0: (W: tDblWord); 1:(L,H: tSglWord); end;
Begin
  Tmp.W  := Multiplican * Multiplier + CryIn;
  CryOut  := Tmp.H;
  Product := Tmp.L;
End;

Function DivideAndRemainder
    (NumeratorHi, NumeratorLo: tSglWord; Divisor: tSglWord;
     var Quotient, Remainder: tSglWord): boolean;
var Tmp1,Tmp2: packed record case byte of 0: (W: tDblWord); 1:(L,H: tSglWord); end;
Begin
  Result := (Divisor <> 0);
  If Result then begin
    Tmp1.H := NumeratorHi;
    Tmp1.L := NumeratorLo;
    Tmp2.W := Tmp1.W div Divisor;
    If (Tmp2.H <> 0)
      then Result := false
      else begin
        Quotient  := Tmp2.L;
        Remainder := Tmp1.W mod Divisor;
        end;
    end;
End;

Function FloatingBinPointToDecStr
    (const Value; const ValNbrBits, ValBinExp: integer; Negative: boolean;
     DecimalPoint: char = '.'; ThousandsSep: char = ' '): AnsiString;

{ Value = Mantissa * 2^BinExp * 10^DecExp }
Var Man: array of tSglWord; CryE: tSglWord; Cry: tDblWord;
    NbrManElem,
    BinExp{neg of # binary fraction bits},
    DecExp{neg of # decimal fraction bits},
    NbrDecFraDigits,
    i, j, Tmp: integer; c: char;
    Tmp1: packed record case byte of 0: (W: tDblWord); 1:(L,H: tSglWord); end;
Label Finish;

{$IfDef DEBUG}
{sub}procedure LogManExp(const Rem: string);
  var s: AnsiString; k: integer;
  begin
    LogFmt('%s: BinExp=%d, DecExp=%d, NbrManElem=%d',
           [Rem,BinExp,DecExp,NbrManElem]);
    s := '';
    For k := 0 to NbrManElem - 1 do s := Format(' %2.2x',[Man[k]]) + s;
    LogFmt('  %s',[s]);
  end;
{$EndIf}

Begin

{ Load Mantissa and binary exponent: }
  NbrManElem := (ValNbrBits + BitsInBufElem - 1) div BitsInBufElem;
  SetLength(Man,NbrManElem);
  Move(Value,Man[0],(ValNbrBits + 7) div 8); {Assuming little endian input}
{ Set exponents: (Value = Mantissa * 2^BinExp * 10^DecExp) }
  BinExp := ValBinExp;
  DecExp := 0;

{ Reduce mantissa to mininum number of bits
      (i.e. while mantissa is odd, div by 2 and inc binary exponent): }
    {$IfDef DEBUG}
  LogManExp('Before trimming');
    {$EndIf}
  While (NbrManElem > 0) and (BinExp < 0) and not Odd(Man[0]) do begin
    Cry := 0;
    for i := NbrManElem - 1 downto 0 do begin
      Tmp := (Cry shl BitsInBufElem) or Man[i];
      Man[i] := (Tmp shr 1);
      Cry := Tmp and 1;
      end;
    inc(BinExp);
    {$IfDef DEBUG}
    LogManExp('Shifting down');
    {$EndIf}
    If Man[NbrManElem - 1] = 0
      then dec(NbrManElem);
    end{while};

{ Check for zero: }
  If NbrManElem = 0 then begin
    Result := '0';
    Goto Finish;
    end;

{ Repeatably multiply by 10 until there is no more fraction. Decrement the
      DecExp at the same time.  Note that a multiply by 10 is same as mul.
      by 5 and inc of BinExp exponent.  Also note that a multiply by 5 adds
      two or three bits to number of mantissa bits. }
  NbrDecFraDigits := -BinExp; {Observe! 0.5, 0.25, 0.125, 0.0625, 0.03125, ...}
  i := NbrManElem + (3*NbrDecFraDigits + BitsInBufElem - 1) div BitsInBufElem;
  If length(Man) < i
        then SetLength(Man,i);
    {$IfDef DEBUG}
  LogManExp('Prep mul out');
    {$EndIf}
  For i := 1 to NbrDecFraDigits do begin
    CryE := 0;
    For j := 0 to NbrManElem - 1
        do MultiplyAndAdd(Man[j],5,CryE,CryE,Man[j]);
//         MultiplyAndAdd(Multiplican, Multiplier, CryIn: tSglWord;
//                        var CryOut, Product: tSglWord);
    If CryE <> 0 then begin
      Inc(NbrManElem);
      Man[NbrManElem-1] := CryE;
      end;
    inc(BinExp);
    dec(DecExp);
    {$IfDef DEBUG}
    LogManExp('Mul out');
    {$EndIf}
    end{i-loop};
    {$IfDef DEBUG}
  LogManExp('Finished multiplies');
    {$EndIf}

{ Finish reducing BinExp to 0 by shifting mantissa up: }
  While (BinExp > 0) do begin
    Cry := 0;
    for i := 0 to NbrManElem - 1 do begin
      Tmp1.W := Man[i] shl 1;
      Man[i] := Tmp1.L + Cry;
      Cry := Tmp1.H;
      end;
    dec(BinExp);
    If Cry <> 0 then begin
      inc(NbrManElem);
      If length(Man) < NbrManElem
        then SetLength(Man,NbrManElem);
      Man[NbrManElem - 1] := Cry;
      end;
    {$IfDef DEBUG}
    LogManExp('Shifting up');
    {$EndIf}
    end{while};

{ Repeatably divide by 10 and use remainders to create decimal AnsiString: }
  Result := ''; {DEBUG}
    {$IfDef DEBUG}
  LogManExp('Before division');
    {$EndIf}
  Repeat
  { If not first then place separators: }
    If Result <> '' then if DecExp = 0
      then Result := DecimalPoint + Result
      else if (ThousandsSep = ' ') and ((DecExp mod 5) = 0)
        then Result := ' ' + Result
        else if not (ThousandsSep in [#0,' ']) and ((DecExp mod 3) = 0)
          then Result := ThousandsSep + Result;
  { DivideAndRemainder mantissa array by 10: }
    CryE := 0;
    For i := NbrManElem - 1 downto 0
        do DivideAndRemainder(CryE,Man[i],10,Man[i],CryE);
//         DivideAndRemainder(NumeratorHi, NumeratorLo: Byte;  Divisor: Byte;
//                            var Quotient, Remainder: Byte): boolean;
    Inc(DecExp);
    c := DecDigits[CryE];
    Result := c + Result;
    If (NbrManElem > 0) and (Man[NbrManElem - 1]=0)
      then dec(NbrManElem);
    Until (DecExp > 0) and (NbrManElem = 0);

Finish:
  If Negative
    then Result := '- ' + Result
    else Result := '+ ' + Result;
End;

Procedure AnalyzeFloat(const Value: extended; var NumberType: tTypeFloat;
                var Negative: boolean; var Exponent: word; var Mantissa: int64);
var ValueRec: packed record Man: Int64; Exp: word end absolute Value;
begin
  Mantissa :=  ValueRec.Man;
  Negative := (ValueRec.Exp and $8000)<>0;
  Exponent := (ValueRec.Exp and $7FFF);
  If (Exponent = $7FFF)
    then if (Mantissa = 0)
      then NumberType := tfInfinity
      else begin
        Mantissa := (Mantissa and $3FFFFFFFFFFFFFFF);
        if ((ValueRec.Man and $4000000000000000) = 0)
          then NumberType := tfSignalingNAN
          else if (Mantissa = 0)
            then NumberType := tfIndefinite
            else NumberType := tfQuietNAN
        end
    else if (Exponent = 0)
      then if (Mantissa = 0)
        then NumberType := tfZero
        else NumberType := tfDenormal
      else NumberType := tfNormal;
end;

Function ExactFloatToStrEx(const Value: extended;
                DecimalPoint: char = '.'; ThousandsSep: char = ' '): AnsiString;
var NumberType: tTypeFloat; Negative: boolean; Exponent: word; Mantissa: int64;
Begin
  AnalyzeFloat(Value,NumberType,Negative,Exponent,Mantissa);
  Case NumberType of
    tfNormal      : Result := FloatingBinPointToDecStr
        (Mantissa, {NbrBits}64, {BinExp}(Exponent - $3FFF) - 63,
         Negative, DecimalPoint, ThousandsSep);
    tfZero: If Negative
               then Result := '- 0'
               else Result := '+ 0';
    tfDenormal    : Result := FloatingBinPointToDecStr
        (Mantissa, {NbrBits}64, {BinExp}(- $3FFF - 62),
         Negative, DecimalPoint, ThousandsSep);
    tfIndefinite  : Result := 'Indefinite';
    tfInfinity: If Negative
               then Result := '- Infinity'
               else Result := '+ Infinity';
    tfQuietNan    : Result := Format('QNaN(%d)',[Mantissa]);
    tfSignalingNan: Result := Format('SNaN(%d)',[Mantissa]);
    else            Result := 'UnknownNumberType';
    end{cases};
End;

Function ExactFloatToStr(const Value: extended): AnsiString;
Begin
  Result := ExactFloatToStrEx(Value,DecimalSeparator,ThousandSeparator);
End;

Function ParseFloat(const Value: extended): AnsiString;
{ This call parses an extended value to its sign, exponent, and mantissa. }
var ValueRec: packed record Man: Int64; Exp: word end absolute Value;
const PN: array [boolean] of char = '+-';
Begin
  Result := Format('Ext(Sgn="%s",Exp=$%4.4x,Man=$%16.16x)',
                   [PN[(ValueRec.Exp and $8000) <> 0],
                       (ValueRec.Exp and $7FFF),
                        ValueRec.Man]);
End;

Function ParseFloat(const Value: double): AnsiString;
{ This call parses a double value to its sign, exponent, and mantissa. }
var ValueRec: Int64 absolute Value;
const PN: array [boolean] of char = '+-';
Begin
  Result := Format('Dbl(Sgn="%s",Exp=$%3.3x,Man=$%13.13x)',
                   [PN[(ValueRec and $8000000000000000) <> 0],
                      ((ValueRec and $7FF0000000000000) shr 52),
                       (ValueRec and $000FFFFFFFFFFFFF)]);
End;

Function ParseFloat(const Value: single): AnsiString;
{ This call parses a single value to its sign, exponent, and mantissa. }
var ValueRec: LongInt absolute Value;
const PN: array [boolean] of char = '+-';
Begin
  Result := Format('Sgl(Sgn="%s",Exp=$%2.2x,Man=$%6.6x)',
                   [PN[(ValueRec and $80000000) <> 0],
                      ((ValueRec and $7F800000) shr 23),
                       (ValueRec and $007FFFFF)]);
End;

End.

