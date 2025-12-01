program Day1_Part2;
const
  Initial_Dial_Number = 50;
  Dial_Max = 100;
var
  Dial_Direction: Char;
  Dial_Number, Previous_Dial_Number, Dial_Length, Password: Int32;

  function TrueMod (x,y: Int32): Int32;
  begin
    x := x mod y;
    if x < 0
    then TrueMod := x + y
    else TrueMod := x;
  end;

  procedure Turn_Dial;
  begin
    if Dial_Direction = 'L'
    then begin
        Dial_Number := TrueMod(Dial_Number - Dial_Length, Dial_Max);
        if (Previous_Dial_Number > 0) and (Previous_Dial_Number - Dial_Length <= 0)
        then Password := Password + 1;
      end
    else begin
        Dial_Number := TrueMod(Dial_Number + Dial_Length, Dial_Max);
        if (Previous_Dial_Number + Dial_Length >= Dial_Max)
        then Password := Password + 1;
    end;
  end;
begin
  Dial_Number := Initial_Dial_Number;
  Password := 0;

  while not Eof do
  begin
    ReadLn(Dial_Direction, Dial_Length);
    { Account for huge spin }
    Password := Password + (Dial_Length div Dial_Max);
    Dial_Length := Dial_Length mod Dial_Max;

    Previous_Dial_Number := Dial_Number;
    if Dial_Length <> 0
    then Turn_Dial;
  end;

  WriteLn(Password);
end.
