program Day1_Part1;
const
  Initial_Dial_Number = 50;
  Dial_Max = 100;
var
  Dial_Direction: Char;
  Dial_Number, Dial_Length, Password: Int32;
begin
  Dial_Number := Initial_Dial_Number;
  Password := 0;

  while not Eof do
  begin
    ReadLn(Dial_Direction, Dial_Length);

    if Dial_Direction = 'L'
    then Dial_Number := Dial_Number - Dial_Length
    else Dial_Number := Dial_Number + Dial_Length;

    Dial_Number := Dial_Number mod Dial_Max;

    if Dial_Number = 0
    then Password := Password + 1;
  end;

  WriteLn(Password);
end.
