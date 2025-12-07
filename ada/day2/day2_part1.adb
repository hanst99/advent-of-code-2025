with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Assertions;

procedure Day2_Part1 is

   type U64 is mod 2**64;

   package U64_IO is new Ada.Text_IO.Modular_IO(U64);

   procedure Put_Padded(S: String; Width: Natural)
	   with Pre => (S'Length <= Width)
   is
     Padded: String(1..Width) := (others=>' ');
   begin
     Padded((Width - S'Length + 1) .. Width) := S;
     Put(Padded);
   end;

   -------------------------------------------------------------------
   -- Convert substring to UInt64
   -------------------------------------------------------------------
   subtype Digit is Character range '0' .. '9';
   subtype Digit_String is String
   with Dynamic_Predicate => (
	   Digit_String'Length > 0
	   and then (for all C of Digit_String => C in Digit'Range)
	   and then (Digit_String(Digit_String'First) /= '0'));

   function To_U64 (S : Digit_String) return U64 is
      V : U64 := 0;
   begin
      for C of S loop
        V := V * 10 + U64 (Character'Pos (C) - Character'Pos ('0'));
      end loop;
      return V;
   end To_U64;


   -------------------------------------------------------------------
   -- FetchNextRange: minimal translation, no extra renames/variables
   -------------------------------------------------------------------
   function FetchNextRange
     (InputCursor : in out Integer;
      Input       : String;
      Left, Right : out U64) return Boolean
   is
      Begin_Pos, End_Pos : Integer;
      Len : constant Integer := Input'Length;
   begin
      if InputCursor > Len then
         return False;
      end if;

      -- Read left bound up to '-'
      Begin_Pos := InputCursor;
      while InputCursor <= Len and then Input (InputCursor) /= '-' loop
         InputCursor := InputCursor + 1;
      end loop;

      if InputCursor > Len then
         return False;
      end if;

      End_Pos := InputCursor - 1;
      Left    := To_U64 (Input (Begin_Pos .. End_Pos));

      -- skip '-'
      InputCursor := InputCursor + 1;
      Begin_Pos   := InputCursor;

      -- read right bound up to ',' or end
      while InputCursor <= Len and then Input (InputCursor) /= ',' loop
         InputCursor := InputCursor + 1;
      end loop;

      if InputCursor = Len + 1 then
	      End_Pos := Len;
      else
	      End_Pos := InputCursor - 1;
      end if;

      Right := To_U64 (Input (Begin_Pos .. End_Pos));

      -- skip ','
      InputCursor := InputCursor + 1;

      return True;
   end FetchNextRange;

   -------------------------------------------------------------------
   function Even (I : U64) return Boolean is
   begin
      return (I and 1) = 0;
   end Even;

   procedure Print_Debug_Header is
   begin
	   Put_Padded ("Left",           Width => 16);
	   Put_Padded ("nLeft",          Width =>  8);
	   Put_Padded ("10^nLeft",       Width => 16);
	   Put_Padded ("AdjustedLeft",   Width => 16);
	   Put_Padded ("AdjustmentLeft", Width => 16);
	   Put_Padded ("Right",          Width => 16);
	   Put_Padded ("nRight",         Width =>  8);
	   Put_Padded ("10^nRight",      Width => 16);
	   Put_Padded ("AdjustedRight",  Width => 16);
	   Put_Padded ("Sum",            Width => 16);
	   New_Line;
   end Print_Debug_Header;
   -------------------------------------------------------------------
   -- Column print function for debug output (matches Pascal alignment)
   -------------------------------------------------------------------
   procedure Print_Debug_Line
     (Left, LeftDigits, PowL, AdjLeft : U64;
      AdjLeftTxt                      : String;
      Right, RightDigits, PowR,
      AdjRight, Sum                  : U64)
   is
   begin
      U64_IO.Put (Left,        Width => 16);
      U64_IO.Put (LeftDigits,  Width => 8);
      U64_IO.Put (PowL,        Width => 16);
      U64_IO.Put (AdjLeft,     Width => 16);
      Put_Padded (AdjLeftTxt, Width => 16);
      U64_IO.Put (Right,       Width => 16);
      U64_IO.Put (RightDigits, Width => 8);
      U64_IO.Put (PowR,        Width => 16);
      U64_IO.Put (AdjRight,    Width => 16);
      U64_IO.Put (Sum,         Width => 16);
      New_Line;
   end Print_Debug_Line;

   procedure Set_Padded(Buffer: in out String; Text: String)
   with Pre => Buffer'Length >= Text'Length
   is begin
	   Buffer := (others => ' ');
	   Buffer((Buffer'Last - Text'Length + 1) .. Buffer'Last) := Text;
   end Set_Padded;
   -------------------------------------------------------------------
   -- SumMirrorsInRange
   -------------------------------------------------------------------
   function SumMirrorsInRange (Left, Right : U64) return U64 is
      AdjustedLeft, AdjustedRight, 
      LeftDigits, RightDigits,     
      K                           : U64 := 0;

      NthPowerOfTen          : U64 := 10;
      NthPowerOfTenRight     : U64 := 10;

      HalfNthPowerOfTen      : U64 := 1;
      HalfNthPowerOfTenRight : U64 := 1;

      HighLeft, HighRight : U64 := 0;
      LeftBound, RightBound : U64 := 0;
      KthSum, Sum : U64 := 0;

      AdjustmentLeft : String(1..8) := (others => ' ');
   begin
      if Right < Left then
         return 0;
      end if;

      ----------------------------------------------------------------
      -- Compute digit info for Left
      ----------------------------------------------------------------
      LeftDigits := 1;
      while Left / NthPowerOfTen > 0 loop
         LeftDigits := LeftDigits + 1;
         NthPowerOfTen := NthPowerOfTen * 10;
         if Even (LeftDigits) then
            HalfNthPowerOfTen := HalfNthPowerOfTen * 10;
         end if;
      end loop;

      HighLeft := Left / HalfNthPowerOfTen;

      if not Even (LeftDigits) then
         Set_Padded(AdjustmentLeft, "a.1");
         LeftDigits := LeftDigits + 1;
         AdjustedLeft := NthPowerOfTen + HalfNthPowerOfTen;
         HighLeft := NthPowerOfTen;
         NthPowerOfTen := NthPowerOfTen * 10;
         HalfNthPowerOfTen := HalfNthPowerOfTen * 10;
      else
         if (Left mod HalfNthPowerOfTen) > HighLeft then
            Set_Padded(AdjustmentLeft, "a.2.b");
            HighLeft := HighLeft + 1;
         else
            Set_Padded(AdjustmentLeft, "a.2.a");
         end if;
         AdjustedLeft := HighLeft * (1 + HalfNthPowerOfTen);
      end if;

      Ada.Assertions.Assert (AdjustedLeft >= Left);
      Ada.Assertions.Assert
        ((AdjustedLeft / HalfNthPowerOfTen) =
         (AdjustedLeft mod HalfNthPowerOfTen));

      if AdjustedLeft > Right then
         return 0;
      end if;

      ----------------------------------------------------------------
      -- Compute digit info for Right
      ----------------------------------------------------------------
      RightDigits := LeftDigits;
      NthPowerOfTenRight := NthPowerOfTen;
      HalfNthPowerOfTenRight := HalfNthPowerOfTen;
      while Right / NthPowerOfTenRight > 0 loop
         RightDigits := RightDigits + 1;
         NthPowerOfTenRight := NthPowerOfTenRight * 10;
         if Even (RightDigits) then
            HalfNthPowerOfTenRight := HalfNthPowerOfTenRight * 10;
         end if;
      end loop;

      HighRight := Right / HalfNthPowerOfTenRight;

      if not Even (RightDigits) then
         RightDigits := RightDigits - 1;
         NthPowerOfTenRight := NthPowerOfTenRight / 10;
         AdjustedRight := NthPowerOfTenRight - 1;

      elsif (Right mod HalfNthPowerOfTenRight) >= HighRight then
         AdjustedRight := HighRight * (1 + HalfNthPowerOfTenRight);

      else
         HighRight := HighRight - 1;
         if HighRight + 1 = HalfNthPowerOfTenRight then
            RightDigits := RightDigits - 2;
            NthPowerOfTenRight := NthPowerOfTenRight / 100;
            HalfNthPowerOfTenRight := HalfNthPowerOfTenRight / 10;
            AdjustedRight := NthPowerOfTenRight - 1;
         else
            AdjustedRight :=
              HighRight * (1 + HalfNthPowerOfTenRight);
         end if;
      end if;

      Ada.Assertions.Assert (AdjustedRight <= Right);

      Ada.Assertions.Assert
        ((AdjustedRight / HalfNthPowerOfTenRight) =
         (AdjustedRight mod HalfNthPowerOfTenRight));

      if AdjustedRight < AdjustedLeft then
         return 0;
      end if;

      ----------------------------------------------------------------
      -- Sum accumulation loop
      ----------------------------------------------------------------
      K := 0;
      Sum := 0;

      while LeftDigits + K <= RightDigits loop

         if K = 0 then
            LeftBound := AdjustedLeft / HalfNthPowerOfTen;
         else
            LeftBound := HalfNthPowerOfTen;
         end if;

         if LeftDigits + K = RightDigits then
            Ada.Assertions.Assert
              (HalfNthPowerOfTen = HalfNthPowerOfTenRight);
            RightBound := AdjustedRight / HalfNthPowerOfTen;
         else
            RightBound := HalfNthPowerOfTen - 1;
         end if;

         KthSum := (RightBound * (RightBound + 1)) / 2;
         KthSum :=
           KthSum - (LeftBound * (LeftBound + 1)) / 2 + LeftBound;
         KthSum := KthSum + KthSum * HalfNthPowerOfTen;

         Sum := Sum + KthSum;

         K := K + 2;
         HalfNthPowerOfTen := HalfNthPowerOfTen * 10;
      end loop;

      Ada.Assertions.Assert (LeftDigits + K = RightDigits + 2);

      ----------------------------------------------------------------
      -- Externalised debug print
      ----------------------------------------------------------------
      Print_Debug_Line
        (Left => Left, LeftDigits => LeftDigits, PowL => NthPowerOfTen, AdjLeft => AdjustedLeft,
		 AdjLeftTxt => AdjustmentLeft, Right => Right, RightDigits => RightDigits,
         PowR => NthPowerOfTenRight, AdjRight => AdjustedRight, Sum => Sum);

      return Sum;
   end SumMirrorsInRange;

   -------------------------------------------------------------------
   -- Main
   -------------------------------------------------------------------
   Input        : String := Get_Line;
   Left, Right  : U64;
   Cursor       : Integer := 1;
   TotalMirrors : U64 := 0;

begin
   Print_Debug_Header;

   while FetchNextRange (Cursor, Input, Left, Right) loop
      TotalMirrors := TotalMirrors + SumMirrorsInRange (Left, Right);
   end loop;

   Put_Line (U64'Image (TotalMirrors));
end Day2_Part1;
