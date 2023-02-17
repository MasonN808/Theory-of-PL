program CaesarCipher;
    uses sysutils, character;
    
    function uppercaseAndValidateString(inputString: string): string;
    var
        upperCaseString: string;
        c: char;
    begin
    upperCaseString := '';
       for c in inputString do
            begin
                // skip if char is a space
                if (not (c = ' ') and not (c = '.')) then
                      begin
                          // Check for invalid characters
                          if (character.IsLetter(c)) then
                            begin
                                // Capitalize the characters
                                if (('a' <= c) and (c <= 'z')) then
                                    begin
                                        // Append character to end of string
                                        upperCaseString := upperCaseString + UpCase(c) 
                                    end
                                else
                                    begin
                                        upperCaseString := upperCaseString + c
                                    end
                            end
                          else
                            begin
                                Writeln('Invalid character: ' + c);
                                exit()
                            end;
                      end
                else
                    begin
                        // Append character to end of string
                        upperCaseString := upperCaseString + c; 
                    end;
            end;
        exit(upperCaseString);
    end;
    
    function encrypt(inputString: string; shiftConstant: integer): string;
    var
        upperCaseString: string;
        encryptedString: string;
        shiftedASCIIInteger: Integer;
        c: char;
    begin
    writeln('Encrypting...');
    upperCaseString := uppercaseAndValidateString(inputString);
    encryptedString := '';
       for c in upperCaseString do
            begin
                // skip if char is a space
                if (not (c = ' ') and not (c = '.')) then
                      begin
                        // Shift the letters right relative to the shiftConstant
                        if ((ord(c) + shiftConstant) > 90) then
                            begin
                                shiftedASCIIInteger := ((ord(c) + shiftConstant) mod 91) + 65
                            end
                        else shiftedASCIIInteger := ord(c) + shiftConstant;
                        
                        encryptedString := encryptedString + chr(shiftedASCIIInteger);
                      end
                else
                    begin
                        upperCaseString := upperCaseString + c; // Append character to end of string
                        encryptedString := encryptedString + c
                    end;
            end;
        exit(encryptedString);
    end;
    
    function decrypt(inputString: string; shiftConstant: integer; verbose: boolean): string;
    var
        upperCaseString: string;
        decryptedString: string;
        shiftedASCIIInteger: Integer;
        c: char;
    begin
    if verbose then
        writeln('Decrypting...');
    upperCaseString := uppercaseAndValidateString(inputString);
    decryptedString := '';
    // Mod the shift amount
    shiftConstant := shiftConstant mod 26;
       for c in upperCaseString do
            begin
                // skip if char is a space
                if (not (c = ' ') and not (c = '.')) then
                      begin
                        // Shift the letters left relative to the shiftConstant
                        if ((ord(c) - shiftConstant) < 65) then
                            begin
                                shiftedASCIIInteger := 91 - (65 - ((ord(c) - shiftConstant) mod 65));
                            end
                        else shiftedASCIIInteger := ord(c) - shiftConstant;
                        
                        decryptedString := decryptedString + chr(shiftedASCIIInteger);
                      end
                else
                    begin
                        upperCaseString := upperCaseString + c; // Append character to end of string
                        decryptedString := decryptedString + c
                    end;
            end;
        exit(decryptedString);
    end;
    
    function solve(inputString: string; maxShiftValue: integer): string;
        var
            shiftValue: Integer;
        begin
        writeln('Solving...');
           for shiftValue := 0 to maxShiftValue do
                begin
                    writeln('Caesar ' + IntToStr(shiftValue) + ': ' + decrypt(inputString, shiftValue, false))
                end;
        exit();
        end;
    
    var
     x: string;
     y: string;
    
    begin
        x := encrypt('E.T. phone home', 8);
        writeln(x);
        y := decrypt(x, 8, true);
        writeln(y);
        x := encrypt('mY HoUsE Is On fIrez', 8);
        writeln(x);
        y := decrypt(x, 8, true);
        writeln(y);
        solve('hal', 26)
    end.







