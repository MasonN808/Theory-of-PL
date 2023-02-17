Declare Function encrypt(inputString As String, shiftValue As Integer) As String
Declare Function decrypt(inputString As String, shiftValue As Integer) As String
Declare Sub solve(inputString As String, maxShiftValue As Integer)
    ' From https://www.freebasic.net/forum/viewtopic.php?t=18590
    ' A Macro for a for each loop
    #Macro Each(__iter__, __arr__)
    	__index As Integer = LBound(__arr__) To UBound(__arr__)
    	#Define __iter__ (__arr__(__index)) 
    #EndMacro
    #Define In ,
    Dim encryptedStr As String = ""
    Dim decryptedStr As String = ""
    Dim inputStrings(...) As String = {"E.T. phone home", "mY HoUsE Is On fIrez"}
    For Each(inputString in inputStrings)
        Print !"-- Encrypting ----"
        encryptedStr = encrypt(inputString, 8)
        Print encryptedStr; !"\n";
        Print !"-- Decrypting ----"
        decryptedStr = decrypt(encryptedStr, 8)
        Print decryptedStr; !"\n";
    Next
    Print !"-- Solving ----"
    solve("hal", 26)
    Sleep
End

Function encrypt(inputString As String, shiftValue As Integer) As String
    ' Below from https://documentation.help/FreeBASIC/KeyPgFornext.html
    Dim As Integer i, j, k, ascValue, shiftedAscValue
    Dim As String encryptedString, charValue
    charValue = ""
    encryptedString = ""
    j = Len(inputString): k = 1
    ' Uppercase all the letters
    inputString = UCase(inputString)
    ' Loop through each character in the input string
    For i = 1 To j Step k
        charValue = Mid(inputString, i, 1)
        ascValue = Asc(charValue)
        If (ascValue = 32 Or ascValue = 46) Then
            ' Concatenate the space or period character to the string
            encryptedString = encryptedString + charValue
        Else
            ' Shift the ASCII value to the right for encryption
            shiftedAscValue = ascValue + shiftValue
            If (shiftedAscValue >= 65 And shiftedAscValue <= 90) Then
                encryptedString = encryptedString + Chr(shiftedAscValue)
            Else
                encryptedString = encryptedString + Chr((shiftedAscValue Mod 91) + 65)
            End If
        End If
    Next
    Return encryptedString
End Function

Function decrypt(inputString As String, shiftValue As Integer) As String
    ' Below from https://documentation.help/FreeBASIC/KeyPgFornext.html
    Dim As Integer i, j, k, ascValue, shiftedAscValue
    Dim As String decryptedString, charValue
    charValue = ""
    decryptedString = ""
    j = Len(inputString): k = 1
    ' Uppercase all the letters
    inputString = UCase(inputString)
    ' Loop through each character in the input string
    For i = 1 To j Step k
        charValue = Mid(inputString, i, 1)
        ascValue = Asc(charValue)
        If (ascValue = 32 Or ascValue = 46) Then
            ' Concatenate the space or period character to the string
            decryptedString = decryptedString + charValue
        Else
            ' Shift the ASCII value to the left for decryption
            shiftedAscValue = ascValue - shiftValue
            If (shiftedAscValue >= 65 And shiftedAscValue <= 90) Then
                decryptedString = decryptedString + Chr(shiftedAscValue)
            Else
                decryptedString = decryptedString + Chr(91-(65-(shiftedAscValue Mod 65)))
            End If
        End If
    Next
    Return decryptedString
End Function

Sub solve(inputString As String, maxShiftValue As Integer)
    Dim As Integer i, j, k, shiftValue
    Dim As String decryptedString, charValue
    shiftValue = 0
    decryptedString = ""
    j = maxShiftValue: k = 1
    ' Uppercase all the letters
    inputString = UCase(inputString)
    ' Loop through each character in the input string
    For i = 0 To j Step k
        decryptedString = decrypt(inputString, i)
        Print decryptedString
    Next
    Return
End Sub
