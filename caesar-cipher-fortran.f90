
program cipher
    character(len=:), allocatable :: testString
    character(len=:), allocatable :: encryptedMessage, decryptedMessage
    testString = "E.T. phone home"
    encryptedMessage = encrypt(testString, 8)
    print *, testString, " --> ", encryptedMessage
    decryptedMessage = decrypt(encryptedMessage, 8)
    print*,  "decrypting..."
    print *, encryptedMessage, " --> ", decryptedMessage
    testString = "mY HoUsE Is On fIrez"
    encryptedMessage = encrypt(testString, 8)
    print *, testString, " --> ", encryptedMessage
    decryptedMessage = decrypt(encryptedMessage, 8)
    print*,  "decrypting..."
    print *, encryptedMessage, " --> ", decryptedMessage
    testString = "hal"
    call solve(testString, 26)

contains
    function encrypt(inputString, shiftValue) result(encryptedString)
        character(len=*), intent (in) :: inputString ! input
        character(len=:), allocatable :: encryptedString, upperCasedString ! output
        integer :: i, ic, shiftValue
        upperCasedString = inputString
        encryptedString = inputString
        print*,  "encrypting..."
        do i=1,len(inputString)
            ! Check for spaces
! pulled from http://computer-programming-forum.com/49-fortran/4075a24f74fcc9ce.htm
            ic = ichar(upperCasedString(i:i)) 
            if (ic == 32 .or. ic==46) cycle
            if (ic >= 97 .and. ic <= 122) upperCasedString(i:i) = char(ic-32)
            ! Now encrypt the Char
            ic = ichar(upperCasedString(i:i)) + shiftValue
            if (ic >= 65 .and. ic <= 90) then
                encryptedString(i:i) = char(ic)
            else ! ASSUME it can't be less than 65
                encryptedString(i:i) = char(mod(ic, 91) + 65)
            end if
        end do
    end function
    
    function decrypt(inputString, shiftValue) result(decryptedString)
        character(len=*), intent (in) :: inputString ! input
        character(len=:), allocatable :: decryptedString, upperCasedString ! output
        integer :: i, ic, shiftValue
        upperCasedString = inputString
        decryptedString = inputString
        do i=1,len(inputString)
! pulled from http://computer-programming-forum.com/49-fortran/4075a24f74fcc9ce.htm
            ic = ichar(upperCasedString(i:i)) 
            if (ic == 32 .or. ic == 46) cycle
            if (ic >= 97 .and. ic <= 122) upperCasedString(i:i) = char(ic-32)
            ! Now encrypt the Char
            ic = ichar(upperCasedString(i:i)) - shiftValue
            if (ic >= 65 .and. ic <= 90) then
                decryptedString(i:i) = char(ic)
            else ! ASSUME it can't be greater than 90
                decryptedString(i:i) = char(91-(65-mod(ic, 65)))
            end if
        end do
    end function
    
    subroutine solve(inputString, maxShiftValue)
        character(len=*), intent (in) :: inputString ! input
        character(len=:), allocatable :: decryptedString
        integer :: i
        print*,  "solving..."
        do i=0, maxShiftValue
            decryptedString = decrypt(inputString, i)
            print *, testString, " --> ", decryptedString
        end do
    end subroutine

end program cipher





