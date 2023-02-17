object Cipher {
	def main(args: Array[String]): Unit = {
	  var encryptedString = ""
	  var decryptedString = ""
	  val inputs = List("E.T. phone home", "mY HoUsE Is On fIrez")
	  for (input <- inputs) {
	    encryptedString = encrypt(input, 8)
	    println("-- Encrypting --")
  	  println(encryptedString)
  	  decryptedString = decrypt(encryptedString, 8)
  	  println("-- Decrypting --")
  	  println(decryptedString)
  	  println(" ")
	  }
	  println("-- Solving --")
	  solve("hal", 26)
	}
	
	def encrypt(input: String, shiftValue: Int): String={
	  var encryptedString = ""
	  // Capitalize all characters in the input string
	  var capitalizedInput = input.toUpperCase()
	  for (c <- capitalizedInput) {
	    var charInt = c.toInt
	    if (charInt == 32 | charInt == 46) {
	      // Concatenate the period or space character to the string
	      encryptedString = encryptedString + c
	    }
	    else {
  	    var shiftedCharInt = charInt + shiftValue
  	    // Check if we need to wrap around
  	    if (shiftedCharInt >= 65 & shiftedCharInt <= 90) {
  	      encryptedString = encryptedString + shiftedCharInt.toChar
  	    }
  	    else {
  	      encryptedString = encryptedString + ((shiftedCharInt % 91) + 65).toChar
  	    }
	    }
	  }
	  return(encryptedString)
	}
	
	def decrypt(input: String, shiftValue: Int): String={
	  var decryptedString = ""
	  // Capitalize all characters in the input string
	  var capitalizedInput = input.toUpperCase()
	  for (c <- capitalizedInput) {
	    var charInt = c.toInt
	    if (charInt == 32 | charInt == 46) {
	      // Concatenate the period or space character to the string
	      decryptedString = decryptedString + c
	    }
	    else {
  	    var shiftedCharInt = charInt - shiftValue
  	    // Check if we need to wrap around
  	    if (shiftedCharInt >= 65 & shiftedCharInt <= 90) {
  	      decryptedString = decryptedString + shiftedCharInt.toChar
  	    }
  	    else {
  	      decryptedString = decryptedString + (91-(65-(shiftedCharInt % 65))).toChar
  	    }
	    }
	  }
	  return(decryptedString)
	}
	
	def solve(input: String, maxShiftValue: Int) {
	  var decryptedString = ""
	  var shiftValue = 0
	  for (shiftValue <- 0 to maxShiftValue) {
	    decryptedString = decrypt(input, shiftValue)
	    println(decryptedString)
	  }
	}
}