object CaesarCipher {
  
  def encrypt(text: String, shift: Int): String = {
    val normalizedShift = shift % 26 
    text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        val shiftedChar = (char - base + normalizedShift) % 26 + base
        shiftedChar.toChar
      } else {
        char 
      }
    }
  }



  def decrypt(text: String, shift: Int): String = {
    val normalizedShift = shift % 26 
    text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        val shiftedChar = (char - base - normalizedShift) % 26 + base
        shiftedChar.toChar
      } else {
        char 
      }
    }
  }



  def cipher(): Unit = {
    print("Do you want to encrypt or decrypt(encrypt=1  decrypt=2): \n")
    val num = scala.io.StdIn.readInt()
    
    print("Enter the plain text:")
    val plaintext = scala.io.StdIn.readLine()

    print("Enter the shift value:")
    val shift = scala.io.StdIn.readInt()

    if(num == 1){
        val ciphertext = encrypt(plaintext, shift)
        print(s"Encrypted ciphertext: $ciphertext")
    } 
    if(num == 2){
        val ciphertext = decrypt(plaintext, shift)
        print(s"Encrypted ciphertext: $ciphertext")
    }
  }

  def main(args: Array[String]): Unit = {
    cipher();
  }
}
