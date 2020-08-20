object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] =
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] = {
    val n = plain.size
    val cipher = new Array[Char](n)
    val K = key.length
    var i = 0
    while(i<n){
      val ki = i % K
      cipher(i) = xor(plain(i), key(ki))
      i += 1
    }
    cipher
  }

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]): Unit ={
    val clen = crib.size; val tlen = ciphertext.size
    var start = 0; var j=tlen; var keyChars = new Array[Char](clen)
    while(start<tlen-clen){
      keyChars = encrypt(crib, ciphertext.slice(start, start+clen))
      if (recur(keyChars)<keyChars.size) {
        j = recur(keyChars)
        val i = start%j
        var key = keyChars.slice(0, j)
        key = key.slice(j-i, j) ++ key.slice(0, j-i)
        println(key.deep.mkString(""))
        println(encrypt(key, ciphertext).deep.mkString(""))
        return
      }
      start += 1
    }
    }

  def recur(s: Array[Char]): Int = {
    var n = 1; var len = s.size
    while (n<len){
      var i=0; var matched = true
      while (i<len-n){
        matched = matched && (s(i)==s(i+n))
        i += 1
      }
      if (matched) return n
      n += 1
    }
    n
  }


  /** The first optional statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) = {
    var shift = 1; val ctlen = ciphertext.size
    while(shift<32){
      var count = 0
      var i = 0
      while(i<ctlen){
        if (ciphertext(i)==ciphertext((shift+i)%ctlen)) count += 1
        i += 1
      }
      println(shift + ": " + count)
      shift +=1
    }
  }

  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) = {
    var s = 0; val ctlen = ciphertext.size
    while(s<ctlen){
      var i = 0
      while(i<ctlen){
        if (ciphertext(i)==ciphertext((s+i)%ctlen)) {
          val index = i%klen
          val letter = xor(ciphertext(i), ' ')
          if (32<=letter && letter<=127) println(index + " " + letter)
        }
        i += 1
      }
      s += klen
    }
  }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString =
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) =
      if(args.length==pos+1) readFile(args(pos)) else readStdin

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}
