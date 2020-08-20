import java.io.*;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import java.util.Arrays;
import java.nio.ByteBuffer;

public class BeastAttack_c
{

  public static void main(String[] args) throws Exception
  {
    byte[] ciphertext=new byte[1024]; // will be plenty big enough

    long lastT = System.currentTimeMillis(); // time of last encryption
    byte[] iv = new byte[0]; // current IV
    callEncrypt(null, 0, ciphertext); // encrypt once to have something to base IV guesses off of
    byte[] lastC = Arrays.copyOfRange(ciphertext, 0, 8); // IV of last encryption
    byte[] message = new byte[8]; // first message block

    for (int i=0; i<8; i++){
      //while(true){
        long curT = System.currentTimeMillis(); // get current time
        //iv = predictIV(lastT, lastC, curT); // predict IV based on time since
        byte[] prefix = new byte[8];
        callEncrypt(prefix, 7-i, ciphertext); // send prefix of first 7 bits of guessed IV
        lastC = Arrays.copyOfRange(ciphertext, 0, 8); // save IV and time of encryption to be used in next guess
        lastT = curT;
        iv = lastC;

        System.out.println("guessed right: ");

        // Goal
        byte[] cBlock = Arrays.copyOfRange(ciphertext, 8, 16); // second block of ciphertext = prefix xor'd with IV
        byte[] newPrefix = new byte[8];

        for (int b=0; b<256; b++){ // try each possible byte
          while(true){ // keep guessing IV until we guess correctly
            byte tryB = (byte) b;
            curT = System.currentTimeMillis();
            byte[] iv2 = predictIV(lastT, lastC, curT); // guess new IV
            newPrefix = new byte[8];
            for (int a=0; a<i; a++){
              newPrefix[6-a] = message[7-a]; //put every recovered byte in new prefix
            }
            newPrefix[7] = tryB; // last byte of prefix is the byte we're guessing
            for (int a=0; a<8; a++){
              newPrefix[a] = (byte)(newPrefix[a] ^ iv2[a] ^ iv[a]); //xor every byte with original and new IV's
            }

            callEncrypt(newPrefix, 8, ciphertext);
            lastC = Arrays.copyOfRange(ciphertext, 0, 8); // save details of encryption for next guess of IV
            lastT = curT;
            if(bytesToLong(iv2) != bytesToLong(lastC)){ // if we didn't guess the IV correctly, try again
            continue;
          } // else we guessed the IV correctly
          byte[] guessCBlock = Arrays.copyOfRange(ciphertext, 8, 16);
          if (bytesToLong(guessCBlock) == bytesToLong(cBlock)){ // guessed the byte correctly
            for (int a=0; a<8; a++){
              newPrefix[a] = (byte)(newPrefix[a] ^ iv2[a]); // recover prefix - xor with itself (0s in first 7-i bytes)
            }
            for (int j=0; j<8; j++){
              newPrefix[7-j] = (byte)(newPrefix[7-j] ^ iv[7-j]); // xor last i+1 bytes of prefix with the original IV to recover message bytes
            }
            for (int j=7; j>0; j--){
              message[j-1] = message[j];
              // shift decrypted bytes to the left
            }
            message[7] = newPrefix[7]; //append decrypted byte onto message
            System.out.println(new String(newPrefix, "US-ASCII"));
            System.out.println();
          }
          break;
        }
      }
      //break;
    //}
  }
}



  public static byte[] longToBytes(long x) {
    ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES);
    buffer.putLong(x);
    return buffer.array();
  }

  public static long bytesToLong(byte[] bytes) {
    ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES);
    buffer.put(bytes);
    buffer.flip();//need flip
    return buffer.getLong();
  }

  static byte[] predictIV(long lastT, byte[] lastC, long curT){
    Long calc = 4*(curT - lastT) + bytesToLong(lastC);
    // Pattern spotted: (newIV - oldIV)/(newTime - oldTime) ≈ 4
    byte[] iv = longToBytes(calc);
    return iv;
  }



  // a helper method to call the external programme "encrypt" in the current directory
  // the parameters are the plaintext, length of plaintext, and ciphertext; returns length of ciphertext
  static int callEncrypt(byte[] prefix, int prefix_len, byte[] ciphertext) throws IOException
  {
    HexBinaryAdapter adapter = new HexBinaryAdapter();
    Process process;

    // run the external process (don't bother to catch exceptions)
    if(prefix != null)
    {
      // turn prefix byte array into hex string
      byte[] p=Arrays.copyOfRange(prefix, 0, prefix_len);
      String PString=adapter.marshal(p);
      process = Runtime.getRuntime().exec("./encrypt "+PString);
    }
    else
    {
      process = Runtime.getRuntime().exec("./encrypt");
    }

    // process the resulting hex string
    String CString = (new BufferedReader(new InputStreamReader(process.getInputStream()))).readLine();
    byte[] c=adapter.unmarshal(CString);
    System.arraycopy(c, 0, ciphertext, 0, c.length);
    return(c.length);
  }
}
