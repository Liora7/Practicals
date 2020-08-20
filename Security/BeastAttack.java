import java.io.*;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import java.util.Arrays;
import java.nio.ByteBuffer;

public class BeastAttack
{

  public static void main(String[] args) throws Exception
  {
    byte[] ciphertext=new byte[1024]; // will be plenty big enough

    long lastT = System.currentTimeMillis(); // time of last encryption
    byte[] iv = new byte[0]; // current IV
    callEncrypt(null, 0, ciphertext); // encrypt once to have something to base IV guesses off of
    byte[] lastC = Arrays.copyOfRange(ciphertext, 0, 8); // IV of last encryption



    while(true){
      long curT = System.currentTimeMillis(); // get current time
      iv = predictIV(lastT, lastC, curT); // predict IV based on time since last IV
      callEncrypt(iv, 7, ciphertext); // send prefix of first 7 bits of guessed IV
      lastC = Arrays.copyOfRange(ciphertext, 0, 8); // save IV and time of encryption to be used in next guess
      lastT = curT;

      // If the guessed IV is correct
      if (bytesToLong(curC) == bytesToLong(iv)){
        byte lastByte = iv[7]; //remember last byte of IV
        System.out.println("guessed right: ");

        // Goal
        byte[] cBlock = Arrays.copyOfRange(ciphertext, 8, 16); // second block of ciphertext = prefix xor'd with IV
        byte[] newPrefix = new byte[8];

        for (int b=0; b<256; b++){ // try each possible byte
          while(true){ // keep guessing IV until we guess correctly
            byte tryB = (byte) b;
            curT = System.currentTimeMillis();
            byte[] iv2 = predictIV(lastT, lastC, curT); // guess new IV
            for (int a=0; a<8; a++){
              newPrefix[a] = iv2[a];
            }
            newPrefix[7] = (byte)(newPrefix[7] ^ tryB); // last byte of prefix is the last byte of the new IV xor'd with the byte we're guessing
            callEncrypt(newPrefix, 8, ciphertext);
            lastC = Arrays.copyOfRange(ciphertext, 0, 8); // save details of encryption for next guess of IV
            lastT = curT;
            if(bytesToLong(iv2) != bytesToLong(curC)){ // if we didn't guess the IV correctly, try again
              continue;
            } // else we guessed the IV correctly
            byte[] guessCBlock = Arrays.copyOfRange(ciphertext, 8, 16);
            if (bytesToLong(guessCBlock) == bytesToLong(cBlock)){ // guessed the byte correctly
              for (int a=0; a<8; a++){
                newPrefix[a] = (byte)(newPrefix[a] ^ iv2[a]); // recover prefix - xor with itself (0s in first 7 bytes)
              }
              newPrefix[7] = (byte)(newPrefix[7] ^ iv[7]); // xor last byte of prefix with the original IV to recover message byte
              System.out.println(new String(newPrefix, "US-ASCII"));
              for (int a=0; a<8; a++){
                System.out.print(String.format("%02x ", guessCBlock[a]));
              }
              System.out.println();
            }
            break;
          }
        }
      }
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
