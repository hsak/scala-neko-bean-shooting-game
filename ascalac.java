/**
 * ajc
 * Auto Java Compiler
 * usase:
 * java ajc hoge
 *  and write hoge.java
 * ajc auto compile hoge.java when you saved hoge.java.
 * 
 * todo: *.java file compile
 *  example: java *
 *  and write fuga.java
 *  compile fuga.java 
 *  write hoge.java
 *  compile hoge.java
 * todo: executable option
 *  example:
 *  java -e hello
 *  hello world
 * todo: any language compile
 *  java ajc -d -java -scala
 *  this example is that java & d & scala source code compile.0
 * copy right h_sakurai all right reserved.
 * license? it is freebsd like license.
 */
import java.io.*;
class ascalac {

	static private String srcName;
	public static void main(String[] args) throws Exception{
		File file = new File(srcName = args[0]);
		File file2 = new File(args[1]);
		long mtimeOld = 0;
		while(true) {
			long mtime = file.lastModified();
			long mtime2 = file2.lastModified();
			if(mtime > mtimeOld && mtime > mtime2) {
				System.out.println("return code="+launch());
			} else {
				Thread.sleep(1000);
			}
			mtimeOld = mtime;
		}

	}
	static int launch() {
		System.out.println("compile start");
		Runtime runtime = Runtime.getRuntime();
		try {
			Process process = runtime.exec("cmd.exe /c call gamem");
			final BufferedReader stdReader = new BufferedReader(
				new InputStreamReader(process.getInputStream())
			);
			Thread stdOutThread = new Thread(new Runnable() {
					public void run() {
						String line;
						try {
							while ((line = stdReader.readLine()) != null) {
								System.out.println(line);
							}
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
				});

			final BufferedReader errReader = new BufferedReader(
				new InputStreamReader(process.getErrorStream())
			);
			Thread stdErrThread = new Thread(new Runnable() {
					public void run() {
						String line;
						try {
							while ((line = errReader.readLine()) != null) {
								System.err.println(line);
							}
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
				});
			stdErrThread.start();
			stdOutThread.run();

			System.out.println("compile end");
			return process.exitValue();
		} catch (IOException e) {
			System.out.println(
				"Command cannot execute."
			);
			e.printStackTrace();
		}
		return -1;
	}
}
