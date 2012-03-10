package tvgen.util;

/**
 * Contains methods to handle the output from the program
 * @author chris
 *
 */
public class SystemOutput {

	public static boolean debugMode = false;
	
	public static void debugMessage(String message) {
		if(debugMode) {
			printMessage(message);
		}
	}
	
	public synchronized static void printMessage(String message) {
		System.out.println(message);
	}
	
	public synchronized static void errorMessage(String message) {
		System.err.println(message);
	}
	
	public static void exitWithError(String message) {
		printMessage(message);
		System.exit(1);
	}
	
}
