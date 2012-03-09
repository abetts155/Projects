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
	
	public static void printMessage(String message) {
		System.out.println(message);
	}
	
	public static void errorMessage(String message) {
		System.err.println(message);
	}
	
	public static void exitWithError(String message) {
		System.out.println(message);
		System.exit(1);
	}
	
}
