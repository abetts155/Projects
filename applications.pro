#
# This ProGuard configuration file illustrates how to process applications.
# Usage:
#     java -jar proguard.jar @applications.pro
#

# Your application may contain more items that need to be preserved; 
# typically classes that are dynamically created using Class.forName:
-keep public class lpsolve.*** { *; }
-keep public interface lpsolve.AbortListener
-keep public interface lpsolve.BbListener
-keep public interface lpsolve.LogListener
-keep public interface lpsolve.MsgListener

# Specify the input jars, output jars, and library jars.

-injars bin/program-generator.jar
-outjars bin/program-generator-obfuscated.jar

-injars bin/ipe-analyser.jar
-outjars bin/ipe-analyser-obfuscated.jar

-libraryjars <java.home>/lib/rt.jar
-libraryjars lib/commons-cli-1.2.jar
#-libraryjars lib/lpsolve55j.jar
-libraryjars lib/junit-4.8.1.jar

# Save the obfuscation mapping to a file, so you can de-obfuscate any stack
# traces later on. Keep a fixed source file attribute and all line number
# tables to get line numbers in the stack traces.
# You can comment this out if you're not interested in stack traces.

-renamesourcefileattribute SourceFile
-keepattributes SourceFile,LineNumberTable

# Preserve all annotations.

-keepattributes *Annotation*

# You can print out the seeds that are matching the keep options below.

#-printseeds out.seeds

# Preserve all public applications.

-printmapping mapping.txt

-keepclasseswithmembers public class * {
    public static void main(java.lang.String[]);
}

# Preserve all native method names and the names of their classes.

-keepclasseswithmembernames class * {
    native <methods>;
}

# Preserve the special static methods that are required in all enumeration
# classes.

-keepclassmembers class * extends java.lang.Enum {
    public static **[] values();
    public static ** valueOf(java.lang.String);
}

# Explicitly preserve all serialization members. The Serializable interface
# is only a marker interface, so it wouldn't save them.
# You can comment this out if your application doesn't use serialization.
# If your code contains serializable classes that have to be backward 
# compatible, please refer to the manual.

-keepclassmembers class * implements java.io.Serializable {
    static final long serialVersionUID;
    static final java.io.ObjectStreamField[] serialPersistentFields;
    private void writeObject(java.io.ObjectOutputStream);
    private void readObject(java.io.ObjectInputStream);
    java.lang.Object writeReplace();
    java.lang.Object readResolve();
}

-dontshrink
-dontoptimize
-dontpreverify
-verbose


