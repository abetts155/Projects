package adam.betts.utilities;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import adam.betts.tools.MainWCETAnalyser;
import adam.betts.utilities.Enums.IProfile;

public class Globals
{

    private static String traceFileName = null;
    private static String outFileName = null;
    private static String uDrawGraphDirectory = null;
    private static ArrayList <String> roots = new ArrayList <String>();
    private static ArrayList <Enums.IProfile> iprofiles = new ArrayList <IProfile>();

    public final static void setTraceFileName (String value)
    {
        Debug.debugMessage(Globals.class, "Trace file is " + value, 1);
        traceFileName = value;
    }

    public final static String getTraceFileName ()
    {
        return traceFileName;
    }

    public final static void setOutputFileName (String value)
    {
        Debug.debugMessage(Globals.class, "Output file is " + value, 1);
        outFileName = value;
    }

    public final static String getOutputFileName ()
    {
        return outFileName;
    }

    public final static void setRoot (String value)
    {
        Debug.debugMessage(Globals.class, "'" + value + "' is the chosen root",
                1);
        roots.add(value);
    }

    public final static boolean hasRoot ()
    {
        return roots.size() > 0;
    }

    public final static String getRoot ()
    {
        assert roots.size() == 1;
        return roots.get(roots.size() - 1);
    }

    public final static List <String> getRoots ()
    {
        return Collections.unmodifiableList(roots);
    }

    public final static boolean uDrawDirectorySet ()
    {
        return uDrawGraphDirectory != null;
    }

    public final static void setUDrawDirectory (String value)
    {
        Debug.debugMessage(MainWCETAnalyser.class,
                "Setting uDraw directory to " + value, 1);
        uDrawGraphDirectory = value;
    }

    public final static String getUDrawDirectory ()
    {
        File dir = new File(uDrawGraphDirectory);
        if (!dir.exists())
        {
            dir.mkdir();
        }
        return uDrawGraphDirectory;
    }

    public final static void addInstrumentationProfile (IProfile value)
    {
        Debug.debugMessage(Globals.class, "Adding instrumentation profile "
                + value, 1);
        iprofiles.add(value);
    }

    public final static IProfile getInstrumentationProfile ()
    {
        assert iprofiles.size() == 1;
        return iprofiles.get(iprofiles.size() - 1);
    }

    public final static List <IProfile> getInstrumentationProfiles ()
    {
        return Collections.unmodifiableList(iprofiles);
    }
}
