package adam.betts.graphs.utils;

public class PathExpression
{

    public final static String alernateOperator = "|";
    public final static String zeroOrMoreOperator = "*";
    public final static String oneOrMoreOperator = "+";
    public final static String concatenationOperator = ".";
    public final static String openParenthesis = " (";
    public final static String closeParenthesis = ") ";
    public final static String nullExpression = "@";

    protected StringBuffer expression = new StringBuffer();

    public PathExpression ()
    {
    }

    public static PathExpression copy (PathExpression pathe)
    {
        PathExpression copye = new PathExpression();
        for (int i = 0; i < pathe.expression.length(); ++i)
        {
            copye.expression.append(pathe.expression.charAt(i));
        }
        return copye;
    }

    public void append (String str)
    {
        expression.append(str);
    }

    public void append (PathExpression pathe)
    {
        expression.append(pathe.expression);
    }

    public void removeLastElement ()
    {
        expression.deleteCharAt(expression.length() - 1);
    }

    public boolean isEmpty ()
    {
        return expression.length() == 0;
    }

    public String toString ()
    {
        return expression.toString();
    }
}
