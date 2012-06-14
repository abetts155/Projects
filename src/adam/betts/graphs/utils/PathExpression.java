package adam.betts.graphs.utils;

public class PathExpression
{

    public final static char alernateOperator = '|';
    public final static char zeroOrMoreOperator = '*';
    public final static char oneOrMoreOperator = '+';
    public final static char concatenationOperator = '.';
    public final static char openParenthesis = '(';
    public final static char closeParenthesis = ')';
    public final static char nullExpression = '#';

    protected StringBuffer expression = new StringBuffer();

    public PathExpression ()
    {
    }

    public void concatenate (int vertexID)
    {
        expression.append(vertexID);
    }

    public void concatenate (PathExpression pathe)
    {
        expression.append(pathe.expression);
    }

    public void union (PathExpression left, PathExpression right)
    {
        expression.append(openParenthesis + left.toString() + alernateOperator
                + right.toString() + closeParenthesis);
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
