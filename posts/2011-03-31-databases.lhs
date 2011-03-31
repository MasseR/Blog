---
title:Databases
tags:haskell,database,school
---

The database used in this post can be fetched from
[here](/static/employee.sql).

I recently had an school assignment to write a java method to list all
employees with their projects. The example output was given as:

~~~~~~~~
John Smith: ProductX, ProductY
Franklin Wong: ProductY, ProductZ, Computerization, Reorganization
Alicia Zelaya: Newbenefits, Computerization
etc..
~~~~~~~~

The solution could be written with one query, but I found it better to
have two queries, and handle the combination logic on application side.
The behaviour would be something along the like of: "For every
employee, find their projects and concatenate them. Then format every
employee and their users such that.."

~~~~~{.sourceCode .java}
public static void assignment2(Connection conn) throws Exception
{
    Statement st = conn.createStatement();
    PreparedStatement pst = conn.prepareStatement("SELECT P.Pname FROM Works_on AS WO JOIN Project AS P ON WO.Pno=P.Pnumber WHERE WO.Essn=?");
    // For every employee
    ResultSet eret = st.executeQuery("SELECT fname, lname, ssn FROM Employee");
    while(eret.next())
    {
        // Find their projects
        pst.setString(1, eret.getString("ssn"));
        ResultSet pret = pst.executeQuery();
        // And concatenate them into a list
        ArrayList<String> projects = new ArrayList<String>();
        while(pret.next())
    	projects.add(pret.getString("pname"));
        System.out.println(format(
    		eret.getString("fname"),
    		eret.getString("lname"),
    		projects));
    }
}

public static String format(String fname, String lname, List<String> projects)
{
    StringBuffer fmt = new StringBuffer("");
    fmt.append(fname).append(" ").append(lname).append(":");
    // Intercalate with ',' and reduce into a string
    for(String project : projects)
        fmt.append(" ").append(project).append(",");
    fmt.deleteCharAt(fmt.length()-1);
    return fmt.toString();
}
~~~~~~~~

This just screams for a functional solution, and I wanted to see how it would
turn out with [HDBC](http://hackage.haskell.org/package/HDBC). This is a copy
of my first attempt.

I'm using SQLite so we need to import HDBC and
[HDBC-sqlite3](http://hackage.haskell.org/package/HDBC-sqlite3)

> import Database.HDBC
> import Database.HDBC.Sqlite3

I'm playing clairvoyant and I see that a situation arises where `forM_` is
better looking than `mapM_`, so let's import it too. We also need
`intercalate` for inserting commas between projects.

> import Control.Monad (forM_)
> import Data.List (intercalate)

Then comes our assignment. Both the java and haskell versions have
exactly the same logic. Find all employees, map through them and find
their projects. Intercalate commas between them, concatenate them and
print them out.

> assignment2 conn = do
>   st <- handleSqlError $ prepare conn "SELECT P.Pname FROM Works_on AS WO JOIN Project AS P ON WO.Pno=P.Pnumber WHERE WO.Essn=?"
>   employees <- handleSqlError $ quickQuery conn "SELECT fname, lname, ssn FROM Employee" []
>   forM_ employees $ \[fname, lname, ssn] -> do
>     handleSqlError $ execute st [ssn]
>     rawprojects <- (handleSqlError $ fetchAllRows st)
>     let projects = intercalate ", " $ map fromSql $ concat rawprojects
>     putStrLn $ fromSql fname ++ " " ++ fromSql lname ++ ": " ++ projects
