import java.util.Hashtable;
import java.util.Map;

import org.jpl7.*;
import java.lang.System;


public class Main 
{
	public static void main(String args[]) 
	{
		System.out.println("Wumpus World");
		Query q1 = new Query("consult", new Term[] {new Atom("C:\\Users\\Lucas\\workspace\\WumpusWorldInterface\\mundowumpus.pl")});
		System.out.println("consult " + (q1.hasSolution() ? "succeeded" : "failed"));
		Query q2 = new Query("iniciar_mundoteste");
		q2.hasSolution();
		Query q3 = new Query("tamanho_mundo(X)");
		q3.hasSolution();
		Map<String, Term>[] solution = q3.allSolutions();
		if (solution != null)
		{
		for (int i = 0; i < solution.length; i++)
		System.out.println("X = " + solution[i].get("X"));
		}

//		System.out.println(q3.isOpen());
		

	}
}