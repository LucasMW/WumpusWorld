import java.util.Hashtable;
import java.util.Map;

import org.jpl7.*;
import java.lang.System;


public class Main 
{
	public static void main(String args[]) 
	{
		System.out.println("Wumpus World");
		Query q1 = new Query("consult", new Term[] {new Atom("mundowumpus.pl")});
		System.out.println("consult " + (q1 != null ? "succeeded" : "failed"));
		Query q2 = new Query("iniciar_mundoteste");
		Query q3 = new Query("tamanho_mundo(X)");
//		Hashtable[] solution = (Hashtable[]) q2.allSolutions();
//		if (solution != null)
//		{
//		for (int i = 0; i < solution.length; i++)
//		System.out.println("X = " + solution[i].get("X"));
//		}

		System.out.println(q3.isOpen());
		

	}
}