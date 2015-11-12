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
		Query q2 = new Query("start");
		q2.hasSolution();
		showSolutions(q2,"Z");
		
		Query q3 = new Query("Z = iniciar_mundoteste");
		showSolutions(q3,"Z");
		
		Query q4 = new Query("agente_local([X,Y])");
		showSolutions(q4,"X");
		showSolutions(q4,"Y");
		Query q5 = new Query("tem_algo([3,1],Z)");
		showSolutions(q5,"Z");
		
		
		
		Query bestAction = new Query("melhor_acao(X)");
		bestAction.hasSolution();
		Map<String, Term>[] solution = bestAction.allSolutions();
		if (solution != null)
		{
			for (int i = 0; i < solution.length; i++)
			{
				System.out.println("X = " + solution[i].get("X"));
			}
		}
		

	System.out.println(bestAction.isOpen());
		
		
	}
	private static void printMap()
	{
		//ask size of map
		//consult every place of map assuming square matrix
		
	}
	
	private static void showSolutions(Query q, String whatToGet  )
	{
		q.hasSolution();
		Map<String, Term>[] solution = q.allSolutions();
		if (solution != null)
		{
			System.out.println("sd");
		for (int i = 0; i < solution.length; i++)
		System.out.println(whatToGet + " = " + solution[i].get(whatToGet));
		System.out.println(q.toString());
		}
	}
}