/*
 * MultiPopulationBasedAlgorithm.java
 * 
 * Created on Feb 10, 2006
 *
 * Copyright (C) 2003 - 2006 
 * Computational Intelligence Research Group (CIRG@UP)
 * Department of Computer Science 
 * University of Pretoria
 * South Africa
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */
package net.sourceforge.cilib.algorithm.population;

import java.util.ArrayList;
import java.util.List;

import net.sourceforge.cilib.problem.OptimisationProblem;

/**
 * 
 * @author Gary Pampara
 *
 */
public abstract class MultiPopulationBasedAlgorithm extends PopulationBasedAlgorithm {
	
	protected List<PopulationBasedAlgorithm> populationBasedAlgorithms;
	
	public MultiPopulationBasedAlgorithm() {
		this.populationBasedAlgorithms = new ArrayList<PopulationBasedAlgorithm>();
	}
	

	/**
	 * 
	 */
	public abstract void performIteration();


	public List<PopulationBasedAlgorithm> getPopulations() {
		return populationBasedAlgorithms;
	}


	public void setPopulations(List<PopulationBasedAlgorithm> populationBasedAlgorithms) {
		this.populationBasedAlgorithms = populationBasedAlgorithms;
	}
	
	public void addPopulationBasedAlgorithm(PopulationBasedAlgorithm algorithm) {
		this.populationBasedAlgorithms.add(algorithm);
	}
	
	public void removePopulationBasedalgorithm(PopulationBasedAlgorithm algorithm) {
		this.populationBasedAlgorithms.remove(algorithm);
	}
	
	public OptimisationProblem getOptimisationProblem() {
		return this.optimisationProblem;
	}
	
	public void setOptimisationProblem(OptimisationProblem problem) {
		this.optimisationProblem = problem;
	}

}