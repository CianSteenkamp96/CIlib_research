/*
 * Copyright (C) 2003 - 2008
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
 */
package net.sourceforge.cilib.type;

import net.sourceforge.cilib.type.types.Type;
import net.sourceforge.cilib.type.types.container.Vector;

/**
 * Class to perform the needed mappings between a top level domain string
 * and the built representation.
 * 
 * @author Gary Pampara
 *
 */
public class StringBasedDomainRegistry implements DomainRegistry {
	
	/**
	 * Generated <u>Serial Version UID</u> for the serialization.
	 */
	private static final long serialVersionUID = 3821361290684036030L;
	private String domainString;
	private String expandedRepresentation;
	private Type builtRepresenation;
	

	/**
	 * Construct an instacne of the DomainRegistry that will contioan the needed 
	 * information about the domain. 
	 */
	public StringBasedDomainRegistry() {
	}
	
	
	public StringBasedDomainRegistry(StringBasedDomainRegistry copy) {
		this.domainString = copy.domainString;
		this.expandedRepresentation = copy.expandedRepresentation;
		this.builtRepresenation = copy.builtRepresenation.getClone();
	}
	
	public StringBasedDomainRegistry getClone() {
		return new StringBasedDomainRegistry(this);
	}
	

	/* (non-Javadoc)
	 * @see net.sourceforge.cilib.type.DomainRegistryNew#getDomainString()
	 */
	public String getDomainString() {
		return domainString;
	}

	/**
	 * Set the value of the string representing the domain.
	 * @param domainString The domainString to set.
	 */
	public void setDomainString(String domainString) {
		this.domainString = domainString;
		
		DomainParser parser = DomainParser.getInstance();
		parser.parse(domainString);

		setExpandedRepresentation(parser.expandDomainString(domainString));
		setBuiltRepresenation(parser.getBuiltRepresentation());
	}
	
	
	/* (non-Javadoc)
	 * @see net.sourceforge.cilib.type.DomainRegistryNew#getExpandedRepresentation()
	 */
	public String getExpandedRepresentation() {
		return expandedRepresentation;
	}

	/**
	 * Set the value of the expaded domain string.
	 * @param expandedRepresentation The expandedRepresentation to set.
	 */
	public void setExpandedRepresentation(String expandedRepresentation) {
		this.expandedRepresentation = expandedRepresentation;
	}

	
	/* (non-Javadoc)
	 * @see net.sourceforge.cilib.type.DomainRegistryNew#getBuiltRepresenation()
	 */
	public Type getBuiltRepresenation() {
		return this.builtRepresenation;
	}

	/**
	 * Set the representation for this domain string. This may cause an inconsistency
	 * as the built represenation and the domain string may differ, depending on the
	 * values of the objects.
	 * @param builtRepresenation The builtRepresenation to set.
	 */
	public void setBuiltRepresenation(Type builtRepresenation) {
		this.builtRepresenation = builtRepresenation;
	}


	/* (non-Javadoc)
	 * @see net.sourceforge.cilib.type.DomainRegistryNew#getDimension()
	 */
	public int getDimension() {
		return ((Vector) this.builtRepresenation).getDimension();
	}
	
}

