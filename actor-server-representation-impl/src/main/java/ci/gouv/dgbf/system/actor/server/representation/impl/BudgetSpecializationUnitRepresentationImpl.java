package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.BudgetSpecializationUnitRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.BudgetSpecializationUnitDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class BudgetSpecializationUnitRepresentationImpl extends AbstractRepresentationEntityImpl<BudgetSpecializationUnitDto> implements BudgetSpecializationUnitRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
