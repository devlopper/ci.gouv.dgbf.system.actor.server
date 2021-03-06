package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.BudgetaryFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.BudgetaryFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class BudgetaryFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<BudgetaryFunctionDto> implements BudgetaryFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
