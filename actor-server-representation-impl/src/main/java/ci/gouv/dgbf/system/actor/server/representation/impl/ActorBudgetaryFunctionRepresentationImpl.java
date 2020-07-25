package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorBudgetaryFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorBudgetaryFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActorBudgetaryFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ActorBudgetaryFunctionDto> implements ActorBudgetaryFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
