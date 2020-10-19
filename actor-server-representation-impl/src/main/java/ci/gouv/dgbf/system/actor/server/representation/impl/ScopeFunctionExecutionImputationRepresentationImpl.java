package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ScopeFunctionExecutionImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionExecutionImputationDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ScopeFunctionExecutionImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeFunctionExecutionImputationDto> implements ScopeFunctionExecutionImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
