package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ExecutionImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ExecutionImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ExecutionImputationDto> implements ExecutionImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
