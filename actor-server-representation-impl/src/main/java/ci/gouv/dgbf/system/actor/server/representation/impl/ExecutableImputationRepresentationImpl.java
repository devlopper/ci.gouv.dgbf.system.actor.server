package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ExecutableImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutableImputationDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ExecutableImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ExecutableImputationDto> implements ExecutableImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
