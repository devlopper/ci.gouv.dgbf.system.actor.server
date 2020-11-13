package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.representation.api.ExecutionImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;

@ApplicationScoped
public class ExecutionImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ExecutionImputationDto> implements ExecutionImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

}