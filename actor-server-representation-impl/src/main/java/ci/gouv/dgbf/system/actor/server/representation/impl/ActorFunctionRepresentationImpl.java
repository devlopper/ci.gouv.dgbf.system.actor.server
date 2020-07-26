package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActorFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ActorFunctionDto> implements ActorFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
