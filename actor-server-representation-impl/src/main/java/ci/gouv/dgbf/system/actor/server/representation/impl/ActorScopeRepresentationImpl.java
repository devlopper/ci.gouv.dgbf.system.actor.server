package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorScopeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActorScopeRepresentationImpl extends AbstractRepresentationEntityImpl<ActorScopeDto> implements ActorScopeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
