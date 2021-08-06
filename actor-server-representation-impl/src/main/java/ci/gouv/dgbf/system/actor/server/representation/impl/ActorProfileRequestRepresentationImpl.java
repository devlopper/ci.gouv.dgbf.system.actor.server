package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorProfileRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileRequestDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActorProfileRequestRepresentationImpl extends AbstractRepresentationEntityImpl<ActorProfileRequestDto> implements ActorProfileRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
