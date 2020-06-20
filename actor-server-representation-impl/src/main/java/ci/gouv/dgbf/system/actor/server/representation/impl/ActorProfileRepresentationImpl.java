package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorProfileRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActorProfileRepresentationImpl extends AbstractRepresentationEntityImpl<ActorProfileDto> implements ActorProfileRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
