package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActorRepresentationImpl extends AbstractRepresentationEntityImpl<ActorDto> implements ActorRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
