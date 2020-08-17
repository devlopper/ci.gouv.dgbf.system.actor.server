package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ImputationDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ImputationDto> implements ImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
