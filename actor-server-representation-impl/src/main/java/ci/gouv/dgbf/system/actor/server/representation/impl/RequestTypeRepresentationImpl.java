package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestTypeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RequestTypeRepresentationImpl extends AbstractRepresentationEntityImpl<RequestTypeDto> implements RequestTypeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
